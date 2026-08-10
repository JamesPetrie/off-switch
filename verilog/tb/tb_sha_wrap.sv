// tb_sha_wrap.sv — directed sha2_wrap test vs the DPI SHA-256 reference.
//   V0 "abc" (FIPS KAT), V1 2-block FIPS vector, V2 "abc" again
//   (first/last re-arm), V3 3-block message with producer stalls.
// Also checks ready is a single-cycle pulse and digest is final in the
// last ready cycle.

module tb (
    input logic clk,
    input logic rst_n
);

    import "DPI-C" function void dpi_sha256(
        input byte unsigned data[128], input int byte_len,
        output bit [255:0] digest
    );

    // -------------------------------------------------------------------------
    // DUT
    // -------------------------------------------------------------------------

    logic         dut_valid = 1'b0;
    logic [511:0] dut_block = '0;
    logic         dut_last  = 1'b0;
    wire          dut_ready;
    wire  [255:0] dut_digest;

    sha2_wrap u_dut (
        .clk    (clk),
        .rst_n  (rst_n),
        .valid  (dut_valid),
        .block  (dut_block),
        .last   (dut_last),
        .ready  (dut_ready),
        .digest (dut_digest)
    );

    // -------------------------------------------------------------------------
    // Vectors: raw messages, padded blocks and expected digests
    // -------------------------------------------------------------------------

    localparam int NumVec    = 4;
    localparam int MaxBlocks = 3;
    localparam int MaxMsgLen = 128;

    localparam string S_ABC = "abc";
    localparam string S_2BL = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";

    // FIPS 180-4 known answers for V0/V1 (independent of the DPI model)
    localparam bit [255:0] KAT_ABC =
        256'hba7816bf_8f01cfea_414140de_5dae2223_b00361a3_96177a9c_b410ff61_f20015ad;
    localparam bit [255:0] KAT_2BL =
        256'h248d6a61_d20638b8_e5c02693_0c3e6039_a33ce459_64ff2167_f6ecedd4_19db06c1;

    byte unsigned msg_bytes [NumVec][MaxMsgLen];
    int           msg_len   [NumVec];
    int           msg_gap   [NumVec];   // idle cycles inserted between blocks

    typedef logic [511:0] blocks_t [MaxBlocks];

    blocks_t    blocks_all [NumVec];
    int         nblocks    [NumVec];
    bit [255:0] exp_digest [NumVec];

    // Standard SHA-256 padding of message `vec` into 512-bit blocks:
    // big-endian bytes, 0x80 terminator, 64-bit bit length in the last word.
    function automatic blocks_t pad_blocks(input int vec);
        blocks_t blocks;
        int      len, total;
        len   = msg_len[vec];
        total = ((len + 8) / 64) + 1;
        for (int b = 0; b < MaxBlocks; b++) blocks[b] = '0;
        for (int j = 0; j < len; j++) begin
            blocks[j/64][511 - 8*(j%64) -: 8] = msg_bytes[vec][j];
        end
        blocks[len/64][511 - 8*(len%64) -: 8] = 8'h80;
        blocks[total-1][63:0] = 64'(len) * 64'd8;
        return blocks;
    endfunction

    initial begin
        // V0/V2: "abc", V1: two-block FIPS vector
        for (int v = 0; v < NumVec; v++) begin
            for (int j = 0; j < MaxMsgLen; j++) msg_bytes[v][j] = 8'h00;
        end
        msg_len[0] = S_ABC.len(); msg_gap[0] = 0;
        msg_len[1] = S_2BL.len(); msg_gap[1] = 0;
        msg_len[2] = S_ABC.len(); msg_gap[2] = 0;
        msg_len[3] = 120;         msg_gap[3] = 3;
        for (int j = 0; j < S_ABC.len(); j++) begin
            msg_bytes[0][j] = byte'(S_ABC[j]);
            msg_bytes[2][j] = byte'(S_ABC[j]);
        end
        for (int j = 0; j < S_2BL.len(); j++) msg_bytes[1][j] = byte'(S_2BL[j]);
        for (int j = 0; j < 120; j++)         msg_bytes[3][j] = byte'(j*7 + 3);

        for (int v = 0; v < NumVec; v++) begin
            nblocks[v]    = ((msg_len[v] + 8) / 64) + 1;
            blocks_all[v] = pad_blocks(v);
            dpi_sha256(msg_bytes[v], msg_len[v], exp_digest[v]);
        end
    end

    // -------------------------------------------------------------------------
    // Test sequencer
    // -------------------------------------------------------------------------

    localparam int Timeout = 10_000;

    typedef enum logic [2:0] {
        PH_SETUP,
        PH_DRIVE,
        PH_WAITBLK,
        PH_GAP,
        PH_CHECK,
        PH_DONE
    } ph_e;

    ph_e          phase      = PH_SETUP;
    int           vec        = 0;
    int           blk        = 0;
    int           gap_cnt    = 0;
    int           wait_cnt   = 0;
    int           n_fail     = 0;
    logic [255:0] got_digest = '0;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase      <= PH_SETUP;
            dut_valid  <= 1'b0;
            dut_block  <= '0;
            dut_last   <= 1'b0;
            vec        <= 0;
            blk        <= 0;
            gap_cnt    <= 0;
            wait_cnt   <= 0;
            n_fail     <= 0;
            got_digest <= '0;
        end else begin
            case (phase)
                PH_SETUP: begin
                    if (vec == 0) $display("=== sha2_wrap unit test ===");
                    blk   <= 0;
                    phase <= PH_DRIVE;
                end

                PH_DRIVE: begin
                    dut_valid <= 1'b1;
                    dut_block <= blocks_all[vec][blk];
                    dut_last  <= (blk == nblocks[vec] - 1);
                    wait_cnt  <= 0;
                    phase     <= PH_WAITBLK;
                end

                PH_WAITBLK: begin
                    // Producer contract: hold valid/block/last until ready.
                    wait_cnt <= wait_cnt + 1;
                    if (dut_ready) begin
                        if (dut_last) begin
                            // Contract: digest is final in the ready cycle
                            got_digest <= dut_digest;
                            dut_valid  <= 1'b0;
                            dut_last   <= 1'b0;
                            phase      <= PH_CHECK;
                        end else begin
                            blk <= blk + 1;
                            if (msg_gap[vec] != 0) begin
                                dut_valid <= 1'b0;
                                gap_cnt   <= msg_gap[vec];
                                phase     <= PH_GAP;
                            end else begin
                                phase <= PH_DRIVE;
                            end
                        end
                    end else if (wait_cnt > Timeout) begin
                        $display("FAIL  [V%0d timeout] block %0d after %0d cycles",
                                 vec, blk, wait_cnt);
                        $fatal;
                    end
                end

                PH_GAP: begin
                    // Producer stall between blocks of one message
                    gap_cnt <= gap_cnt - 1;
                    if (gap_cnt <= 1) phase <= PH_DRIVE;
                end

                PH_CHECK: begin
                    if (got_digest !== exp_digest[vec]) begin
                        $display("FAIL  [V%0d digest] got %h expected %h",
                                 vec, got_digest, exp_digest[vec]);
                        n_fail <= n_fail + 1;
                    end else if ((vec == 0 || vec == 2) && got_digest !== KAT_ABC) begin
                        $display("FAIL  [V%0d KAT] got %h", vec, got_digest);
                        n_fail <= n_fail + 1;
                    end else if (vec == 1 && got_digest !== KAT_2BL) begin
                        $display("FAIL  [V%0d KAT] got %h", vec, got_digest);
                        n_fail <= n_fail + 1;
                    end else begin
                        if (msg_gap[vec] != 0) begin
                            $display("PASS  [V%0d %0d-block gapped] digest matches",
                                     vec, nblocks[vec]);
                        end else begin
                            $display("PASS  [V%0d %0d-block] digest matches",
                                     vec, nblocks[vec]);
                        end
                    end
                    vec   <= vec + 1;
                    phase <= (vec == NumVec - 1) ? PH_DONE : PH_SETUP;
                end

                PH_DONE: begin
                    if (n_fail == 0) $display("All %0d sha2_wrap vectors passed.", NumVec);
                    else             $display("%0d sha2_wrap vector(s) FAILED.", n_fail);
                    if (n_fail != 0) $fatal;
                    $finish;
                end

                default: phase <= PH_SETUP;
            endcase
        end
    end

    // ready must only pulse while a block is in flight, one cycle at a time
    always @(posedge clk) begin
        if (rst_n && dut_ready && phase != PH_WAITBLK) begin
            $display("FAIL  [spurious ready] phase=%0d", phase);
            $fatal;
        end
    end

endmodule
