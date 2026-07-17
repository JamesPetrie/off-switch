// FIPS 205 SHA2-128s variable-length T_l member function.
//
// T_l(PK.seed, ADRS, M) = Trunc_n(SHA-256(
//     PK.seed || zero_pad_to_64_bytes || ADRSc || M))
//
// M is supplied as 1..WOTS_LEN top-aligned n-byte elements. Buffering the
// elements keeps this first implementation simple and makes the SHA block
// construction explicit. The same engine covers FORS public-key compression
// (K=14 roots) and WOTS+ public-key compression (len=35 chain endpoints).

module slh_hash_tl
    import slh_dsa_pkg::*;
(
    input  logic                         clk,
    input  logic                         rst_n,

    input  logic                         start,
    input  logic [8*N_BYTES-1:0]         pk_seed,
    input  logic [175:0]                 adrs_c,
    input  logic [5:0]                   element_count,

    input  logic                         element_valid,
    input  logic [MAX_NODE_BITS-1:0]     element,
    output logic                         element_ready,

    output logic                         busy,
    output logic                         done,
    output logic                         error,
    output logic [MAX_NODE_BITS-1:0]     result
);

    localparam int unsigned PREFIX_BYTES = 64;
    localparam int unsigned ADRSC_BYTES = 22;
    localparam int unsigned SHA_BLOCK_BYTES = 64;
    localparam int unsigned MAX_ELEMENTS = WOTS_LEN;
    localparam int unsigned MAX_TOTAL_BYTES =
        PREFIX_BYTES + ADRSC_BYTES + MAX_ELEMENTS * N_BYTES;
    localparam int unsigned MAX_BLOCKS =
        (MAX_TOTAL_BYTES + 1 + 8 + SHA_BLOCK_BYTES - 1) / SHA_BLOCK_BYTES;
    localparam int unsigned BLOCK_INDEX_BITS = $clog2(MAX_BLOCKS);
    localparam logic [5:0] MAX_ELEMENT_COUNT =
        6'(MAX_ELEMENTS);

    typedef enum logic [1:0] {
        StIdle,
        StLoad,
        StHash
    } state_e;

    state_e                     state_q;
    logic [8*N_BYTES-1:0]       pk_seed_q;
    logic [175:0]               adrs_c_q;
    logic [5:0]                 element_count_q;
    logic [5:0]                 load_index_q;
    logic [BLOCK_INDEX_BITS-1:0] block_index_q;
    logic [8*N_BYTES-1:0]       elements_q [0:MAX_ELEMENTS-1];

    logic                       sha_valid;
    logic [511:0]               sha_block;
    logic                       sha_last;
    wire                        sha_ready;
    wire [255:0]                sha_digest;
    wire [127:0] unused_sha_digest_tail = sha_digest[127:0];
    wire element_aligned = (element[127:0] == '0);

    integer                     byte_index;
    integer                     global_index;
    integer                     total_bytes;
    integer                     total_blocks;
    logic [5:0]                 element_index;
    logic [3:0]                 element_byte;
    logic [7:0]                 selected_byte;
    logic [63:0]                total_length_bits;

    sha256_wrap u_sha256 (
        .clk    (clk),
        .rst_n  (rst_n),
        .valid  (sha_valid),
        .block  (sha_block),
        .last   (sha_last),
        .ready  (sha_ready),
        .digest (sha_digest)
    );

    assign busy          = (state_q != StIdle);
    assign element_ready = (state_q == StLoad);

    always_comb begin
        total_bytes       = PREFIX_BYTES + ADRSC_BYTES
                            + element_count_q * N_BYTES;
        total_blocks      = (total_bytes + 1 + 8 + SHA_BLOCK_BYTES - 1)
                            / SHA_BLOCK_BYTES;
        total_length_bits = 8 * total_bytes;
        element_index     = '0;
        element_byte      = '0;

        sha_valid = (state_q == StHash);
        sha_block = '0;
        sha_last  = (block_index_q
                     == BLOCK_INDEX_BITS'(total_blocks - 1));

        for (byte_index = 0; byte_index < SHA_BLOCK_BYTES;
             byte_index = byte_index + 1) begin
            global_index = block_index_q * SHA_BLOCK_BYTES + byte_index;
            selected_byte = 8'h00;

            if (global_index < N_BYTES) begin
                selected_byte = pk_seed_q[
                    8*N_BYTES - 1 - 8*global_index -: 8];
            end else if (global_index < PREFIX_BYTES) begin
                selected_byte = 8'h00;
            end else if (global_index < PREFIX_BYTES + ADRSC_BYTES) begin
                selected_byte = adrs_c_q[
                    8*ADRSC_BYTES - 1
                    - 8*(global_index-PREFIX_BYTES) -: 8];
            end else if (global_index < total_bytes) begin
                element_index = 6'(
                    (global_index-PREFIX_BYTES-ADRSC_BYTES) / N_BYTES);
                element_byte = 4'(
                    (global_index-PREFIX_BYTES-ADRSC_BYTES) % N_BYTES);
                selected_byte = elements_q[element_index][
                    8*N_BYTES - 1 - 8*element_byte -: 8];
            end else if (global_index == total_bytes) begin
                selected_byte = 8'h80;
            end else if ((block_index_q
                          == BLOCK_INDEX_BITS'(total_blocks - 1))
                         && (byte_index >= SHA_BLOCK_BYTES - 8)) begin
                selected_byte = total_length_bits[
                    63 - 8*(byte_index-(SHA_BLOCK_BYTES-8)) -: 8];
            end
            sha_block[511 - 8*byte_index -: 8] = selected_byte;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q         <= StIdle;
            pk_seed_q       <= '0;
            adrs_c_q        <= '0;
            element_count_q <= '0;
            load_index_q    <= '0;
            block_index_q   <= '0;
            done            <= 1'b0;
            error           <= 1'b0;
            result          <= '0;
        end else begin
            done  <= 1'b0;
            error <= 1'b0;

            unique case (state_q)
                StIdle: begin
                    if (start) begin
                        if ((element_count == 0)
                            || (element_count > MAX_ELEMENT_COUNT)) begin
                            done  <= 1'b1;
                            error <= 1'b1;
                        end else begin
                            pk_seed_q       <= pk_seed;
                            adrs_c_q        <= adrs_c;
                            element_count_q <= element_count;
                            load_index_q    <= '0;
                            state_q         <= StLoad;
                        end
                    end
                end

                StLoad: begin
                    if (element_valid) begin
                        if (!element_aligned) begin
                            done    <= 1'b1;
                            error   <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            elements_q[load_index_q] <=
                                element[MAX_NODE_BITS-1 -: 8*N_BYTES];
                            if (load_index_q == element_count_q - 1) begin
                                block_index_q <= '0;
                                state_q       <= StHash;
                            end else begin
                                load_index_q <= load_index_q + 1'b1;
                            end
                        end
                    end
                end

                StHash: begin
                    if (sha_ready) begin
                        if (sha_last) begin
                            result <= {
                                sha_digest[255 -: 8*N_BYTES],
                                {(MAX_N_BYTES-N_BYTES)*8{1'b0}}
                            };
                            done    <= 1'b1;
                            state_q <= StIdle;
                        end else begin
                            block_index_q <= block_index_q + 1'b1;
                        end
                    end
                end

                default: state_q <= StIdle;
            endcase
        end
    end

endmodule
