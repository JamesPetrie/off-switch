// Fixed-message H_msg for FIPS 205 SLH-DSA-SHA2-128s.
//
// The Off-Switch application message is 72 bytes and the Pure API context is
// empty. The internal message is therefore 0x00 || 0x00 || application_message.
// H_msg uses an inner SHA-256 followed by MGF1-SHA-256 with counter zero. Since
// m=30, one MGF1 digest supplies the complete output.

module slh_hmsg
    import slh_dsa_pkg::*;
#(
    parameter int unsigned APP_MESSAGE_BYTES = 72
) (
    input  logic                         clk,
    input  logic                         rst_n,
    input  logic                         start,
    input  logic [8*N_BYTES-1:0]         randomizer_r,
    input  logic [8*N_BYTES-1:0]         pk_seed,
    input  logic [8*N_BYTES-1:0]         pk_root,
    input  logic [8*APP_MESSAGE_BYTES-1:0] application_message,

    output logic                         busy,
    output logic                         done,
    output logic [255:0]                 digest,
    output logic [HMSG_FORS_BITS-1:0]    fors_message,
    output logic [HMSG_TREE_BYTES*8-1:0] tree_index,
    output logic [HMSG_LEAF_BYTES*8-1:0] leaf_index
);

    localparam int unsigned SHA_BLOCK_BITS = 512;
    localparam int unsigned PURE_PREFIX_BYTES = 2;
    localparam int unsigned INNER_TOTAL_BYTES =
        3 * N_BYTES + PURE_PREFIX_BYTES + APP_MESSAGE_BYTES;
    localparam int unsigned INNER_BLOCK0_MESSAGE_BYTES =
        64 - 3 * N_BYTES - PURE_PREFIX_BYTES;
    localparam int unsigned INNER_REMAINING_MESSAGE_BYTES =
        APP_MESSAGE_BYTES - INNER_BLOCK0_MESSAGE_BYTES;
    localparam int unsigned INNER_BLOCK1_ZERO_BITS =
        SHA_BLOCK_BITS - 8 * INNER_REMAINING_MESSAGE_BYTES - 8;
    localparam int unsigned MGF1_TOTAL_BYTES = 2 * N_BYTES + 32 + 4;
    localparam int unsigned MGF1_BLOCK1_ZERO_BITS =
        SHA_BLOCK_BITS - 32 - 8 - 64;

    localparam logic [63:0] INNER_LENGTH_BITS = 8 * INNER_TOTAL_BYTES;
    localparam logic [63:0] MGF1_LENGTH_BITS  = 8 * MGF1_TOTAL_BYTES;

    typedef enum logic [2:0] {
        StIdle,
        StInner0,
        StInner1,
        StInner2,
        StMgf0,
        StMgf1
    } state_e;

    state_e                            state_q;
    logic [8*N_BYTES-1:0]              randomizer_q;
    logic [8*N_BYTES-1:0]              pk_seed_q;
    logic [8*N_BYTES-1:0]              pk_root_q;
    logic [8*APP_MESSAGE_BYTES-1:0]    message_q;
    logic [255:0]                      inner_digest_q;

    logic                              sha_valid;
    logic [511:0]                      sha_block;
    logic                              sha_last;
    wire                               sha_ready;
    wire [255:0]                       sha_digest;

    sha256_wrap u_sha256 (
        .clk    (clk),
        .rst_n  (rst_n),
        .valid  (sha_valid),
        .block  (sha_block),
        .last   (sha_last),
        .ready  (sha_ready),
        .digest (sha_digest)
    );

    assign busy = (state_q != StIdle);

    always_comb begin
        sha_valid = 1'b0;
        sha_block = '0;
        sha_last  = 1'b0;

        unique case (state_q)
            StInner0: begin
                sha_valid = 1'b1;
                sha_block = {
                    randomizer_q,
                    pk_seed_q,
                    pk_root_q,
                    16'h0000,
                    message_q[8*APP_MESSAGE_BYTES-1
                              -: 8*INNER_BLOCK0_MESSAGE_BYTES]
                };
            end

            StInner1: begin
                sha_valid = 1'b1;
                sha_block = {
                    message_q[8*INNER_REMAINING_MESSAGE_BYTES-1:0],
                    8'h80,
                    {INNER_BLOCK1_ZERO_BITS{1'b0}}
                };
            end

            StInner2: begin
                sha_valid = 1'b1;
                sha_last  = 1'b1;
                sha_block = {448'b0, INNER_LENGTH_BITS};
            end

            StMgf0: begin
                sha_valid = 1'b1;
                sha_block = {randomizer_q, pk_seed_q, inner_digest_q};
            end

            StMgf1: begin
                sha_valid = 1'b1;
                sha_last  = 1'b1;
                sha_block = {
                    32'b0,
                    8'h80,
                    {MGF1_BLOCK1_ZERO_BITS{1'b0}},
                    MGF1_LENGTH_BITS
                };
            end

            default: ;
        endcase
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q       <= StIdle;
            randomizer_q  <= '0;
            pk_seed_q     <= '0;
            pk_root_q     <= '0;
            message_q     <= '0;
            inner_digest_q <= '0;
            digest        <= '0;
            fors_message  <= '0;
            tree_index    <= '0;
            leaf_index    <= '0;
            done          <= 1'b0;
        end else begin
            done <= 1'b0;
            unique case (state_q)
                StIdle: begin
                    if (start) begin
                        randomizer_q <= randomizer_r;
                        pk_seed_q    <= pk_seed;
                        pk_root_q    <= pk_root;
                        message_q    <= application_message;
                        state_q      <= StInner0;
                    end
                end

                StInner0: if (sha_ready) state_q <= StInner1;
                StInner1: if (sha_ready) state_q <= StInner2;
                StInner2: if (sha_ready) begin
                    inner_digest_q <= sha_digest;
                    state_q        <= StMgf0;
                end
                StMgf0: if (sha_ready) state_q <= StMgf1;
                StMgf1: if (sha_ready) begin
                    digest       <= {sha_digest[255:16], 16'b0};
                    fors_message <= sha_digest[255 -: HMSG_FORS_BITS];
                    tree_index   <= sha_digest[87:32] & 56'h3fffffffffffff;
                    leaf_index   <= sha_digest[31:16] & 16'h01ff;
                    done         <= 1'b1;
                    state_q      <= StIdle;
                end
                default: state_q <= StIdle;
            endcase
        end
    end

endmodule
