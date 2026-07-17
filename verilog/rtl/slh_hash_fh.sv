// FIPS 205 SHA2-128s F/H member functions.
//
// F(PK.seed, ADRS, M1) = Trunc_n(SHA-256(
//     PK.seed || zero_pad_to_64_bytes || ADRSc || M1))
// H(PK.seed, ADRS, M2) uses the same construction with a 2*n-byte message.
//
// Both fixed-profile inputs occupy two SHA-256 blocks after padding. The
// output is top-aligned in the common 256-bit SLH node interface.

module slh_hash_fh
    import slh_dsa_pkg::*;
(
    input  logic                         clk,
    input  logic                         rst_n,

    input  logic                         start,
    input  logic                         use_h,
    input  logic [8*N_BYTES-1:0]         pk_seed,
    input  logic [175:0]                 adrs_c,
    input  logic [MAX_NODE_BITS-1:0]     message,

    output logic                         busy,
    output logic                         done,
    output logic [MAX_NODE_BITS-1:0]     result
);

    localparam int unsigned SHA256_BLOCK_BITS = 512;
    localparam int unsigned PREFIX_ZERO_BITS =
        SHA256_BLOCK_BITS - 8 * N_BYTES;
    localparam int unsigned F_MESSAGE_BITS = 8 * N_BYTES;
    localparam int unsigned H_MESSAGE_BITS = 16 * N_BYTES;
    localparam int unsigned F_TOTAL_BYTES = 64 + 22 + N_BYTES;
    localparam int unsigned H_TOTAL_BYTES = 64 + 22 + 2 * N_BYTES;
    localparam int unsigned F_BLOCK1_ZERO_BITS =
        SHA256_BLOCK_BITS - 176 - F_MESSAGE_BITS - 8 - 64;
    localparam int unsigned H_BLOCK1_ZERO_BITS =
        SHA256_BLOCK_BITS - 176 - H_MESSAGE_BITS - 8 - 64;

    localparam logic [63:0] F_LENGTH_BITS = 8 * F_TOTAL_BYTES;
    localparam logic [63:0] H_LENGTH_BITS = 8 * H_TOTAL_BYTES;

    typedef enum logic [1:0] {
        StIdle,
        StBlock0,
        StBlock1
    } state_e;

    state_e                     state_q;
    logic                       use_h_q;
    logic [8*N_BYTES-1:0]       pk_seed_q;
    logic [175:0]               adrs_c_q;
    logic [MAX_NODE_BITS-1:0]   message_q;

    logic                       sha_valid;
    logic [511:0]               sha_block;
    logic                       sha_last;
    wire                        sha_ready;
    wire [255:0]                sha_digest;
    wire [127:0] unused_sha_digest_tail = sha_digest[127:0];

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
            StBlock0: begin
                sha_valid = 1'b1;
                sha_block = {pk_seed_q, {PREFIX_ZERO_BITS{1'b0}}};
            end

            StBlock1: begin
                sha_valid = 1'b1;
                sha_last  = 1'b1;
                if (use_h_q) begin
                    sha_block = {
                        adrs_c_q,
                        message_q[MAX_NODE_BITS-1 -: H_MESSAGE_BITS],
                        8'h80,
                        {H_BLOCK1_ZERO_BITS{1'b0}},
                        H_LENGTH_BITS
                    };
                end else begin
                    sha_block = {
                        adrs_c_q,
                        message_q[MAX_NODE_BITS-1 -: F_MESSAGE_BITS],
                        8'h80,
                        {F_BLOCK1_ZERO_BITS{1'b0}},
                        F_LENGTH_BITS
                    };
                end
            end

            default: ;
        endcase
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q    <= StIdle;
            use_h_q    <= 1'b0;
            pk_seed_q  <= '0;
            adrs_c_q   <= '0;
            message_q  <= '0;
            done       <= 1'b0;
            result     <= '0;
        end else begin
            done <= 1'b0;
            unique case (state_q)
                StIdle: begin
                    if (start) begin
                        use_h_q   <= use_h;
                        pk_seed_q <= pk_seed;
                        adrs_c_q  <= adrs_c;
                        message_q <= message;
                        state_q   <= StBlock0;
                    end
                end

                StBlock0: begin
                    if (sha_ready) begin
                        state_q <= StBlock1;
                    end
                end

                StBlock1: begin
                    if (sha_ready) begin
                        result  <= {sha_digest[255 -: 8*N_BYTES],
                                    {(MAX_N_BYTES-N_BYTES)*8{1'b0}}};
                        done    <= 1'b1;
                        state_q <= StIdle;
                    end
                end

                default: state_q <= StIdle;
            endcase
        end
    end

endmodule
