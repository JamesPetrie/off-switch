// Fixed-width license stream wrapper for security_block.
//
// The wrapper receives the selected backend's complete packed license as
// 256-bit words, most-significant word first. It then holds the reconstructed
// license stable while the existing security_block performs verification.

module security_block_stream
    import arith_pkg::*;
    import base_pkg::*;
# (
    parameter bit          CRYPTO_TYPE = 0,
    parameter int unsigned NUM_SIGNERS = 2,

    localparam int unsigned LICENSE_W = CRYPTO_TYPE ?
        $bits(hss_pkg::license_t) : $bits(ecdsa_pkg::license_t),
    localparam int unsigned LICENSE_BEATS =
        (LICENSE_W + LICENSE_STREAM_W - 1) / LICENSE_STREAM_W,
    localparam int unsigned LICENSE_BUFFER_W =
        LICENSE_BEATS * LICENSE_STREAM_W,
    localparam int unsigned LICENSE_BEAT_CNT_W =
        (LICENSE_BEATS > 1) ? $clog2(LICENSE_BEATS) : 1,
    localparam int unsigned ALLOW_W = 64,
    localparam int unsigned WORKLD_W = 8,

    parameter logic [ALLOW_W-1:0] ALLOWANCE_INCREMENT =
        64'd1_000_000_000_000
) (
    input  logic clk,
    input  logic rst_n,

    input  logic                        license_valid,
    output logic                        license_ready,
    input  logic [LICENSE_STREAM_W-1:0] license,

    input  logic                workload_valid,
    input  logic [WORKLD_W-1:0] workload_a,
    input  logic [WORKLD_W-1:0] workload_b,

    input  logic [WIDTH-1:0] trng_seed,
    input  logic             trng_load_seed,

    output logic [WIDTH-1:0]   nonce,
    output logic               nonce_ready,
    output logic [WORKLD_W-1:0] workload_result,
    output logic               result_valid,
    output logic [ALLOW_W-1:0] allowance,
    output logic               enabled
);

    typedef enum logic {
        StReceive,
        StVerify
    } state_e;

    state_e state_q, state_d;
    logic [LICENSE_BUFFER_W-1:0] license_buffer_q, license_buffer_d;
    logic [LICENSE_BEAT_CNT_W-1:0] license_beat_q, license_beat_d;

    logic core_license_valid;
    logic core_license_ready;
    logic core_nonce_ready;

    wire [LICENSE_W-1:0] packed_license =
        license_buffer_q[LICENSE_BUFFER_W-1 -: LICENSE_W];

    security_block #(
        .CRYPTO_TYPE        (CRYPTO_TYPE),
        .NUM_SIGNERS        (NUM_SIGNERS),
        .ALLOWANCE_INCREMENT(ALLOWANCE_INCREMENT)
    ) u_security_block (
        .clk            (clk),
        .rst_n          (rst_n),
        .license_valid  (core_license_valid),
        .license_ready  (core_license_ready),
        .license        (packed_license),
        .workload_valid (workload_valid),
        .workload_a     (workload_a),
        .workload_b     (workload_b),
        .trng_seed      (trng_seed),
        .trng_load_seed (trng_load_seed),
        .nonce          (nonce),
        .nonce_ready    (core_nonce_ready),
        .workload_result(workload_result),
        .result_valid   (result_valid),
        .allowance      (allowance),
        .enabled        (enabled)
    );

    assign nonce_ready = core_nonce_ready;

    always_comb begin
        state_d          = state_q;
        license_buffer_d = license_buffer_q;
        license_beat_d   = license_beat_q;

        license_ready     = 1'b0;
        core_license_valid = 1'b0;

        unique case (state_q)
            StReceive: begin
                license_ready = core_nonce_ready;
                if (license_valid && license_ready) begin
                    license_buffer_d = {
                        license_buffer_q[
                            LICENSE_BUFFER_W-LICENSE_STREAM_W-1:0
                        ],
                        license
                    };

                    if (license_beat_q ==
                            LICENSE_BEAT_CNT_W'(LICENSE_BEATS - 1)) begin
                        license_beat_d = '0;
                        state_d        = StVerify;
                    end else begin
                        license_beat_d = license_beat_q + 1'b1;
                    end
                end
            end

            StVerify: begin
                core_license_valid = 1'b1;
                if (core_license_ready)
                    state_d = StReceive;
            end

            default: state_d = StReceive;
        endcase
    end

    // A complete transaction overwrites every buffer bit before verification,
    // so the large data buffer does not require reset.
    always_ff @(posedge clk) begin
        license_buffer_q <= license_buffer_d;
    end

    // Reset discards a partial transaction by resetting its word count and
    // returning to the receive state.
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q        <= StReceive;
            license_beat_q <= '0;
        end else begin
            state_q        <= state_d;
            license_beat_q <= license_beat_d;
        end
    end

endmodule
