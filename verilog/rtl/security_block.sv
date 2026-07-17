// Security Block
//
// Manages crypto-based license validation, a TRNG nonce source, an allowance
// counter, and a gated workload unit.
// CRYPTO_TYPE selects the verification engine:
//   0 = ECDSA, 1 = HSS-LMS, 2 = SLH-DSA-SHA2-128s.
//
// Protocol:
//   1. On startup, waits INIT_DELAY then generates an initial nonce
//   2. nonce_ready pulses when a fresh nonce is available in nonce[]
//   3. Submit a license via valid-ready handshake: assert license_valid with
//      license; transfer completes when license_ready is high.
//      The signature must be over the current nonce as the message hash.
//   4. On valid license: allowance += ALLOWANCE_INCREMENT (saturating), new nonce
//      On invalid license: same nonce retained, can retry
//   5. Workload (signed 8-bit add) is gated: result is zeroed when allowance == 0
//   6. Allowance decrements by 1 every cycle while > 0

module security_block
    import arith_pkg::*;
    import base_pkg::*;
# (
    parameter int unsigned CRYPTO_TYPE = 0,
    parameter int unsigned NUM_SIGNERS =
        (CRYPTO_TYPE == 2) ? 1 : 2,
    parameter bit          SLH_STREAM_INPUT = 1'b0,
    parameter logic [127:0] SLH_PK_SEED = '0,
    parameter logic [127:0] SLH_PK_ROOT = '0,
    parameter logic [127:0] SLH_DEVICE_ID =
        slh_dsa_pkg::OFFSWITCH_DEFAULT_DEVICE_ID,
    parameter logic [63:0]  SLH_POLICY_EPOCH = 64'd1,

    // License width depends on crypto type
    localparam int unsigned LICENSE_W    =
        (CRYPTO_TYPE == 0) ? $bits(ecdsa_pkg::license_t) :
        (CRYPTO_TYPE == 1) ? $bits(hss_pkg::license_t) :
        (SLH_STREAM_INPUT)  ? 1 :
                              8 * slh_dsa_pkg::SIG_BYTES,
    localparam int unsigned SIGNER_IDX_W = (NUM_SIGNERS > 1) ? $clog2(NUM_SIGNERS) : 1,
    localparam int unsigned ALLOW_W      = 64,
    localparam int unsigned WORKLD_W     =  8,

    parameter logic [ALLOW_W-1:0] ALLOWANCE_INCREMENT = 64'd1_000_000_000_000
)(
    input  logic             clk,
    input  logic             rst_n,

    // License interface (valid-ready)
    input  logic                  license_valid,
    output logic                  license_ready,
    input  logic [LICENSE_W-1:0]  license,
    output logic                  license_passed,

    input  logic                           slh_sig_valid,
    output logic                           slh_sig_ready,
    input  logic [slh_dsa_pkg::STREAM_BITS-1:0] slh_sig_data,
    input  logic [slh_dsa_pkg::STREAM_BYTES-1:0] slh_sig_keep,
    input  logic                           slh_sig_last,

    // Workload interface
    input  logic                workload_valid,
    input  logic [WORKLD_W-1:0] workload_a,
    input  logic [WORKLD_W-1:0] workload_b,

    // TRNG seed (for simulation)
    input  logic [WIDTH-1:0] trng_seed,
    input  logic             trng_load_seed,

    // Outputs
    output logic [WIDTH-1:0]    nonce,
    output logic                nonce_ready,
    output logic [WORKLD_W-1:0] workload_result,
    output logic                result_valid,
    output logic [ALLOW_W-1:0]  allowance,
    output logic                enabled
);

    // -------------------------------------------------------------------------
    // Constants
    // -------------------------------------------------------------------------

    localparam int INIT_DELAY = 100;
    localparam int DELAYCNT_W = $clog2(INIT_DELAY); // delay counter width



    // -------------------------------------------------------------------------
    // FSM states
    // -------------------------------------------------------------------------

    typedef enum logic [2:0] {
        StInitDelay,
        StRequestNonce,
        StPublishAndWait,
        StWaitVerify
    } state_e;

    // -------------------------------------------------------------------------
    // Registers
    // -------------------------------------------------------------------------

    state_e                    state_q,            state_d;
    logic [ALLOW_W-1:0]        allowance_q,        allowance_d;
    logic                      result_valid_q,     result_valid_d;
    logic [WORKLD_W-1:0]       workload_result_q,  workload_result_d;
    logic [DELAYCNT_W-1:0]     delay_cnt_q,        delay_cnt_d;  // counts init delay
    // TODO update READMEs for multi signer support
    logic [SIGNER_IDX_W-1:0]   signer_q,           signer_d; // currently expected signer


    // -------------------------------------------------------------------------
    // TRNG instance
    // -------------------------------------------------------------------------

    logic             trng_request_new;
    logic [WIDTH-1:0] trng_nonce;
    logic             trng_nonce_valid;

    trng u_trng (
        .clk         (clk),
        .rst_n       (rst_n),
        .enable      (1'b1),
        .request_new (trng_request_new),
        .load_seed   (trng_load_seed),
        .seed        (trng_seed),
        .nonce       (trng_nonce),
        .nonce_valid (trng_nonce_valid)
    );

    // -------------------------------------------------------------------------
    // Crypto verification engine
    // -------------------------------------------------------------------------

    // Input valid to be driven from the FSM
    logic             crypto_valid;

    // Outputs
    wire              slh_sig_ready_internal;
    logic             crypto_ready;
    logic             crypto_verif_passed;
    assign slh_sig_ready = slh_sig_ready_internal;

    generate
        if (CRYPTO_TYPE == 0) begin : g_ecdsa
            assign slh_sig_ready_internal = 1'b0 & ^{
                slh_sig_valid, slh_sig_data, slh_sig_keep, slh_sig_last
            };
            ecdsa_pkg::license_t ecdsa_license;
            assign ecdsa_license = license;

            ecdsa u_ecdsa (
                .clk          (clk),
                .rst_n        (rst_n),
                .valid        (crypto_valid),
                .z            (trng_nonce),
                .r            (ecdsa_license.r),
                .s            (ecdsa_license.s),
                .q_x          (ecdsa_pkg::PUBKEYS[signer_q].q_x),
                .q_y          (ecdsa_pkg::PUBKEYS[signer_q].q_y),
                .gpq_x        (ecdsa_pkg::PUBKEYS[signer_q].gpq_x),
                .gpq_y        (ecdsa_pkg::PUBKEYS[signer_q].gpq_y),
                .ready        (crypto_ready),
                .verif_passed (crypto_verif_passed)
            );
        end else if (CRYPTO_TYPE == 1) begin : g_hss_lms
            assign slh_sig_ready_internal = 1'b0 & ^{
                slh_sig_valid, slh_sig_data, slh_sig_keep, slh_sig_last
            };
            hss_pkg::license_t hss_license;
            assign hss_license = license;

            hss_verify u_hss (
                .clk          (clk),
                .rst_n        (rst_n),
                .valid        (crypto_valid),
                .message      (trng_nonce),
                .license      (hss_license),
                .identifier   (hss_pkg::PUBKEYS[signer_q].identifier),
                .root_pub_key (hss_pkg::PUBKEYS[signer_q].root_pub_key),
                .ready        (crypto_ready),
                .verif_passed (crypto_verif_passed)
            );
        end else begin : g_slh_dsa
            initial begin
                if (CRYPTO_TYPE != 2) begin
                    $error("Unsupported CRYPTO_TYPE");
                end
                if (NUM_SIGNERS != 1) begin
                    $error("The initial SLH-DSA integration supports one signer");
                end
            end

            if (SLH_STREAM_INPUT) begin : g_stream
                wire stream_crypto_valid = crypto_valid
                    | (1'b0 & ^license);

                slh_stream_verify u_slh (
                    .clk          (clk),
                    .rst_n        (rst_n),
                    .valid        (stream_crypto_valid),
                    .pk_seed      (SLH_PK_SEED),
                    .pk_root      (SLH_PK_ROOT),
                    .message      ({
                        slh_dsa_pkg::OFFSWITCH_DOMAIN,
                        SLH_DEVICE_ID,
                        trng_nonce,
                        SLH_POLICY_EPOCH
                    }),
                    .sig_valid    (slh_sig_valid),
                    .sig_ready    (slh_sig_ready_internal),
                    .sig_data     (slh_sig_data),
                    .sig_keep     (slh_sig_keep),
                    .sig_last     (slh_sig_last),
                    .ready        (crypto_ready),
                    .verif_passed (crypto_verif_passed)
                );
            end else begin : g_packed
                logic [8*slh_dsa_pkg::SIG_BYTES-1:0] slh_license;
                assign slh_license = license;
                assign slh_sig_ready_internal = 1'b0 & ^{
                    slh_sig_valid, slh_sig_data, slh_sig_keep, slh_sig_last
                };

                slh_license_verify u_slh (
                    .clk          (clk),
                    .rst_n        (rst_n),
                    .valid        (crypto_valid),
                    .pk_seed      (SLH_PK_SEED),
                    .pk_root      (SLH_PK_ROOT),
                    .message      ({
                        slh_dsa_pkg::OFFSWITCH_DOMAIN,
                        SLH_DEVICE_ID,
                        trng_nonce,
                        SLH_POLICY_EPOCH
                    }),
                    .license      (slh_license),
                    .ready        (crypto_ready),
                    .verif_passed (crypto_verif_passed)
                );
            end
        end
    endgenerate;

    // -------------------------------------------------------------------------
    // Allowance — combinational next value
    // -------------------------------------------------------------------------

    logic increment_allowance;

    // one bit wider for overflow check
    wire [ALLOW_W:0]  allowance_sum = {1'b0, allowance_q} + {1'b0, ALLOWANCE_INCREMENT};

    always_comb begin
        if (increment_allowance) begin
            // sum if no overflow, else max value (all 1s)
            allowance_d = !allowance_sum[ALLOW_W] ? allowance_sum[ALLOW_W-1:0] : '1;
        end else if (allowance_q != 0) begin
            allowance_d = allowance_q - 1;
        end else begin
            allowance_d = '0;
        end
    end

    // -------------------------------------------------------------------------
    // Workload — combinational, pipelined one cycle
    // -------------------------------------------------------------------------

    assign workload_result_d = {WORKLD_W{enabled}} & (workload_a + workload_b);
    assign result_valid_d    = workload_valid;

    // -------------------------------------------------------------------------
    // FSM — combinational
    // -------------------------------------------------------------------------

    always_comb begin
        // Register input defaults
        state_d             = state_q;
        delay_cnt_d         = delay_cnt_q;
        signer_d            = signer_q;

        // Combinational signal defaults
        trng_request_new    = 1'b0;
        nonce_ready         = 1'b0;
        nonce               =   '0;
        crypto_valid        = 1'b0;
        license_ready       = 1'b0;
        license_passed      = 1'b0;
        increment_allowance = 1'b0;

        unique case (state_q)

            StInitDelay: begin
                delay_cnt_d = delay_cnt_q + 1;
                if (int'(delay_cnt_q) >= INIT_DELAY)
                    state_d = StRequestNonce;
            end

            StRequestNonce: begin
                trng_request_new = 1'b1;
                state_d          = StPublishAndWait;
            end

            StPublishAndWait: begin
                if (trng_nonce_valid) begin
                    nonce_ready = 1;
                    nonce       = trng_nonce;
                    if (license_valid) begin
                        state_d = StWaitVerify;
                    end
                end
            end

            StWaitVerify: begin
                crypto_valid = 1'b1;
                if (crypto_ready) begin
                    license_ready = 1'b1;
                    license_passed = crypto_verif_passed;
                    if (crypto_verif_passed) begin
                        // Require a valid license from each signer (in fixed order)
                        // against the same nonce before rotating the nonce.
                        if (int'(signer_q) == NUM_SIGNERS - 1) begin
                            increment_allowance = 1'b1;
                            signer_d            = '0;
                            state_d             = StRequestNonce;
                        end else begin
                            signer_d = signer_q + 1'b1;
                            state_d  = StPublishAndWait;
                        end
                    end else begin
                        state_d = StPublishAndWait;
                    end
                end
            end

            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // Assign register based outputs
    // -------------------------------------------------------------------------

    assign allowance = allowance_q;
    assign enabled   = (allowance_q != 0) ? 1'b1 : 1'b0;

    assign workload_result = workload_result_q;
    assign result_valid    = result_valid_q;

    // -------------------------------------------------------------------------
    // Sequential
    // -------------------------------------------------------------------------

    // FSM state register
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q <= StInitDelay;
        end else begin
            state_q <= state_d;
        end
    end

    // Allowance register
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            allowance_q <= '0;
        end else begin
            allowance_q <= allowance_d;
        end
    end

    // Workload result pipeline registers
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            result_valid_q <= 1'b0;
            workload_result_q  <= '0;
        end else begin
            result_valid_q <= result_valid_d;
            workload_result_q  <= workload_result_d;
        end
    end

    // Init delay counter
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            delay_cnt_q <= '0;
        end else begin
            delay_cnt_q <= delay_cnt_d;
        end
    end

    // Signer index register
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            signer_q <= '0;
        end else begin
            signer_q <= signer_d;
        end
    end


endmodule
