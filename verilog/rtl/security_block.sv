// Security Block
//
// Manages crypto-based license validation, a TRNG nonce source, an allowance
// counter, and a gated workload unit.
// CRYPTO_TYPE selects the verification engine: 0 = ECDSA, 1 = HSS-LMS.
//
// Protocol:
//   1. On startup, waits INIT_DELAY then generates an initial nonce
//   2. nonce_ready is high while a nonce is available for signing
//   3. Submit the license as a beat stream on license_valid/ready/data:
//      one 512-bit beat for ECDSA, metadata then elements for HSS-LMS.
//      The signature must be over the current nonce as the message hash.
//   4. nonce_ready falls when verification starts. On failure, the same nonce is
//      made ready again; after final successful verification it rises only when
//      a new nonce has been generated. The producer sends a fixed number of
//      beats and releases license_valid once the last one is accepted.
//   5. On valid license: allowance += ALLOWANCE_INCREMENT (saturating), request new nonce
//      On invalid license: same nonce retained, can retry
//   6. Workload (signed 8-bit add) is gated: result is zeroed when allowance == 0
//   7. Allowance decrements by 1 every cycle while > 0

module security_block
    import arith_pkg::*;
    import base_pkg::*;
# (
    parameter bit          CRYPTO_TYPE = 0,  // 1 = HSS-LMS, 0 = ECDSA
    parameter int unsigned NUM_SIGNERS = 2,  // Number of signers

    // ECDSA delivers its whole license in one beat; HSS-LMS streams elements.
    localparam int unsigned LICENSE_BEAT_W = CRYPTO_TYPE ? WIDTH
                                                         : $bits(ecdsa_pkg::license_t),
    localparam int unsigned SIGNER_IDX_W = (NUM_SIGNERS > 1) ? $clog2(NUM_SIGNERS) : 1,
    localparam int unsigned ALLOW_W      = 64,
    localparam int unsigned WORKLD_W     =  8,

    parameter logic [ALLOW_W-1:0] ALLOWANCE_INCREMENT = 64'd1_000_000_000_000
)(
    input  logic             clk,
    input  logic             rst_n,

    // License beat stream (valid-ready, one beat per accepted cycle)
    input  logic                        license_valid,
    output logic                        license_ready,
    input  logic [LICENSE_BEAT_W-1:0]   license_data,

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

    // Passed straight through to the crypto backend while a verification is
    // in flight. crypto_input_ready paces the license beats; crypto_done marks
    // the end of the verification.
    logic             crypto_valid;
    logic             crypto_input_ready;
    logic             crypto_done;
    logic             crypto_verif_passed;

    generate
        if (CRYPTO_TYPE == 0) begin : g_ecdsa
            // One beat is the whole license and, as before, must stay stable
            // until the engine is done -- so accepting the beat and finishing
            // the verification are the same event.
            ecdsa_pkg::license_t ecdsa_license;
            wire                 ecdsa_ready;

            assign ecdsa_license       = license_data;
            assign crypto_input_ready  = ecdsa_ready;
            assign crypto_done         = ecdsa_ready;

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
                .ready        (ecdsa_ready),
                .verif_passed (crypto_verif_passed)
            );
        end else begin : g_hss_lms

            hss_verify u_hss (
                .clk          (clk),
                .rst_n        (rst_n),
                .message      (trng_nonce),
                .valid        (crypto_valid),
                .ready        (crypto_input_ready),
                .data         (license_data),
                .identifier   (hss_pkg::PUBKEYS[signer_q].identifier),
                .root_pub_key (hss_pkg::PUBKEYS[signer_q].root_pub_key),
                .verify_done  (crypto_done),
                .verif_passed (crypto_verif_passed)
            );
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
                    // Recognise that a transaction is starting; the beats
                    // themselves are taken by the engine from StWaitVerify.
                    if (license_valid) begin
                        state_d = StWaitVerify;
                    end
                end
            end

            StWaitVerify: begin
                // Hand the stream straight to the crypto backend. The
                // external ready is gated by valid in the RTL rather than
                // relying on the producer: the ECDSA engine reports ready from
                // its own state, so it would otherwise assert after an early
                // release. crypto_done stays ungated -- completion is a fact
                // about the backend, not part of the handshake.
                crypto_valid  = license_valid;
                license_ready = license_valid && crypto_input_ready;

                if (crypto_done) begin
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
