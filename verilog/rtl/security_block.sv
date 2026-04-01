// Security Block
//
// Manages ECDSA-based license validation, a TRNG nonce source, an allowance
// counter, and a gated workload unit.
//
// Protocol:
//   1. On startup, waits INIT_DELAY_CYCLES then generates an initial nonce
//   2. nonce_ready pulses when a fresh nonce is available in nonce[]
//   3. Submit a license via valid-ready handshake: assert license_valid with
//      (license_r, license_s); transfer completes when license_ready is high.
//      The signature must be over the current nonce as the message hash.
//   4. On valid license: allowance += ALLOWANCE_INCREMENT (saturating), new nonce
//      On invalid license: same nonce retained, can retry
//   5. Workload (signed 8-bit add) is gated: result is zeroed when allowance == 0
//   6. Allowance decrements by 1 every cycle while > 0

module security_block
    import arith_pkg::*;
# (
    localparam int unsigned ALLOW_W = 64
)(
    input  logic             clk,
    input  logic             rst_n,

    // License interface (valid-ready)
    input  logic             license_valid,
    output logic             license_ready,
    input  logic [WIDTH-1:0] license_r,
    input  logic [WIDTH-1:0] license_s,

    // Workload interface
    input  logic             workload_valid,
    input  logic [7:0]       int8_a,
    input  logic [7:0]       int8_b,

    // TRNG seed (for simulation)
    input  logic [WIDTH-1:0] trng_seed,
    input  logic             trng_load_seed,

    // Outputs
    output logic [WIDTH-1:0]    nonce,
    output logic                nonce_ready,
    output logic [7:0]          int8_result,
    output logic                result_valid,
    output logic [ALLOW_W-1:0]  allowance,
    output logic                enabled
);

    // -------------------------------------------------------------------------
    // Constants
    // -------------------------------------------------------------------------

    localparam int                  INIT_DELAY_CYCLES  = 100;
    localparam logic [ALLOW_W-1:0]  ALLOWANCE_INCREMENT = 64'd1_000_000_000_000;

    // -------------------------------------------------------------------------
    // FSM states
    // -------------------------------------------------------------------------

    typedef enum logic [2:0] {
        StInitDelay,
        StRequestNonce,
        StWaitNonce,
        StPublishAndWait,
        StWaitVerify
    } state_e;

    // -------------------------------------------------------------------------
    // Registers
    // -------------------------------------------------------------------------

    state_e             state_q,      state_d;
    logic [ALLOW_W-1:0] allowance_q,  allowance_d;
    logic [6:0]         delay_cnt_q,  delay_cnt_d;  // counts to 100
    logic               result_valid_q, result_valid_d;
    logic [7:0]         int8_result_q,  int8_result_d;

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
    // ECDSA instance
    // -------------------------------------------------------------------------

    logic             ecdsa_valid;
    logic             ecdsa_ready;
    logic             ecdsa_verif_passed;

    assign ecdsa_valid = (state_q == StPublishAndWait) && license_valid;

    ecdsa u_ecdsa (
        .clk          (clk),
        .rst_n        (rst_n),
        .valid        (ecdsa_valid),
        .z            (trng_nonce),
        .r            (license_r),
        .s            (license_s),
        .ready        (ecdsa_ready),
        .verif_passed (ecdsa_verif_passed)
    );

    // -------------------------------------------------------------------------
    // Allowance — combinational next value
    // -------------------------------------------------------------------------

    logic             increment_allowance;
    wire [ALLOW_W:0]  allowance_sum       = {1'b0, allowance_q} + {1'b0, ALLOWANCE_INCREMENT}; // one bit wider for overflow check

    always_comb begin
        if (increment_allowance)
            allowance_d = !allowance_sum[ALLOW_W] ? allowance_sum[ALLOW_W-1:0] : '1; // sum if no overflow, else max value (all 1s)
        else if (allowance_q != 0)
            allowance_d = allowance_q - 1;
        else
            allowance_d = '0;
    end

    // -------------------------------------------------------------------------
    // Workload — combinational, pipelined one cycle
    // -------------------------------------------------------------------------

    always_comb begin
        logic [7:0] sum;
        sum              = int8_a + int8_b;
        int8_result_d    = sum & {8{enabled}};
        result_valid_d   = workload_valid;
    end

    // -------------------------------------------------------------------------
    // FSM — combinational
    // -------------------------------------------------------------------------

    always_comb begin
        // Defaults
        state_d             = state_q;
        nonce_ready         = 1'b0;
        increment_allowance = 1'b0;
        delay_cnt_d         = delay_cnt_q;
        trng_request_new    = 1'b0;

        unique case (state_q)

            StInitDelay: begin
                delay_cnt_d = delay_cnt_q + 1;
                if (delay_cnt_q >= 7'(INIT_DELAY_CYCLES - 1))
                    state_d = StRequestNonce;
            end

            StRequestNonce: begin
                trng_request_new = 1'b1;
                state_d          = StWaitNonce;
            end

            StWaitNonce: begin
                if (trng_nonce_valid)
                    state_d = StPublishAndWait;
            end

            StPublishAndWait: begin
                nonce_ready = 1;
                if (license_valid)
                    state_d = StWaitVerify;
            end

            StWaitVerify: begin
                if (ecdsa_ready) begin
                    if (ecdsa_verif_passed) begin
                        increment_allowance = 1'b1;
                        state_d             = StRequestNonce;
                    end else begin
                        state_d = StPublishAndWait;
                    end
                end
            end

            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // Output assignments
    // -------------------------------------------------------------------------

    assign nonce         = trng_nonce;
    assign license_ready = ecdsa_ready;
    assign allowance     = allowance_q;
    assign enabled       = (allowance_q != 0);
    assign int8_result   = int8_result_q;
    assign result_valid  = result_valid_q;

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
            int8_result_q  <= '0;
        end else begin
            result_valid_q <= result_valid_d;
            int8_result_q  <= int8_result_d;
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


endmodule
