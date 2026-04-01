// tb_security_block.sv
//
// Nonce derivation:
//   trng_load_seed=1 fires on the first posedge after rst_n deasserts.
//   TRNG counter = TRNG_SEED after that edge, then increments.
//   trng_request_new pulses 100 cycles later (after INIT_DELAY_CYCLES).

`include "tb_math_pkg.sv"

module tb (
    input logic clk,
    input logic rst_n
);
    import arith_pkg::*;
    import tb_math_pkg::*;

    // -------------------------------------------------------------------------
    // DUT signals
    // -------------------------------------------------------------------------

    logic             license_valid  = 1'b0;
    logic             license_ready;
    logic [WIDTH-1:0] license_r      = '0;
    logic [WIDTH-1:0] license_s      = '0;
    logic             workload_valid = 1'b0;
    logic [7:0]       int8_a         = '0;
    logic [7:0]       int8_b         = '0;
    logic [WIDTH-1:0] trng_seed      = '0;
    logic             trng_load_seed = 1'b0;

    logic [WIDTH-1:0] nonce;
    logic             nonce_ready;
    logic [7:0]       int8_result;
    logic             result_valid;
    logic [63:0]      allowance;
    logic             enabled;

    security_block u_dut (
        .clk            (clk),
        .rst_n          (rst_n),
        .license_valid  (license_valid),
        .license_ready  (license_ready),
        .license_r      (license_r),
        .license_s      (license_s),
        .workload_valid (workload_valid),
        .int8_a         (int8_a),
        .int8_b         (int8_b),
        .trng_seed      (trng_seed),
        .trng_load_seed (trng_load_seed),
        .nonce          (nonce),
        .nonce_ready    (nonce_ready),
        .int8_result    (int8_result),
        .result_valid   (result_valid),
        .allowance      (allowance),
        .enabled        (enabled)
    );

    // -------------------------------------------------------------------------
    // Constants
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] TRNG_SEED = 256'd12345;

    // ECDSA signing: d=2 (Q=2G), k=7
    localparam logic [WIDTH-1:0] PRIV_KEY = 256'd2;
    localparam logic [WIDTH-1:0] SIGN_K   = 256'd7;

    localparam int VERIFY_TIMEOUT = 15_000_000;
    localparam int NONCE_TIMEOUT  = 300;

    // -------------------------------------------------------------------------
    // TB state machine
    // -------------------------------------------------------------------------

    typedef enum logic [4:0] {
        PH_INIT,
        PH_T1_CHECK,
        PH_T2_DRIVE,   PH_T2_CHECK,
        PH_T3_CHECK,
        PH_T4_SUBMIT,  PH_T4_CHECK,
        PH_T5_DRIVE,   PH_T5_CHECK,
        PH_T6_SUBMIT,  PH_T6_CHECK,
        PH_T7_DRIVE,   PH_T7_CHECK,
        PH_T8_DRIVE,   PH_T8_CHECK,
        PH_T9_DRIVE,   PH_T9_CHECK,
        PH_T10_DRIVE,  PH_T10_CHECK,
        PH_T11_WAIT,   PH_T11_CHECK,
        PH_T12_SUBMIT, PH_T12_CHECK,
        PH_T13_SUBMIT, PH_T13_CHECK,
        PH_T14_SUBMIT, PH_T14_WAIT, PH_T14_REPLAY, PH_T14_CHECK,
        PH_DONE
    } ph_e;

    ph_e         phase;
    logic        reset_done = 1'b0;
    int          wait_cnt   = 0;
    int          pass_count = 0;
    int          fail_count = 0;
    logic [63:0]      saved_allow;
    logic [WIDTH-1:0] saved_nonce;
    logic [WIDTH-1:0] saved_r;
    logic [WIDTH-1:0] saved_s;

    // -------------------------------------------------------------------------
    // Sequencer
    // -------------------------------------------------------------------------

    // Previous cycle phase register for edge detection
    ph_e phase_d1;
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase_d1 <= PH_INIT;
        end else begin
            phase_d1 <= phase;
        end
    end

    // Timeout counter
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            wait_cnt <= 0;
        end else if (phase != phase_d1) begin // phase change → reset counter
            wait_cnt <= 0;
        end else begin // else increment
            wait_cnt <= wait_cnt + 1;
        end
    end

    // Stimulus and checks - FSM

    // Stimulus driving and checking logic
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase        <= PH_INIT;

            reset_done     <= 1'b1;
            trng_seed      <= TRNG_SEED;
            trng_load_seed <= 1'b1;
            license_valid  <= 1'b0;
            license_r      <= '0;
            license_s      <= '0;
            workload_valid <= 1'b0;
            int8_a         <= '0;
            int8_b         <= '0;
            pass_count     <= 0;
            fail_count     <= 0;
        end else if (reset_done) begin

            case (phase)

                // -------------------------------------------------------
                PH_INIT: begin
                    trng_load_seed <= 1'b0;   // one-cycle seed pulse done
                    phase        <= PH_T1_CHECK;
                end

                // -------------------------------------------------------
                // T1: Initial state
                // -------------------------------------------------------
                PH_T1_CHECK: begin
                    if (allowance == '0 && enabled == 1'b0) begin
                        $display("PASS  [T1  initial state] allowance=0 enabled=0");
                        pass_count <= pass_count + 1;
                    end else begin
                        $display("FAIL  [T1  initial state] allowance=%0d enabled=%0b",
                                 allowance, enabled);
                        fail_count <= fail_count + 1;
                    end
                    phase <= phase.next();
                end

                // -------------------------------------------------------
                // T2: Workload blocked (enabled=0)
                // -------------------------------------------------------
                PH_T2_DRIVE: begin
                    workload_valid <= 1'b1;
                    int8_a         <= 8'd10;
                    int8_b         <= 8'd20;
                    phase        <= PH_T2_CHECK;
                end

                PH_T2_CHECK: begin
                    workload_valid <= 1'b0;
                    if (result_valid) begin

                        // Check
                        if (int8_result == 8'd0) begin
                            $display("PASS  [T2  workload blocked] result=0");
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T2  workload blocked] result=%0d (expected 0)",
                                    int8_result);
                            fail_count <= fail_count + 1;
                        end

                        // Next test
                        phase <= phase.next();
                    end
                end

                // -------------------------------------------------------
                // T3: nonce_ready asserts correctly
                // -------------------------------------------------------
                PH_T3_CHECK: begin
                    if (nonce_ready) begin
                        $display("PASS  [T3  nonce_ready] nonce=0x%h", nonce);
                        pass_count <= pass_count + 1;
                        phase  <= phase.next();
                    end else if (wait_cnt > NONCE_TIMEOUT) begin
                        $fatal("FAIL  [T3  nonce ready] timeout");
                    end
                end

                // -------------------------------------------------------
                // T4: Submit valid license
                // -------------------------------------------------------
                PH_T4_SUBMIT: begin
                    if (nonce_ready) begin
                        ecdsa_sig_t sig;

                        assert(allowance == 0) else $fatal("Expected allowance=0 at license submission, got %0d", allowance);

                        sig = ecdsa_sign(nonce, PRIV_KEY, SIGN_K);
                        license_valid  <= 1'b1;
                        license_r      <= sig.r;
                        license_s      <= sig.s;

                        phase        <= PH_T4_CHECK;
                    end
                end

                PH_T4_CHECK: begin

                    // Hold license_r/s until license_ready pulses
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                    end

                    // Check allowance afterwards (when valid is back to deasserted)
                    if (!license_valid) begin

                        if (allowance != '0) begin
                            $display("PASS  [T4  valid license] allowance incremented to %0d", allowance);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T4  valid license] allowance not incremented");
                            fail_count <= fail_count + 1;
                        end

                        phase       <= phase.next();
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal("FAIL  [T4  valid license] handshake timeout");
                    end
                end

                // -------------------------------------------------------
                // T5: Workload unblocked — 50 + 30 = 80
                // -------------------------------------------------------
                PH_T5_DRIVE: begin

                    assert(allowance != '0) else $fatal("Expected allowance>0 before driving T5, got %0d", allowance);
                    assert(enabled   ==  1) else $fatal("Expected enabled=1 before driving T5, got %0d", enabled);

                    workload_valid <= 1'b1;
                    int8_a         <= 8'd50;
                    int8_b         <= 8'd30;
                    phase          <= PH_T5_CHECK;
                end

                PH_T5_CHECK: begin
                    workload_valid <= 1'b0;

                    if (result_valid) begin

                        // Check
                        if (int8_result == 8'd80) begin
                            $display("PASS  [T5  workload unblocked] 50+30=%0d", int8_result);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T5  workload unblocked] expected 80 got %0d",
                                    int8_result);
                            fail_count <= fail_count + 1;
                        end

                        // Next test
                        phase <= phase.next();
                    end
                end

                // -------------------------------------------------------
                // T6: Invalid license — expect rejection, nonce unchanged
                // Submit VALID_R/VALID_S (valid for z=NONCE_1) against
                // the current nonce (which is no longer NONCE_1).
                // -------------------------------------------------------
                PH_T6_SUBMIT: begin
                    if (nonce_ready) begin
                        license_valid <= 1'b1;
                        license_r     <= 256'd11111;
                        license_s     <= 256'd22222;
                        saved_allow   <= allowance;
                        saved_nonce   <= nonce;
                        phase         <= PH_T6_CHECK;
                    end
                end

                PH_T6_CHECK: begin
                    // Hold license_r/s until license_ready pulses
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                    end

                    // Check after deassert
                    if (!license_valid) begin
                        if (allowance <= saved_allow && nonce == saved_nonce) begin
                            $display("PASS  [T6  invalid license] allowance not incremented, nonce unchanged");
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T6  invalid license] allowance incremented or nonce changed \
                                     (allowance=%0d, expected <=%0d; nonce=0x%h, expected 0x%h)",
                                     allowance, saved_allow, nonce, saved_nonce);
                            fail_count <= fail_count + 1;
                        end
                        phase <= phase.next();
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal("FAIL  [T6  invalid license] timeout");
                    end
                end

                // -------------------------------------------------------
                // T7: Workload — 50 + 30 = 80 (positive values)
                // -------------------------------------------------------
                PH_T7_DRIVE: begin
                    workload_valid <= 1'b1;
                    int8_a         <= 8'd50;
                    int8_b         <= 8'd30;
                    phase          <= PH_T7_CHECK;
                end

                PH_T7_CHECK: begin
                    workload_valid <= 1'b0;

                    if (result_valid) begin

                        // Check
                        if (int8_result == 8'd80) begin
                            $display("PASS  [T7  50+30] result=%0d", int8_result);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T7  50+30] expected 80 got %0d", int8_result);
                            fail_count <= fail_count + 1;
                        end

                        // Next test
                        phase <= phase.next();
                    end
                end

                // -------------------------------------------------------
                // T8: -10 + -20 = -30
                // -------------------------------------------------------
                PH_T8_DRIVE: begin
                    workload_valid <= 1'b1;
                    int8_a         <= 8'hF6;   // -10
                    int8_b         <= 8'hEC;   // -20
                    phase          <= PH_T8_CHECK;
                end

                PH_T8_CHECK: begin
                    workload_valid <= 1'b0;

                    if (result_valid) begin

                        // Check
                        if (int8_result == 8'hE2) begin  // -30
                            $display("PASS  [T8  -10+-20] result=0x%h", int8_result);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T8  -10+-20] expected 0xE2 got 0x%h", int8_result);
                            fail_count <= fail_count + 1;
                        end

                        // Next test
                        phase <= phase.next();
                    end
                end

                // -------------------------------------------------------
                // T9: 100 + -30 = 70
                // -------------------------------------------------------
                PH_T9_DRIVE: begin
                    workload_valid <= 1'b1;
                    int8_a         <= 8'd100;
                    int8_b         <= 8'hE2;   // -30
                    phase          <= PH_T9_CHECK;
                end

                PH_T9_CHECK: begin
                    workload_valid <= 1'b0;

                    if (result_valid) begin

                        // Check
                        if (int8_result == 8'd70) begin
                            $display("PASS  [T9  100+-30] result=%0d", int8_result);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T9  100+-30] expected 70 got %0d", int8_result);
                            fail_count <= fail_count + 1;
                        end

                        // Next test
                        phase <= phase.next();
                    end
                end

                // -------------------------------------------------------
                // T10: 127 + 1 = -128 (overflow wrapping)
                // -------------------------------------------------------
                PH_T10_DRIVE: begin
                    workload_valid <= 1'b1;
                    int8_a         <= 8'd127;
                    int8_b         <= 8'd1;
                    phase          <= PH_T10_CHECK;
                end

                PH_T10_CHECK: begin
                    workload_valid <= 1'b0;

                    if (result_valid) begin

                        // Check
                        if (int8_result == 8'h80) begin  // -128
                            $display("PASS  [T10 127+1 overflow] result=0x%h", int8_result);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T10 127+1 overflow] expected 0x80 got 0x%h", int8_result);
                            fail_count <= fail_count + 1;
                        end

                        // Next test
                        phase       <= phase.next();
                    end
                end

                // -------------------------------------------------------
                // T11: Allowance decrements by 1 per cycle
                // -------------------------------------------------------
                PH_T11_WAIT: begin
                    if (wait_cnt == 0)
                        saved_allow <= allowance; // capture starting allowance at beginning of wait
                    if (wait_cnt == 100)
                        phase <= phase.next();
                end

                PH_T11_CHECK: begin
                    if ( allowance >= (saved_allow - 105) &&
                            allowance <= (saved_allow - 95) ) begin
                        $display("PASS  [T11 allowance decrement] delta=%0d over ~100 cycles", saved_allow - allowance);
                        pass_count <= pass_count + 1;
                    end else begin
                        $display("FAIL  [T11 allowance decrement] delta=%0d, expected ~100", saved_allow - allowance);
                        fail_count <= fail_count + 1;
                    end
                    phase <= phase.next();
                end

                // -------------------------------------------------------
                // T12: New nonce after valid license
                //   Sign the current nonce dynamically, submit, check
                //   that the nonce changes afterwards.
                // -------------------------------------------------------
                PH_T12_SUBMIT: begin
                    if (nonce_ready) begin
                        ecdsa_sig_t sig;
                        sig = ecdsa_sign(nonce, PRIV_KEY, SIGN_K);
                        license_valid <= 1'b1;
                        license_r     <= sig.r;
                        license_s     <= sig.s;
                        saved_nonce   <= nonce;
                        phase         <= PH_T12_CHECK;
                    end
                end

                PH_T12_CHECK: begin
                    // Hold license_r/s until license_ready pulses
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                    end

                    // Check when next nonce is ready (after handshake completed)
                    if (!license_valid && nonce_ready) begin
                        if (nonce != saved_nonce) begin
                            $display("PASS  [T12 new nonce] nonce changed");
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T12 new nonce] nonce unchanged");
                            fail_count <= fail_count + 1;
                        end
                        phase <= phase.next();
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal("FAIL  [T12 new nonce] timeout");
                    end
                end

                // -------------------------------------------------------
                // T13: License signed for wrong nonce is rejected
                //   Sign a wrong nonce (9999), submit against the
                //   current nonce. Expect rejection.
                // -------------------------------------------------------
                PH_T13_SUBMIT: begin
                    if (nonce_ready) begin
                        ecdsa_sig_t sig;
                        sig = ecdsa_sign(256'd9999, PRIV_KEY, SIGN_K);
                        license_valid <= 1'b1;
                        license_r     <= sig.r;
                        license_s     <= sig.s;
                        saved_allow   <= allowance;
                        saved_nonce   <= nonce;
                        phase         <= PH_T13_CHECK;
                    end
                end

                PH_T13_CHECK: begin
                    // Hold license_r/s until license_ready pulses
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                    end

                    // Check after deassert
                    if (!license_valid) begin
                        if (allowance <= saved_allow && nonce == saved_nonce) begin
                            $display("PASS  [T13 wrong nonce] allowance not incremented, nonce unchanged");
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T13 wrong nonce] allowance incremented or nonce changed \
                                     (allowance=%0d, expected <=%0d; nonce=0x%h, expected 0x%h)",
                                     allowance, saved_allow, nonce, saved_nonce);
                            fail_count <= fail_count + 1;
                        end
                        phase <= phase.next();
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal("FAIL  [T13 wrong nonce] timeout");
                    end
                end

                // -------------------------------------------------------
                // T14: Replay attack — submit same signature twice
                //   First submission is valid (signs current nonce),
                //   second reuses the same (r, s) against a new nonce.
                // -------------------------------------------------------
                PH_T14_SUBMIT: begin
                    if (nonce_ready) begin
                        ecdsa_sig_t sig;
                        sig = ecdsa_sign(nonce, PRIV_KEY, SIGN_K);
                        license_valid <= 1'b1;
                        license_r     <= sig.r;
                        license_s     <= sig.s;
                        saved_r       <= sig.r;
                        saved_s       <= sig.s;
                        phase         <= PH_T14_WAIT;
                    end
                end

                PH_T14_WAIT: begin
                    // Hold license_r/s until license_ready pulses
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                        phase <= PH_T14_REPLAY;
                    end
                end

                PH_T14_REPLAY: begin
                    if (nonce_ready) begin
                        // Replay the saved signature against the new nonce
                        license_valid <= 1'b1;
                        license_r     <= saved_r;
                        license_s     <= saved_s;
                        saved_allow   <= allowance;
                        saved_nonce   <= nonce;
                        phase         <= PH_T14_CHECK;
                    end
                end

                PH_T14_CHECK: begin
                    // Hold license_r/s until license_ready pulses
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                    end

                    // Check after deassert
                    if (!license_valid) begin
                        if (allowance <= saved_allow && nonce == saved_nonce) begin
                            $display("PASS  [T14 replay attack] allowance not incremented, nonce unchanged");
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [T14 replay attack] allowance incremented or nonce changed \
                                     (allowance=%0d, expected <=%0d; nonce=0x%h, expected 0x%h)",
                                     allowance, saved_allow, nonce, saved_nonce);
                            fail_count <= fail_count + 1;
                        end
                        phase <= phase.next();
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal("FAIL  [T14 replay attack] timeout");
                    end
                end

                // -------------------------------------------------------
                PH_DONE: begin
                    $display("");
                    if (fail_count == 0)
                        $display("All %0d security_block tests passed.", pass_count);
                    else
                        $display("security_block: %0d passed, %0d FAILED.", pass_count, fail_count);
                    $finish;
                end

                default: ;
            endcase
        end
    end

endmodule
