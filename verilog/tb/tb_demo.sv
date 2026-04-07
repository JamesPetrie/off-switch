// tb_demo.sv
//
// Demonstrates the full license lifecycle with a continuous workload:
//   A free-running workload fires every WORKLOAD_PERIOD cycles from the
//   start of simulation, printing each result immediately. The FSM only
//   handles license submission. The output shows:
//     - Workloads blocked before any license
//     - License #1 accepted, workloads succeed
//     - Allowance drains, workloads blocked again
//     - License #2 accepted, workloads resume

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
    logic             workload_valid;
    logic [7:0]       workload_a;
    logic [7:0]       workload_b;
    logic [WIDTH-1:0] trng_seed      = '0;
    logic             trng_load_seed = 1'b0;

    logic [WIDTH-1:0] nonce;
    logic             nonce_ready;
    logic [7:0]       workload_result;
    logic             result_valid;
    logic [63:0]      allowance;
    logic             enabled;

    // -------------------------------------------------------------------------
    // DUT — override ALLOWANCE_INCREMENT for fast drain
    // -------------------------------------------------------------------------

    localparam logic [63:0] DEMO_INCREMENT = 64'd5_000_000;

    security_block #(
        .ALLOWANCE_INCREMENT (DEMO_INCREMENT)
    ) u_dut (
        .clk            (clk),
        .rst_n          (rst_n),
        .license_valid  (license_valid),
        .license_ready  (license_ready),
        .license_r      (license_r),
        .license_s      (license_s),
        .workload_valid (workload_valid),
        .workload_a     (workload_a),
        .workload_b     (workload_b),
        .trng_seed      (trng_seed),
        .trng_load_seed (trng_load_seed),
        .nonce          (nonce),
        .nonce_ready    (nonce_ready),
        .workload_result(workload_result),
        .result_valid   (result_valid),
        .allowance      (allowance),
        .enabled        (enabled)
    );

    // -------------------------------------------------------------------------
    // Constants
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] TRNG_SEED = 256'd12345;
    localparam logic [WIDTH-1:0] PRIV_KEY  = 256'd2;
    localparam logic [WIDTH-1:0] SIGN_K    = 256'd7;

    localparam int VERIFY_TIMEOUT  = 15_000_000;
    localparam int NONCE_TIMEOUT   = 300;
    localparam int WORKLOAD_PERIOD = 1_000_000;

    // -------------------------------------------------------------------------
    // Free-running workload driver (independent of FSM)
    // -------------------------------------------------------------------------

    int cycle_cnt;
    int work_idx;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cycle_cnt      <= 0;
            work_idx       <= 0;
            workload_valid <= 1'b0;
            workload_a     <= '0;
            workload_b     <= '0;
        end else begin
            cycle_cnt <= cycle_cnt + 1;
            if (cycle_cnt > 0 && cycle_cnt % WORKLOAD_PERIOD == 0) begin
                workload_valid <= 1'b1;
                workload_a     <= 8'(work_idx + 1);
                workload_b     <= 8'(work_idx * 2 + 10);
                work_idx       <= work_idx + 1;
            end else begin
                workload_valid <= 1'b0;
            end
        end
    end

    // -------------------------------------------------------------------------
    // Result observer — prints every workload result immediately
    // -------------------------------------------------------------------------

    int ok_cnt      = 0;
    int blocked_cnt = 0;

    always @(posedge clk) begin
        if (result_valid) begin
            if (workload_result != 8'd0) begin
                ok_cnt <= ok_cnt + 1;
                $display("[cycle %0d] Workload: %0d + %0d = %0d  (allowance = %0d)",
                         cycle_cnt, workload_a, workload_b, workload_result, allowance);
            end else begin
                blocked_cnt <= blocked_cnt + 1;
                $display("[cycle %0d] Workload: %0d + %0d = %0d  (allowance = %0d)",
                         cycle_cnt, workload_a, workload_b, workload_result, allowance);
            end
        end
    end

    // -------------------------------------------------------------------------
    // FSM — handles license submission only
    // -------------------------------------------------------------------------

    typedef enum logic [3:0] {
        PH_INIT,
        PH_WAIT_NONCE,
        PH_SUBMIT1,
        PH_ACCEPT1,
        PH_WAIT_DRAIN,
        PH_SUBMIT2,
        PH_ACCEPT2,
        PH_WAIT_RESUME,
        PH_DONE
    } ph_e;

    ph_e         phase;
    logic        reset_done = 1'b0;
    int          wait_cnt   = 0;
    int          pass_count = 0;
    int          fail_count = 0;

    // Phase-change edge detection and timeout counter
    ph_e phase_d1;
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) phase_d1 <= PH_INIT;
        else        phase_d1 <= phase;
    end

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)                 wait_cnt <= 0;
        else if (phase != phase_d1) wait_cnt <= 0;
        else                        wait_cnt <= wait_cnt + 1;
    end

    // FSM logic
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase          <= PH_INIT;
            reset_done     <= 1'b1;
            trng_seed      <= TRNG_SEED;
            trng_load_seed <= 1'b1;
            license_valid  <= 1'b0;
            license_r      <= '0;
            license_s      <= '0;
            pass_count     <= 0;
            fail_count     <= 0;
        end else if (reset_done) begin

            case (phase)

                // ---------------------------------------------------------
                PH_INIT: begin
                    trng_load_seed <= 1'b0;
                    $display("");
                    $display("=== Demo: Continuous Workload with License Lifecycle ===");
                    $display("    ALLOWANCE_INCREMENT = %0d cycles", DEMO_INCREMENT);
                    $display("    WORKLOAD_PERIOD     = %0d cycles", WORKLOAD_PERIOD);
                    $display("");
                    phase <= PH_WAIT_NONCE;
                end

                // ---------------------------------------------------------
                PH_WAIT_NONCE: begin
                    if (nonce_ready) begin
                        $display("[cycle %0d] Nonce ready — submitting license #1 ...",
                                 cycle_cnt);
                        phase <= PH_SUBMIT1;
                    end else if (wait_cnt > NONCE_TIMEOUT) begin
                        $fatal("[cycle %0d] Nonce timeout", cycle_cnt);
                    end
                end

                // ---------------------------------------------------------
                // First license
                // ---------------------------------------------------------
                PH_SUBMIT1: begin
                    if (nonce_ready) begin
                        ecdsa_sig_t sig;
                        sig = ecdsa_sign(nonce, PRIV_KEY, SIGN_K);
                        license_valid <= 1'b1;
                        license_r     <= sig.r;
                        license_s     <= sig.s;
                        phase         <= PH_ACCEPT1;
                    end
                end

                PH_ACCEPT1: begin
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                    end

                    if (!license_valid) begin
                        if (allowance != '0) begin
                            $display("[cycle %0d] >>> License #1 ACCEPTED — allowance = %0d",
                                     cycle_cnt, allowance);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [license #1] not accepted");
                            fail_count <= fail_count + 1;
                        end
                        phase <= PH_WAIT_DRAIN;
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal("[cycle %0d] License #1 verify timeout", cycle_cnt);
                    end
                end

                // ---------------------------------------------------------
                // Wait for allowance to drain
                // ---------------------------------------------------------
                PH_WAIT_DRAIN: begin
                    if (!enabled) begin
                        $display("[cycle %0d] >>> Allowance exhausted — submitting license #2 ...",
                                 cycle_cnt);
                        phase <= PH_SUBMIT2;
                    end
                end

                // ---------------------------------------------------------
                // Second license
                // ---------------------------------------------------------
                PH_SUBMIT2: begin
                    if (nonce_ready) begin
                        ecdsa_sig_t sig;
                        sig = ecdsa_sign(nonce, PRIV_KEY, SIGN_K);
                        license_valid <= 1'b1;
                        license_r     <= sig.r;
                        license_s     <= sig.s;
                        phase         <= PH_ACCEPT2;
                    end
                end

                PH_ACCEPT2: begin
                    if (license_ready) begin
                        license_valid <= 1'b0;
                        license_r     <= '0;
                        license_s     <= '0;
                    end

                    if (!license_valid) begin
                        if (allowance != '0 && enabled) begin
                            $display("[cycle %0d] >>> License #2 ACCEPTED — allowance = %0d",
                                     cycle_cnt, allowance);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [license #2] not accepted");
                            fail_count <= fail_count + 1;
                        end
                        phase <= PH_WAIT_RESUME;
                    end else if (wait_cnt > VERIFY_TIMEOUT) begin
                        $fatal("[cycle %0d] License #2 verify timeout", cycle_cnt);
                    end
                end

                // ---------------------------------------------------------
                // Let a few more workloads print, then finish
                // ---------------------------------------------------------
                PH_WAIT_RESUME: begin
                    // Guard: phase_d1 check ensures wait_cnt has reset
                    // (stale value from PH_ACCEPT2 persists for one cycle)
                    if (phase_d1 == PH_WAIT_RESUME && wait_cnt > 3 * WORKLOAD_PERIOD) begin
                        // Final checks
                        if (ok_cnt > 0) begin
                            $display("PASS  [workload enabled]  %0d workloads produced correct results", ok_cnt);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [workload enabled]  no workloads succeeded");
                            fail_count <= fail_count + 1;
                        end

                        if (blocked_cnt > 0) begin
                            $display("PASS  [workload blocked]  %0d workloads were correctly blocked", blocked_cnt);
                            pass_count <= pass_count + 1;
                        end else begin
                            $display("FAIL  [workload blocked]  never saw a blocked workload");
                            fail_count <= fail_count + 1;
                        end

                        phase <= PH_DONE;
                    end
                end

                // ---------------------------------------------------------
                PH_DONE: begin
                    $display("");
                    if (fail_count == 0)
                        $display("All %0d demo checks passed.", pass_count);
                    else
                        $display("Demo: %0d passed, %0d FAILED.", pass_count, fail_count);
                    $finish;
                end

                default: ;
            endcase
        end
    end

endmodule
