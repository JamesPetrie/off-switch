// tb_hss.sv — HSS-LMS signature verification test
//
// Uses tb_hss_sign_pkg to sign the message at runtime across HSS_LEVELS
// layers, then pulses valid and checks verif_passed.

module tb (
    input logic clk,
    input logic rst_n
);
    import arith_pkg::*;
    import hss_pkg::*;
    import tb_hss_tree_pkg::*;
    import tb_hss_sign_pkg::*;

    // -------------------------------------------------------------------------
    // Test vector: arbitrary 256-bit message
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] MESSAGE =
        256'h0b98309ccea6343bf486b4b04ec7d7b7a5b5adda1edf46e43a15b5e99edc21b4;

    // -------------------------------------------------------------------------
    // DUT
    // -------------------------------------------------------------------------

    logic             dut_valid = 1'b0;
    logic             dut_ready;
    logic             dut_verif_passed;
    logic             saved_verif_passed = 1'b0;
    license_t         dut_license;

    hss_verify u_dut (
        .clk          (clk),
        .rst_n        (rst_n),
        .valid        (dut_valid),
        .message      (MESSAGE),
        .license      (dut_license),
        .ready        (dut_ready),
        .verif_passed (dut_verif_passed)
    );

    // -------------------------------------------------------------------------
    // Test sequencer
    // -------------------------------------------------------------------------

    localparam int TIMEOUT = 2_000_000;

    typedef enum logic [2:0] {
        PH_INIT,
        PH_START,
        PH_WAIT,
        PH_CHECK,
        PH_DONE
    } ph_e;

    ph_e phase   = PH_INIT;
    int  wait_cnt = 0;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            phase       <= PH_INIT;
            dut_valid   <= 1'b0;
            dut_license <= 0;
            wait_cnt    <= 0;
        end else begin
            case (phase)
                PH_INIT: begin
                    $display("=== HSS-LMS Verification Test (L=%0d) ===", HSS_LEVELS);
                    init_leaves();
                    dut_license <= hss_sign(MESSAGE);
                    phase       <= PH_START;
                end

                PH_START: begin
                    dut_valid <= 1'b1;
                    phase     <= PH_WAIT;
                    wait_cnt  <= 0;
                end

                PH_WAIT: begin
                    dut_valid <= 1'b0;
                    wait_cnt  <= wait_cnt + 1;

                    if (dut_ready) begin
                        $display("  Completed in %0d cycles", wait_cnt);
                        saved_verif_passed <= dut_verif_passed;
                        phase <= PH_CHECK;
                    end else if (wait_cnt > TIMEOUT) begin
                        $display("FAIL  [timeout] after %0d cycles", wait_cnt);
                        $fatal;
                    end
                end

                PH_CHECK: begin
                    if (saved_verif_passed) begin
                        $display("PASS  [HSS-LMS verification] signature valid");
                    end else begin
                        $display("FAIL  [HSS-LMS verification] signature rejected");
                    end
                    phase <= PH_DONE;
                end

                PH_DONE: begin
                    $finish;
                end

                default: ;
            endcase
        end
    end

endmodule
