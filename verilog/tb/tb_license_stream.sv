// Protocol checks for the conservative 256-bit license stream wrapper.

module tb (
    input logic clk,
    input logic rst_n
);
    import arith_pkg::*;

    logic local_rst_n = 1'b1;
    wire dut_rst_n = rst_n && local_rst_n;
    logic license_valid = 1'b0;
    logic license_ready;
    logic [255:0] license = '0;
    logic [WIDTH-1:0] nonce;
    logic nonce_ready;
    logic [7:0] workload_result;
    logic result_valid;
    logic [63:0] allowance;
    logic enabled;

    security_block_stream #(
        .CRYPTO_TYPE(0),
        .NUM_SIGNERS(1)
    ) u_dut (
        .clk             (clk),
        .rst_n           (dut_rst_n),
        .license_valid   (license_valid),
        .license_ready   (license_ready),
        .license         (license),
        .workload_valid  (1'b0),
        .workload_a      ('0),
        .workload_b      ('0),
        .trng_seed       (256'h1234),
        .trng_load_seed  (1'b0),
        .nonce           (nonce),
        .nonce_ready     (nonce_ready),
        .workload_result (workload_result),
        .result_valid    (result_valid),
        .allowance       (allowance),
        .enabled         (enabled)
    );

    task automatic send_word(input logic [255:0] value);
        @(negedge clk);
        license       = value;
        license_valid = 1'b1;
        do begin
            @(posedge clk);
        end while (!license_ready);
        @(negedge clk);
        license_valid = 1'b0;
    endtask

    initial begin
        @(negedge rst_n);
        @(posedge rst_n);
        wait (nonce_ready && license_ready);

        send_word(256'h1111);
        if (u_dut.license_beat_q != 1 || u_dut.core_license_valid) begin
            $fatal(1, "first word started verification or counter mismatch");
        end
        $display("PASS [one word does not start ECDSA verification]");

        @(negedge clk);
        local_rst_n = 1'b0;
        repeat (2) @(posedge clk);
        if (u_dut.license_beat_q != 0) begin
            $fatal(1, "reset did not discard the partial transaction");
        end
        @(negedge clk);
        local_rst_n = 1'b1;
        wait (nonce_ready && license_ready);
        $display("PASS [reset discards a partial transaction]");

        send_word(256'h2222);
        send_word(256'h3333);
        @(negedge clk);
        if (!u_dut.core_license_valid || license_ready) begin
            $fatal(1, "final word did not start verification or apply backpressure");
        end
        $display("PASS [final word starts verification and applies backpressure]");
        $display("All 3 license stream protocol checks passed.");
        $finish;
    end

endmodule
