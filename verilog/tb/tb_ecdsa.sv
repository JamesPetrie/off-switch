module tb (
    input logic clk,
    input logic rst_n
);
    import ecdsa_pkg::*;

    // -------------------------------------------------------------------------
    // Test vector type
    // -------------------------------------------------------------------------

    typedef struct {
        string        name;
        logic [255:0] z;
        logic [255:0] r;
        logic [255:0] s;
        logic         expect_verif_passed;
    } test_vec_t;

    // -------------------------------------------------------------------------
    // Test vectors (r, s pre-computed from ECDSA signing with d=2, Q=2G)
    // -------------------------------------------------------------------------

    localparam int NumTests = 8;

    localparam test_vec_t TESTS [NumTests] = '{

        // Test 1: Valid signature (z=12345, k=7)
        '{name: "Valid: z=12345, k=7",
          z: 256'h0000000000000000000000000000000000000000000000000000000000003039,
          r: 256'h5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc,
          s: 256'hf5ed201cb1d1a1679c74d7d3fc42fe4c1f3ae9c52970ca600b9c474eec66cf51,
          expect_verif_passed: 1'b1},

        // Test 2: Valid signature (z=0xDEADBEEF, k=0x123456)
        '{name: "Valid: z=0xDEADBEEF, k=0x123456",
          z: 256'h00000000000000000000000000000000000000000000000000000000DEADBEEF,
          r: 256'hf8ccf508990ceef9e5b84f5aeb9fee1739d3d3b140fc05e5b2ff58524c660ba2,
          s: 256'h98c86259b3f72418d11058c5ec03fc5dca499123880da2d4a989089afef4be26,
          expect_verif_passed: 1'b1},

        // Test 3: Valid signature (256-bit z, k)
        '{name: "Valid: 256-bit z, k",
          z: 256'hb94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9,
          r: 256'hd595ec6770e9878b6ee380665e7f6785f32cf6a2f2b31343504c4e9e96622ff0,
          s: 256'h8ce29a99cf1a9f77a7cd29fbd9f240b76b3215222b10ce7fa184083b2782a379,
          expect_verif_passed: 1'b1},

        // Test 4: Invalid — wrong message hash (z=99999 instead of 12345)
        '{name: "Invalid: wrong z",
          z: 256'h000000000000000000000000000000000000000000000000000000000001869F,
          r: 256'h5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc,
          s: 256'hf5ed201cb1d1a1679c74d7d3fc42fe4c1f3ae9c52970ca600b9c474eec66cf51,
          expect_verif_passed: 1'b0},

        // Test 5: Invalid — wrong r (r=11111 instead of real r)
        '{name: "Invalid: wrong r",
          z: 256'h0000000000000000000000000000000000000000000000000000000000003039,
          r: 256'h0000000000000000000000000000000000000000000000000000000000002B67,
          s: 256'hf5ed201cb1d1a1679c74d7d3fc42fe4c1f3ae9c52970ca600b9c474eec66cf51,
          expect_verif_passed: 1'b0},

        // Test 6: Invalid — wrong s (s=22222 instead of real s)
        '{name: "Invalid: wrong s",
          z: 256'h0000000000000000000000000000000000000000000000000000000000003039,
          r: 256'h5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc,
          s: 256'h00000000000000000000000000000000000000000000000000000000000056CE,
          expect_verif_passed: 1'b0},

        // Test 7: Valid signature (z=0xCAFEBABE, k=0x999)
        '{name: "Valid: z=0xCAFEBABE, k=0x999",
          z: 256'h00000000000000000000000000000000000000000000000000000000CAFEBABE,
          r: 256'h2e43be7a12916cf6f312a513fcb6c98b708ce2dd18dc4ebf72a807c9c8a31b0d,
          s: 256'h8f67ef46a32e112d1b99b2d2e6adbb9a55e2b8894a1dcecec8e039f56b5eb2f8,
          expect_verif_passed: 1'b1},

        // Test 8: Invalid — random z/r/s
        '{name: "Invalid: random z/r/s",
          z: 256'h0000000000000000000000000000000000000000000000001111111111111111,
          r: 256'h0000000000000000000000000000000000000000000000002222222222222222,
          s: 256'h0000000000000000000000000000000000000000000000003333333333333333,
          expect_verif_passed: 1'b0}
    };

    // TB signal to avoid execution before reset
    logic reset_done = 0;

    // -------------------------------------------------------------------------
    // DUT
    // -------------------------------------------------------------------------

    logic         valid      = 1'b0;
    logic [255:0] dut_z      = '0;
    logic [255:0] dut_r      = '0;
    logic [255:0] dut_s      = '0;

    logic         ready;
    logic         verif_passed;

    ecdsa u_dut (
        .clk       (clk),
        .rst_n     (rst_n),
        .valid     (valid),
        .z         (dut_z),
        .r         (dut_r),
        .s         (dut_s),
        .q_x       (PUBKEYS[0].q_x),
        .q_y       (PUBKEYS[0].q_y),
        .gpq_x     (PUBKEYS[0].gpq_x),
        .gpq_y     (PUBKEYS[0].gpq_y),
        .ready     (ready),
        .verif_passed (verif_passed)
    );

    // -------------------------------------------------------------------------
    // Test sequencer
    // -------------------------------------------------------------------------

    int next_req_ptr = 0;
    int curr_rsp_ptr = 0;
    int pass_count   = 0;
    int fail_count   = 0;
    int cycles       = 0;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            reset_done <= 1'b1;

            valid  <= 1'b0;
            dut_z  <= '0;
            dut_r  <= '0;
            dut_s  <= '0;

        end else if (reset_done) begin

            // Drive next request (or idle)
            if (!valid || ready) begin
                if (next_req_ptr < NumTests) begin
                    valid        <= 1'b1;
                    cycles       <= 1;
                    dut_z        <= TESTS[next_req_ptr].z;
                    dut_r        <= TESTS[next_req_ptr].r;
                    dut_s        <= TESTS[next_req_ptr].s;
                    next_req_ptr <= next_req_ptr + 1;
                end else begin
                    valid  <= 1'b0;
                    cycles <= 0;
                end
            end

            // Check response and handle timeout
            if (ready) begin
                if (verif_passed === TESTS[curr_rsp_ptr].expect_verif_passed) begin
                    $display("PASS  [%s] — %0d cycle(s), verif_passed=%0b",
                             TESTS[curr_rsp_ptr].name, cycles, verif_passed);
                    pass_count <= pass_count + 1;
                end else begin
                    $display("FAIL  [%s] — expected verif_passed=%0b, got %0b",
                             TESTS[curr_rsp_ptr].name,
                             TESTS[curr_rsp_ptr].expect_verif_passed, verif_passed);
                    fail_count <= fail_count + 1;
                end

                curr_rsp_ptr <= curr_rsp_ptr + 1;

            end else if (valid) begin
                cycles <= cycles + 1;
                if (cycles > 10000000) begin
                    $display("FAIL  [%s] — timeout", TESTS[curr_rsp_ptr].name);
                    fail_count   <= fail_count + 1;
                    curr_rsp_ptr <= curr_rsp_ptr + 1;
                    valid        <= 1'b0;
                end
            end

            // All tests complete
            if (curr_rsp_ptr == NumTests) begin
                $display("");
                if (fail_count == 0) begin
                    $display("All %0d ECDSA tests passed.", pass_count);
                    $finish;
                end else begin
                    $display("ECDSA: %0d passed, %0d failed.", pass_count, fail_count);
                    $fatal;
                end
            end

        end
    end

endmodule
