module tb (
    input logic clk,
    input logic rst_n
);

    // ---------------------------------------------------------------------------
    // Test vector type and array
    // ---------------------------------------------------------------------------

    typedef struct {
        string        name;
        logic [255:0] a;
        logic [255:0] b;
        logic [1:0]   op;        // 0=add, 1=sub, 2=mul, 3=inv (b ignored)
        logic         prime_sel; // 0=prime_p, 1=prime_n
        logic [255:0] expected;
    } test_vec_t;

    // secp256k1 field prime: p = 2^256 - 2^32 - 977
    localparam logic [255:0] P =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F;
    // secp256k1 curve order n
    localparam logic [255:0] N =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141;

    // ---------------------------------------------------------------------------
    // Active tests: add + sub + mul
    // When inv is implemented: append inv vectors and update NumTests
    // ---------------------------------------------------------------------------

    localparam int NumTests = 24;

    localparam test_vec_t TESTS [NumTests] = '{

        // --- Addition ---
        '{name: "Add: 100 + 200 mod p",
          a: 256'd100, b: 256'd200, op: 2'd0, prime_sel: 1'b0,
          expected: 256'd300},

        '{name: "Add: (p-63) + 100 mod p (wrap)",
          a: P - 256'd63, b: 256'd100, op: 2'd0, prime_sel: 1'b0,
          expected: 256'd37},

        '{name: "Add: 12345 + 0 mod p",
          a: 256'd12345, b: 256'd0, op: 2'd0, prime_sel: 1'b0,
          expected: 256'd12345},

        '{name: "Add: 100 + 200 mod n",
          a: 256'd100, b: 256'd200, op: 2'd0, prime_sel: 1'b1,
          expected: 256'd300},

        // --- Subtraction ---
        '{name: "Sub: 500 - 300 mod p",
          a: 256'd500, b: 256'd300, op: 2'd1, prime_sel: 1'b0,
          expected: 256'd200},

        '{name: "Sub: 100 - 200 mod p (wrap)",
          a: 256'd100, b: 256'd200, op: 2'd1, prime_sel: 1'b0,
          expected: P - 256'd100},

        '{name: "Sub: (p-1) - 10 mod p",
          a: P - 256'd1, b: 256'd10, op: 2'd1, prime_sel: 1'b0,
          expected: P - 256'd11},

        '{name: "Sub: 12345 - 0 mod p",
          a: 256'd12345, b: 256'd0, op: 2'd1, prime_sel: 1'b0,
          expected: 256'd12345},

        '{name: "Sub: 10 - 20 mod n (wrap)",
          a: 256'd10, b: 256'd20, op: 2'd1, prime_sel: 1'b1,
          expected: N - 256'd10},

        // --- Multiplication (op=2) ---
        '{name: "Mul: 3 * 5 mod p",
          a: 256'd3, b: 256'd5, op: 2'd2, prime_sel: 1'b0,
          expected: 256'h000000000000000000000000000000000000000000000000000000000000000f},

        '{name: "Mul: 12345 * 0 mod p",
          a: 256'd12345, b: 256'd0, op: 2'd2, prime_sel: 1'b0,
          expected: 256'h0000000000000000000000000000000000000000000000000000000000000000},

        '{name: "Mul: 12345 * 1 mod p",
          a: 256'd12345, b: 256'd1, op: 2'd2, prime_sel: 1'b0,
          expected: 256'h0000000000000000000000000000000000000000000000000000000000003039},

        '{name: "Mul: 123456 * 789012 mod p",
          a: 256'd123456, b: 256'd789012, op: 2'd2, prime_sel: 1'b0,
          expected: 256'h00000000000000000000000000000000000000000000000000000016adfc2d00},

        '{name: "Mul: 64-bit operands mod p",
          a: 256'd12345678901234, b: 256'd98765432109876, op: 2'd2, prime_sel: 1'b0,
          expected: 256'h000000000000000000000000000000000000000003f09a63c9ae1be72ffc8328},

        '{name: "Mul: 128-bit operands mod p",
          a: 256'd123456789012345678901234567890,
          b: 256'd987654321098765432109876543210, op: 2'd2, prime_sel: 1'b0,
          expected: 256'h00000000000000136ccc118300207d2e6cfe0022e5d56a89116ec6de5d5f3ff4},

        '{name: "Mul: 256-bit operands mod p",
          a: 256'd123456789012345678901234567890123456789,
          b: 256'd987654321098765432109876543210987654321, op: 2'd2, prime_sel: 1'b0,
          expected: 256'h0d936c6dd454c29c60200b8f07db6a2cc48ee37874bd5a6e7df6807e34223f56},

        '{name: "Mul: 12345 * 67890 mod n",
          a: 256'd12345, b: 256'd67890, op: 2'd2, prime_sel: 1'b1,
          expected: 256'h0000000000000000000000000000000000000000000000000000000031f46c22},

        // --- Inversion (op=3, b ignored) ---
        '{name: "Inv: 3^-1 mod p",
          a: 256'd3, b: 256'd0, op: 2'd3, prime_sel: 1'b0,
          expected: 256'haaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9fffffd75},

        '{name: "Inv: 1^-1 mod p",
          a: 256'd1, b: 256'd0, op: 2'd3, prime_sel: 1'b0,
          expected: 256'h0000000000000000000000000000000000000000000000000000000000000001},

        '{name: "Inv: (p-1)^-1 mod p",
          a: P - 256'd1, b: 256'd0, op: 2'd3, prime_sel: 1'b0,
          expected: 256'hfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2e},

        '{name: "Inv: 64-bit operand mod p",
          a: 256'd123456789012345, b: 256'd0, op: 2'd3, prime_sel: 1'b0,
          expected: 256'h43935996906f5d218e1ec367f09936b53fc2d144ffe34e491ea06c94d9c5b23a},

        '{name: "Inv: 128-bit operand mod p",
          a: 256'd123456789012345678901234567890, b: 256'd0, op: 2'd3, prime_sel: 1'b0,
          expected: 256'hfe9887f806cf8d2b479104f140d50f5ad3564dbf2da0aac4102af985dcfbdcc1},

        '{name: "Inv: 256-bit operand mod p",
          a: 256'd12345678901234567890123456789012345678901234567890,
          b: 256'd0, op: 2'd3, prime_sel: 1'b0,
          expected: 256'he283f7a0797a92877a86eafa0c633f36504f8c6dc2fcc48c94784d7b6b356746},

        '{name: "Inv: 999999999999999999^-1 mod n",
          a: 256'd999999999999999999, b: 256'd0, op: 2'd3, prime_sel: 1'b1,
          expected: 256'h770324249fefd1cf9af30bf8abb7b824d83d80511a9c7f91c354d804d1eb0322}
    };

    // TB signal to avoid execution before reset
    logic reset_done = 0;

    // ---------------------------------------------------------------------------
    // DUT
    // ---------------------------------------------------------------------------

    // Driven exclusively by the always block below
    logic         valid     = 1'b0;
    logic [1:0]   op        = '0;
    logic         prime_sel = 1'b0;
    logic [255:0] data_a    = '0;
    logic [255:0] data_b    = '0;

    logic         ready;
    logic [255:0] result;

    arith u_dut (
        .clk       (clk),
        .rst_n     (rst_n),
        .valid     (valid),
        .op        (op),
        .prime_sel (prime_sel),
        .data_a    (data_a),
        .data_b    (data_b),
        .ready     (ready),
        .result    (result)
    );

    // ---------------------------------------------------------------------------
    // Test sequencer — sole driver of DUT inputs
    // ---------------------------------------------------------------------------

    // using separate pointers for request and response to allow back-to-back testing
    int next_req_ptr = 0; // Next request to be driven to the DUT
    int curr_rsp_ptr = 0; // Current response driven by the DUT when ready = 1
    int pass_count   = 0;
    int fail_count   = 0;
    int cycles       = 0; // number of cycles with valid asserted for the current request

    // Using always block to allow non-blocking assignment of the DUT inputs in Verilator
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            reset_done <= 1'b1;

            valid     <= 1'b0;
            op        <= '0;
            prime_sel <= 1'b0;
            data_a    <= '0;
            data_b    <= '0;

        end else if (reset_done) begin

            // Drive next request (or idle)
            if (!valid || ready) begin
                if (next_req_ptr < NumTests) begin
                    valid        <= 1'b1;
                    cycles       <= 1;
                    op           <= TESTS[next_req_ptr].op;
                    prime_sel    <= TESTS[next_req_ptr].prime_sel;
                    data_a       <= TESTS[next_req_ptr].a;
                    data_b       <= TESTS[next_req_ptr].b;
                    next_req_ptr <= next_req_ptr + 1;
                end else begin
                    valid  <= 1'b0;
                    cycles <= 0;
                end
            end

            // Check response and handle timeout
            if (ready) begin
                // Result available — check and advance
                if (result === TESTS[curr_rsp_ptr].expected) begin
                    $display("PASS  [%s] — %0d cycle(s)",
                             TESTS[curr_rsp_ptr].name, cycles);
                    pass_count <= pass_count + 1;
                end else begin
                    $display("FAIL  [%s]", TESTS[curr_rsp_ptr].name);
                    $display("      expected: %h", TESTS[curr_rsp_ptr].expected);
                    $display("      actual:   %h", result);
                    fail_count <= fail_count + 1;
                end

                curr_rsp_ptr <= curr_rsp_ptr + 1;

            end else if (valid) begin
                // Waiting — increment cycle counter and watch for timeout
                cycles <= cycles + 1;
                if (cycles > 4000) begin
                    $display("FAIL  [%s] — timeout", TESTS[curr_rsp_ptr].name);
                    fail_count      <= fail_count + 1;
                    curr_rsp_ptr    <= curr_rsp_ptr + 1;
                    valid           <= 1'b0;
                end
            end

            // All tests complete
            if (curr_rsp_ptr == NumTests) begin
                $display("");
                if (fail_count == 0)
                    $display("All %0d arith tests passed.", pass_count);
                else
                    $display("arith: %0d passed, %0d failed.", pass_count, fail_count);
                $finish;
            end

        end
    end

endmodule
