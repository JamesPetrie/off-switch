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
        logic [255:0] modulus;
        logic         subtract;
        logic [255:0] expected;
    } test_vec_t;

    localparam int NumTests = 9;

    // secp256k1 group order n
    localparam logic [255:0] N =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141;

    // BN254 scalar field modulus r
    localparam logic [255:0] BN254_R =
        256'h030644e72e131a029b85045b68181585d2833e84879b9709142e0f853d28ae55;

    localparam test_vec_t TESTS [NumTests] = '{
        '{name: "Simple addition",
          a: 256'd100, b: 256'd200, modulus: N, subtract: 1'b0,
          expected: 256'd300},

        '{name: "Addition with modular wrap",
          a: N - 256'd63, b: 256'd100, modulus: N, subtract: 1'b0,
          expected: 256'd37},

        '{name: "Simple subtraction",
          a: 256'd500, b: 256'd300, modulus: N, subtract: 1'b1,
          expected: 256'd200},

        '{name: "Subtraction requiring modular correction",
          a: 256'd100, b: 256'd200, modulus: N, subtract: 1'b1,
          expected: N - 256'd100},

        '{name: "Subtraction from modulus-1",
          a: N - 256'd1, b: 256'd10, modulus: N, subtract: 1'b1,
          expected: N - 256'd11},

        '{name: "Add zero",
          a: 256'd12345, b: 256'd0, modulus: N, subtract: 1'b0,
          expected: 256'd12345},

        '{name: "Subtract zero",
          a: 256'd12345, b: 256'd0, modulus: N, subtract: 1'b1,
          expected: 256'd12345},

        '{name: "Different modulus: small prime 997",
          a: 256'd500, b: 256'd600, modulus: 256'd997, subtract: 1'b0,
          expected: 256'd103},

        '{name: "BN254 subtraction wrap",
          a: 256'd10, b: 256'd20, modulus: BN254_R, subtract: 1'b1,
          expected: BN254_R - 256'd10}
    };

    // TB signal to avoid execution before reset
    logic reset_done = 0;

    // ---------------------------------------------------------------------------
    // DUT
    // ---------------------------------------------------------------------------

    // Driven exclusively by the always block below
    logic         valid    = 1'b0;
    logic [255:0] a        = '0;
    logic [255:0] b        = '0;
    logic [255:0] modulus  = '0;
    logic         subtract = 1'b0;

    logic         ready;
    logic [255:0] result;
    logic         adjust;

    mod_add u_dut (
        .clk      (clk),
        .rst_n    (rst_n),
        .valid    (valid),
        .a        (a),
        .b        (b),
        .modulus  (modulus),
        .subtract (subtract),
        .ready    (ready),
        .result   (result),
        .adjust   (adjust)
    );

    // ---------------------------------------------------------------------------
    // Test sequencer — sole driver of DUT inputs
    // ---------------------------------------------------------------------------

    // using separate pointers for request and response to allow back-to-back testing
    int   next_req_ptr = 0; // Next request to be driven to the DUT
    int   curr_rsp_ptr = 0; // Current response driven by the DUT when ready = 1
    int   pass_count = 0;
    int   fail_count = 0;
    int   cycles     = 0;  // number of cycles with valid asserted for the current request

    // Using always block to allow non-blocking assignment of the DUT inputs in Verilator
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            reset_done <= 1'b1;

            valid    <= 1'b0;
            a        <= '0;
            b        <= '0;
            modulus  <= '0;
            subtract <= 1'b0;

        end else if (reset_done) begin

            // Drive next request (or idle)
            if (!valid || ready) begin
                if (next_req_ptr < NumTests) begin
                    valid        <= 1'b1;
                    cycles       <= 1;
                    a            <= TESTS[next_req_ptr].a;
                    b            <= TESTS[next_req_ptr].b;
                    modulus      <= TESTS[next_req_ptr].modulus;
                    subtract     <= TESTS[next_req_ptr].subtract;
                    next_req_ptr <= next_req_ptr + 1;
                end else begin
                    valid   <= 1'b0;
                    cycles  <= 0;
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
                if (cycles > 20) begin
                    $display("FAIL  [%s] — timeout", TESTS[curr_rsp_ptr].name);
                    fail_count      <= fail_count + 1;
                    curr_rsp_ptr    <= curr_rsp_ptr + 1;
                    valid           <= 1'b0;
                end
            end

        end
    end

endmodule
