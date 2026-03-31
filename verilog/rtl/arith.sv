// Arith - Modular arithmetic unit for secp256k1 field operations
//
// Performs add, sub, mul, inv modulo either field prime p or curve order n.
// Operands are read from and results written to an external register file.
//
// Operations (op input):
//   0 = add: f <- a + b  mod m
//   1 = sub: f <- a - b  mod m
//   2 = mul: f <- a * b  mod m
//   3 = inv: f <- a^(-1) mod m  (b ignored)
//
// Modulus selection (prime_sel): 0 = prime_p, 1 = prime_n
//
// Protocol:
//   1. Set data_a, data_b, op, prime_sel; pulse valid high and hold until ready
//   2. ready pulses high for one cycle when the result is available

module arith
    import arith_pkg::*; // import in module header to be used in port list
(
    input  logic             clk,
    input  logic             rst_n,
    input  logic             valid,
    input  op_e              op,
    input  logic             prime_sel,
    input  logic [WIDTH-1:0] data_a,
    input  logic [WIDTH-1:0] data_b,

    output logic             ready,
    output logic [WIDTH-1:0] result
);

    // secp256k1 field prime: p = 2^256 - 2^32 - 977
    localparam logic [WIDTH-1:0] PRIME_P =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F;
    // secp256k1 curve order n
    localparam logic [WIDTH-1:0] PRIME_N =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141;

    // ---------------------------------------------------------------------------
    // Prime selection (direct from input)
    // ---------------------------------------------------------------------------

    wire [WIDTH-1:0] selected_prime = prime_sel ? PRIME_N : PRIME_P;

    // ---------------------------------------------------------------------------
    // Shared mod_add — instance
    // ---------------------------------------------------------------------------

    // Inputs shared across multiple blocks, assigned after each block declared
    logic             mod_add_valid;
    logic [WIDTH-1:0] mod_add_a;
    logic [WIDTH-1:0] mod_add_b;
    logic             mod_add_subtract;

    logic             mod_add_ready;
    logic [WIDTH-1:0] mod_add_result;
    logic             mod_add_adjust;

    mod_add u_mod_add (
        .clk      (clk),
        .rst_n    (rst_n),
        .valid    (mod_add_valid),
        .a        (mod_add_a),
        .b        (mod_add_b),
        .modulus  (selected_prime),
        .subtract (mod_add_subtract),
        .ready    (mod_add_ready),
        .result   (mod_add_result),
        .adjust   (mod_add_adjust)
    );

    // ---------------------------------------------------------------------------
    // mod_mul instance
    // ---------------------------------------------------------------------------

    // Input glue logic
    wire mod_mul_valid = valid && (op == OP_MUL);

    // Output nets
    logic             mod_mul_ready;
    logic [WIDTH-1:0] mod_mul_result;

    // mod_add interface
    logic             mod_mul_add_valid;
    logic [WIDTH-1:0] mod_mul_add_a;
    logic [WIDTH-1:0] mod_mul_add_b;
    logic             mod_mul_add_subtract;

    // mod_add resp glue logic
    wire              mod_mul_add_ready  = mod_mul_add_valid && mod_add_ready;
    wire [WIDTH-1:0]  mod_mul_add_result = mod_add_result;

    mod_mul u_mod_mul (
        .clk             (clk),
        .rst_n           (rst_n),
        .valid           (mod_mul_valid),
        .x               (data_a),
        .y               (data_b),
        .mod_add_ready   (mod_mul_add_ready),
        .mod_add_result  (mod_mul_add_result),
        .ready           (mod_mul_ready),
        .result          (mod_mul_result),
        .mod_add_valid   (mod_mul_add_valid),
        .mod_add_a       (mod_mul_add_a),
        .mod_add_b       (mod_mul_add_b),
        .mod_add_subtract(mod_mul_add_subtract)
    );

    // ---------------------------------------------------------------------------
    // mod_inv instance
    // ---------------------------------------------------------------------------

    // Input glue logic
    wire mod_inv_valid = valid && (op == OP_INV);

    // Output nets
    logic             mod_inv_ready;
    logic             mod_inv_exists_unused;    // not used currently at arith level
    logic [WIDTH-1:0] mod_inv_result;

    // mod_add interface
    logic             mod_inv_add_valid;
    logic [WIDTH-1:0] mod_inv_add_a;
    logic [WIDTH-1:0] mod_inv_add_b;
    logic             mod_inv_add_subtract;

    // mod_add resp glue logic
    wire              mod_inv_add_ready  = mod_inv_add_valid && mod_add_ready;
    wire [WIDTH-1:0]  mod_inv_add_result = mod_add_result;
    wire              mod_inv_add_adjust = mod_add_adjust;

    mod_inv u_mod_inv (
        .clk             (clk),
        .rst_n           (rst_n),
        .valid           (mod_inv_valid),
        .x               (data_a),
        .modulus         (selected_prime),
        .mod_add_ready   (mod_inv_add_ready),
        .mod_add_result  (mod_inv_add_result),
        .mod_add_adjust  (mod_inv_add_adjust),
        .ready           (mod_inv_ready),
        .exists          (mod_inv_exists_unused),
        .result          (mod_inv_result),
        .mod_add_valid   (mod_inv_add_valid),
        .mod_add_a       (mod_inv_add_a),
        .mod_add_b       (mod_inv_add_b),
        .mod_add_subtract(mod_inv_add_subtract)
    );

    // ---------------------------------------------------------------------------
    // mod_add input assignments
    // ---------------------------------------------------------------------------

    always_comb begin
        mod_add_valid    = 1'b0;
        mod_add_a        = '0;
        mod_add_b        = '0;
        mod_add_subtract = 1'b0;

        unique case(op)
            OP_ADD: begin
                mod_add_valid    = valid;
                mod_add_a        = data_a;
                mod_add_b        = data_b;
                mod_add_subtract = 1'b0;
            end
            OP_SUB: begin
                mod_add_valid    = valid;
                mod_add_a        = data_a;
                mod_add_b        = data_b;
                mod_add_subtract = 1'b1;
            end
            OP_MUL: begin
                mod_add_valid    = mod_mul_add_valid;
                mod_add_a        = mod_mul_add_a;
                mod_add_b        = mod_mul_add_b;
                mod_add_subtract = mod_mul_add_subtract;
            end
            OP_INV: begin
                mod_add_valid    = mod_inv_add_valid;
                mod_add_a        = mod_inv_add_a;
                mod_add_b        = mod_inv_add_b;
                mod_add_subtract = mod_inv_add_subtract;
            end
        endcase
    end

    // ---------------------------------------------------------------------------
    // Output assignments
    // ---------------------------------------------------------------------------

    always_comb begin
        ready  = 1'b0;
        result = '0;

        unique case(op)
            OP_ADD,
            OP_SUB: begin
                ready  = mod_add_ready;
                result = mod_add_result;
            end
            OP_MUL: begin
                ready  = mod_mul_ready;
                result = mod_mul_result;
            end
            OP_INV: begin
                ready  = mod_inv_ready;
                result = mod_inv_result;
            end
        endcase
    end

endmodule
