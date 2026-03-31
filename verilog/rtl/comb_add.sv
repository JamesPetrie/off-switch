module comb_add
    import arith_pkg::*; // import in module header to be used in port list
(
    input  wire [WIDTH-1:0] a,
    input  wire [WIDTH-1:0] b,
    input  wire             subtract,
    output wire [WIDTH-1:0] result,
    output wire             carry_out
);

    // Two's complement negation of b:
    //   Step 1: bitwise invert b (ones' complement)
    //   Step 2: add 1 via carry-in (subtract fed as cin below)
    // Together these form -(b) in two's complement.
    // When subtract=0 the uninverted b and cin=0 pass through unchanged.
    wire [WIDTH:0] b_ext = {1'b0, b};
    wire [WIDTH:0] b_eff = subtract ? ~b_ext : b_ext;

    // Single (WIDTH+1)-bit full adder with carry-in.
    // The third operand is a single bit (the carry-in),
    // which synthesis tools map directly to the adder's carry-in port.
    wire [WIDTH:0] sum = {1'b0, a} + b_eff + { {WIDTH{1'b0}}, subtract};

    assign result    = sum[WIDTH-1:0];
    assign carry_out = sum[WIDTH];
endmodule
