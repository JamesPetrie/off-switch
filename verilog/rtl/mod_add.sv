module mod_add #(
    localparam int unsigned WIDTH = 256
) (
    input  logic             clk,
    input  logic             rst_n,
    input  logic             valid,
    input  logic [WIDTH-1:0] a,
    input  logic [WIDTH-1:0] b,
    input  logic [WIDTH-1:0] modulus,
    input  logic             subtract,

    output logic             ready,
    output logic [WIDTH-1:0] result,
    output logic             adjust
);

    // FSM enum
    typedef enum logic {
        StAdd,
        StAdjust
    } state_e;

    // ---------------------------------------------------------------------------
    // Registers
    // ---------------------------------------------------------------------------

    // FSM state
    state_e           state_q;
    state_e           state_d;

    // Intermediate result
    logic [WIDTH-1:0] result_ab_q;
    logic [WIDTH-1:0] result_ab_d;

    // Intermediate carry
    logic             carry_ab_q;
    logic             carry_ab_d;

    // ---------------------------------------------------------------------------
    // Adder instance
    // ---------------------------------------------------------------------------

    // Adder inputs
    logic [WIDTH-1:0] adder_a;
    logic [WIDTH-1:0] adder_b;
    logic             adder_subtract;

    // Adder outputs
    logic [WIDTH-1:0] comb_result;
    logic             comb_carry_out;

    // Additional adder_ready signal considered in the FSM to support sequential adders as well
    // adder_ready is always 1 (comb_add has no latency)
    wire adder_ready = 1'b1;

    comb_add u_comb_add (
        .a         (adder_a),
        .b         (adder_b),
        .subtract  (adder_subtract),
        .result    (comb_result),
        .carry_out (comb_carry_out)
    );

    // ---------------------------------------------------------------------------
    // FSM
    // ---------------------------------------------------------------------------

    // FSM: combinational next-state, and output decode (including adder inputs) + data registers controlled by the FSM
    always_comb begin
        // Defaults

        // Next state (maintain by default)
        state_d        = state_q;

        // Adder inputs (unsused defaults, always overridden)
        adder_a        = 'x;
        adder_b        = 'x;
        adder_subtract = 1'bx;

        // Module outputs (masked when inactive)
        ready          = 1'b0;
        adjust         = 1'b0;
        result         = '0;

        // Data registers (maintain by default)
        result_ab_d    = result_ab_q;
        carry_ab_d     = carry_ab_q;


        unique case (state_q)
            StAdd: begin
                // Adder computes a ± b
                adder_a        = a;
                adder_b        = b;
                adder_subtract = subtract;

                // sample adder results and move to next state when inputs are valid and adder is ready
                if (valid && adder_ready) begin
                    result_ab_d = comb_result;
                    carry_ab_d  = comb_carry_out;
                    state_d     = StAdjust;
                end
            end
            StAdjust: begin
                // Adder computes result_ab ∓ modulus for correction (subtract sense inverted)
                adder_a        = result_ab_q;
                adder_b        = modulus;
                adder_subtract = ~subtract;

                // assign final results when adder is ready
                // new adder result is discarded if adjust is not needed
                if (adder_ready) begin
                    ready    = 1'b1;
                    // assign adjust first and then reuse for result
                    adjust   = // when a-b is negative (carry_ab=1)
                               ( subtract && carry_ab_q) ||
                               // when a+b overflowed (carry_ab=1), can't rely on comb_carry_out
                               (!subtract && carry_ab_q) ||
                               // when a+b did not overflow but subtracting m does not make it negative
                               (!subtract && ~comb_carry_out);
                    result   = adjust ? comb_result : result_ab_q;

                    state_d  = StAdd;
                end
            end
            default: ; // empty - defaults are set outside the case statement
        endcase
    end

    // Sequential: register updates, asynchronous active-low reset
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q     <= StAdd;
        end else begin
            state_q     <= state_d;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            result_ab_q <= '0;
            carry_ab_q  <= 1'b0;
        end else begin
            result_ab_q <= result_ab_d;
            carry_ab_q  <= carry_ab_d;
        end
    end

endmodule
