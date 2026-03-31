// Mod_mul - Modular multiplication via right-to-left binary shift-and-add
//
// Computes (x * y) mod modulus.
// Drives an external mod_add instance for all additions; the caller wires
// mod_add_{valid,a,b,subtract} to the mod_add inputs and feeds
// mod_add_{result,ready} back as inputs to this module.
//
// Protocol:
//   1. Assert valid and hold x, y stable until ready pulses
//   2. Wire the external mod_add as directed by mod_add_valid/a/b/subtract
//   3. Feed mod_add_result and mod_add_ready back as inputs
//   4. ready pulses high for one cycle when result is available
//
// State machine: StIdle -> StAdd/StDouble <-> StDouble -> StDone -> StIdle

module mod_mul
    import arith_pkg::*; // import in module header to be used in port list
(
    input  logic             clk,
    input  logic             rst_n,
    // Control
    input  logic             valid,
    // Operands (held stable throughout computation)
    input  logic [WIDTH-1:0] x,
    input  logic [WIDTH-1:0] y,
    // input  logic [WIDTH-1:0] modulus, // feeding the modulus to mod_add is taken care of in the arith block

    // External mod_add resp
    input  logic             mod_add_ready,
    input  logic [WIDTH-1:0] mod_add_result,

    // Result
    output logic             ready,
    output logic [WIDTH-1:0] result,

    // External mod_add req
    output logic             mod_add_valid,
    output logic [WIDTH-1:0] mod_add_a,
    output logic [WIDTH-1:0] mod_add_b,
    output logic             mod_add_subtract
);

    // FSM states
    typedef enum logic [1:0] {
        StIdle,
        StAdd,
        StDouble,
        StDone
    } state_e;

    // ---------------------------------------------------------------------------
    // Registers
    // ---------------------------------------------------------------------------

    // FSM state
    state_e           state_q,             state_d;

    // Accumulator and shift registers
    logic [WIDTH-1:0] multiplicand_lsh_q,  multiplicand_lsh_d;
    logic [WIDTH-1:0] multiplier_rsh_q,    multiplier_rsh_d;
    logic [WIDTH-1:0] result_acc_q,        result_acc_d;

    // ---------------------------------------------------------------------------
    // FSM — combinational next-state, output decode, data register inputs
    // ---------------------------------------------------------------------------

    always_comb begin
        // Outputs (inactive by default)
        ready               = 1'b0;
        result              = '0;

        // Registers (hold value by default)
        state_d             = state_q;
        multiplicand_lsh_d  = multiplicand_lsh_q;
        multiplier_rsh_d    = multiplier_rsh_q;
        result_acc_d        = result_acc_q;

        // mod_add outputs (masked when inactive)
        mod_add_valid    = 1'b0;
        mod_add_a        = '0;
        mod_add_b        = '0;
        mod_add_subtract = 1'b0;

        unique case (state_q)
            StIdle: begin
                if (valid) begin
                    multiplicand_lsh_d  = x;
                    multiplier_rsh_d    = y;
                    result_acc_d        = '0;
                    // Determine starting state based on multiplier value
                    // Note: Using register input! Dependency on the assignment above
                    state_d = multiplier_rsh_d[0]    ? StAdd    :
                              multiplier_rsh_d != '0 ? StDouble :
                                                       StDone;
                end
            end

            StAdd: begin
                // Only entered when multiplier_rsh_q[0] = 1
                mod_add_valid    = 1'b1;
                mod_add_a        = result_acc_q;
                mod_add_b        = multiplicand_lsh_q;
                mod_add_subtract = 1'b0;

                if (mod_add_ready) begin
                    result_acc_d = mod_add_result;
                    state_d      = StDouble;
                end
            end

            StDouble: begin
                // Drive mod_add: curr_multiplicand * 2 (via self-add)
                mod_add_valid    = 1'b1;
                mod_add_a        = multiplicand_lsh_q;
                mod_add_b        = multiplicand_lsh_q;
                mod_add_subtract = 1'b0;

                if (mod_add_ready) begin
                    multiplicand_lsh_d = mod_add_result;
                    multiplier_rsh_d   = multiplier_rsh_q >> 1;

                    // Determine starting state based on new multiplier value
                    // Note: Using register input! Dependency on the assignment above
                    state_d = multiplier_rsh_d[0]    ? StAdd    :
                              multiplier_rsh_d != '0 ? StDouble :
                                                       StDone;
                end
            end

            StDone: begin
                state_d = StIdle;
                ready   = 1'b1;
                result  = result_acc_q;
            end

            default: ; // empty — defaults are set outside the case statement
        endcase
    end

    // ---------------------------------------------------------------------------
    // Sequential: register updates, asynchronous active-low reset
    // ---------------------------------------------------------------------------

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) state_q <= StIdle;
        else        state_q <= state_d;
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            multiplicand_lsh_q <= '0;
            multiplier_rsh_q   <= '0;
            result_acc_q       <= '0;
        end else begin
            multiplicand_lsh_q <= multiplicand_lsh_d;
            multiplier_rsh_q   <= multiplier_rsh_d;
            result_acc_q       <= result_acc_d;
        end
    end

endmodule
