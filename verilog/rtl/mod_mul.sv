// Mod_mul - Modular multiplication via binary shift-and-add (using modular add and double)
//
// Computes (a * b) mod modulus.
// Drives an external mod_add instance for all additions; the caller wires
// mod_add_{valid,a,b,subtract} to the mod_add inputs and feeds
// mod_add_{result,ready} back as inputs to this module.
//
// Protocol:
//   1. Assert valid and hold a, b stable until ready pulses
//   2. Wire the external mod_add as directed by mod_add_valid/a/b/subtract
//   3. Feed mod_add_result and mod_add_ready back as inputs
//   4. ready pulses high for one cycle when result is available
//
// State machine: StIdle -> StAdd (conditional) -> StDone -> StIdle
//                            ^            |
//                            |__StDouble__|

module mod_mul
    import arith_pkg::*; // import in module header to be used in port list
(
    input  logic             clk,
    input  logic             rst_n,
    // Control
    input  logic             valid,
    // Operands (held stable throughout computation)
    input  logic [WIDTH-1:0] a,
    input  logic [WIDTH-1:0] b,
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
    output logic             mod_add_subtract // always 0 for mod_mul (add and double)
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

    // REVISIT - could use MSB first shift-and-add to avoid having a register for the multiplicand
    // multiplicand "left-shifted", actually doubled via modular self-add (no real shifting happens)
    logic [WIDTH-1:0] multiplicand_lsh_q,  multiplicand_lsh_d;

    // REVISIT - could also use a mux to index the multiplier bits (though that is also significant gate count)
    // multiplier right-shifted, here we do real shifting and always check the LSB only
    logic [WIDTH-1:0] multiplier_rsh_q,    multiplier_rsh_d;

    // Accumulates the result after each addition step; holds the final result at the end
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
                    multiplicand_lsh_d  = a;
                    multiplier_rsh_d    = b;
                    result_acc_d        = '0;
                    state_d             = StAdd;
                end
            end

            StAdd: begin
                // Drive mod_add: acc + multiplicand_lsh
                mod_add_valid    = 1'b1;
                mod_add_a        = result_acc_q;
                mod_add_b        = multiplicand_lsh_q;
                mod_add_subtract = 1'b0;

                if (mod_add_ready) begin
                    if (multiplier_rsh_q[0]) begin
                        result_acc_d = mod_add_result;
                    end

                    // Check stop condition (are all other multiplier bits zero?)
                    state_d = (multiplier_rsh_q[WIDTH-1:1] != '0) ? StDouble : StDone;
                end
            end

            StDouble: begin
                // Drive mod_add: multiplicand_lsh * 2 (via self-add)
                mod_add_valid    = 1'b1;
                mod_add_a        = multiplicand_lsh_q;
                mod_add_b        = multiplicand_lsh_q;
                mod_add_subtract = 1'b0;

                if (mod_add_ready) begin
                    multiplicand_lsh_d = mod_add_result;
                    multiplier_rsh_d   = multiplier_rsh_q >> 1;

                    // Optimization: skip StAdd when next LSB=0
                    // Simulation time and run cycles saving might be significant
                    state_d = multiplier_rsh_q[1] ? StAdd : StDouble;
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
        if (!rst_n) begin
            state_q <= StIdle;
        end else begin
            state_q <= state_d;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            multiplicand_lsh_q <= '0;
        end else begin
            multiplicand_lsh_q <= multiplicand_lsh_d;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            multiplier_rsh_q <= '0;
        end else begin
            multiplier_rsh_q <= multiplier_rsh_d;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            result_acc_q <= '0;
        end else begin
            result_acc_q <= result_acc_d;
        end
    end

endmodule
