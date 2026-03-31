// Mod_inv - Modular inverse via Binary Extended GCD
//
// Computes x^(-1) mod modulus, or reports that the inverse does not exist.
// Assumes the modulus is an odd prime (secp256k1 field prime or curve order).
//
// Drives an external mod_add instance for all arithmetic; the caller wires
// mod_add_{valid,a,b,subtract} to the mod_add inputs and feeds
// mod_add_{result,ready,adjust} back as inputs to this module.
//
// Protocol:
//   1. Assert valid and hold x, modulus stable until ready pulses
//   2. Wire the external mod_add as directed by mod_add_valid/a/b/subtract
//   3. Feed mod_add_result, mod_add_ready, mod_add_adjust back as inputs
//   4. ready pulses high for one cycle when result is available
//   5. When ready, check exists: 1 = result is the inverse, 0 = no inverse
//
// State machine:
//   StIdle → StOpSel → StDiv2Add → StDiv2P1 → StOpSel (loop)
//                     → StSubRems → StSubRemsRev → StSubCoeffs → StOpSel (loop)
//                     → StDone → StIdle

module mod_inv
    import arith_pkg::*; // import in module header to be used in port list
(
    input  logic             clk,
    input  logic             rst_n,
    // Control
    input  logic             valid,
    // Operands (held stable throughout computation)
    input  logic [WIDTH-1:0] x,
    input  logic [WIDTH-1:0] modulus,

    // External mod_add resp
    input  logic             mod_add_ready,
    input  logic [WIDTH-1:0] mod_add_result,
    input  logic             mod_add_adjust,

    // Result
    output logic             ready,
    output logic             exists,
    output logic [WIDTH-1:0] result,

    // External mod_add req
    output logic             mod_add_valid,
    output logic [WIDTH-1:0] mod_add_a,
    output logic [WIDTH-1:0] mod_add_b,
    output logic             mod_add_subtract
);

    // wide 1 value ('b0...01)
    localparam bit [WIDTH-1:0] WIDE_1 = {{(WIDTH-1){1'b0}}, 1'b1};

    // FSM states
    typedef enum logic [2:0] {
        StIdle,
        StOpSel,
        StDiv2Add,
        StDiv2P1,
        StSubRems,
        StSubRemsRev,
        StSubCoeffs,
        StDone
    } state_e;

    // ---------------------------------------------------------------------------
    // Registers
    // ---------------------------------------------------------------------------

    // FSM state
    state_e           state_q,           state_d;

    // Remainders
    logic [WIDTH-1:0] x_rem_q,           x_rem_d;
    logic [WIDTH-1:0] y_rem_q,           y_rem_d;

    // Bezout coefficients
    logic [WIDTH-1:0] s_coeff_q,         s_coeff_d;
    logic [WIDTH-1:0] u_coeff_q,         u_coeff_d;

    // Helper flags
    logic             reduced_xny_q,     reduced_xny_d;     // 1 = x was reduced, 0 = y was reduced
    logic             div2_xny_q,        div2_xny_d;        // 1 = dividing x/s, 0 = dividing y/u
    logic             div2_coeff_odd_q,  div2_coeff_odd_d;  // was the original coefficient odd?

    // ---------------------------------------------------------------------------
    // Combinational helpers
    // ---------------------------------------------------------------------------

    // Select current coefficient based on div2_xny
    wire  [WIDTH-1:0] div2_coeff = div2_xny_q ? s_coeff_q : u_coeff_q;

    // ---------------------------------------------------------------------------
    // FSM — combinational next-state, output decode, data register inputs
    // ---------------------------------------------------------------------------

    always_comb begin
        // Outputs (inactive by default)
        ready  = 1'b0;
        result = '0;
        exists = 1'b0;

        // Registers (hold value by default)
        state_d          = state_q;
        x_rem_d          = x_rem_q;
        y_rem_d          = y_rem_q;
        s_coeff_d        = s_coeff_q;
        u_coeff_d        = u_coeff_q;
        reduced_xny_d    = reduced_xny_q;
        div2_xny_d       = div2_xny_q;
        div2_coeff_odd_d = div2_coeff_odd_q;

        // mod_add outputs (masked when inactive)
        mod_add_valid    = 1'b0;
        mod_add_a        = '0;
        mod_add_b        = '0;
        mod_add_subtract = 1'b0;

        unique case (state_q)
            // -----------------------------------------------------------------
            StIdle: begin
                if (valid) begin
                    x_rem_d   = x;
                    y_rem_d   = modulus;
                    s_coeff_d = WIDE_1;
                    u_coeff_d = '0;
                    state_d   = StOpSel;
                end
            end

            // -----------------------------------------------------------------
            StOpSel: begin
                if (x_rem_q == '0) begin
                    // Termination: gcd found
                    state_d  = StDone;
                end else if (!x_rem_q[0]) begin
                    // x is even: divide x/s pair
                    div2_xny_d       = 1'b1;
                    div2_coeff_odd_d = s_coeff_q[0];
                    state_d          = StDiv2Add;
                end else if (!y_rem_q[0]) begin
                    // y is even: divide y/u pair
                    div2_xny_d       = 1'b0;
                    div2_coeff_odd_d = u_coeff_q[0];
                    state_d          = StDiv2Add;
                end else begin
                    // Both odd: subtract remainders
                    state_d = StSubRems;
                end
            end

            // -----------------------------------------------------------------
            // Div2: divide remainder by 2, adjust coefficient
            //
            //   r = r >> 1
            //   if c is even:  c = c >> 1
            //   if c is odd:   c = (c >> 1) + (mod >> 1),  then c = c + 1
            //
            // The odd case is split across StDiv2Add and StDiv2P1 to avoid
            // exceeding WIDTH bits in the intermediate (c + mod) value.
            // -----------------------------------------------------------------
            StDiv2Add: begin
                // Drive mod_add: (c >> 1) + (mod >> 1)
                mod_add_valid    = 1'b1;
                mod_add_a        = div2_coeff >> 1;
                mod_add_b        = modulus >> 1;
                mod_add_subtract = 1'b0;

                if (mod_add_ready) begin
                    // Shift the remainder and update coefficient based on parity
                    if (div2_xny_q) begin
                        x_rem_d   = x_rem_q >> 1;
                        s_coeff_d = div2_coeff[0] ? mod_add_result : (div2_coeff >> 1);
                    end else begin
                        y_rem_d   = y_rem_q >> 1;
                        u_coeff_d = div2_coeff[0] ? mod_add_result : (div2_coeff >> 1);
                    end
                    div2_coeff_odd_d = div2_coeff[0];
                    state_d          = StDiv2P1;
                end
            end

            // -----------------------------------------------------------------
            StDiv2P1: begin
                // Drive mod_add: c + 1
                mod_add_valid = 1'b1;
                mod_add_a     = div2_coeff;
                mod_add_b     = WIDE_1;

                if (mod_add_ready) begin
                    // Apply +1 only if original coefficient was odd
                    if (div2_coeff_odd_q) begin
                        if (div2_xny_q)
                            s_coeff_d = mod_add_result;
                        else
                            u_coeff_d = mod_add_result;
                    end
                    state_d = StOpSel;
                end
            end

            // -----------------------------------------------------------------
            StSubRems: begin
                // Try x - y
                mod_add_valid    = 1'b1;
                mod_add_a        = x_rem_q;
                mod_add_b        = y_rem_q;
                mod_add_subtract = 1'b1;

                if (mod_add_ready) begin
                    if (!mod_add_adjust) begin
                        // No underflow: x >= y
                        x_rem_d       = mod_add_result;
                        reduced_xny_d = 1'b1;
                        state_d       = StSubCoeffs;
                    end else begin
                        // Underflow: x < y, need reverse subtraction
                        state_d = StSubRemsRev;
                    end
                end
            end

            // -----------------------------------------------------------------
            StSubRemsRev: begin
                // y - x (guaranteed no underflow)
                mod_add_valid    = 1'b1;
                mod_add_a        = y_rem_q;
                mod_add_b        = x_rem_q;
                mod_add_subtract = 1'b1;

                if (mod_add_ready) begin
                    y_rem_d       = mod_add_result;
                    reduced_xny_d = 1'b0;
                    state_d       = StSubCoeffs;
                end
            end

            // -----------------------------------------------------------------
            StSubCoeffs: begin
                // If x was reduced: s = s - u, else: u = u - s
                mod_add_valid    = 1'b1;
                mod_add_a        = reduced_xny_q ? s_coeff_q : u_coeff_q;
                mod_add_b        = reduced_xny_q ? u_coeff_q : s_coeff_q;
                mod_add_subtract = 1'b1;

                if (mod_add_ready) begin
                    if (reduced_xny_q)
                        s_coeff_d = mod_add_result;
                    else
                        u_coeff_d = mod_add_result;
                    state_d = StOpSel;
                end
            end

            // -----------------------------------------------------------------
            StDone: begin
                state_d = StIdle;
                ready   = 1'b1;
                exists  = (y_rem_q == WIDE_1); // ignoring m = 0/1 cases (assuming large prime)
                result  = exists ? u_coeff_q : '0;
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
            x_rem_q          <= '0;
            y_rem_q          <= '0;
            s_coeff_q        <= '0;
            u_coeff_q        <= '0;
            reduced_xny_q    <= 1'b0;
            div2_xny_q       <= 1'b0;
            div2_coeff_odd_q <= 1'b0;
        end else begin
            x_rem_q          <= x_rem_d;
            y_rem_q          <= y_rem_d;
            s_coeff_q        <= s_coeff_d;
            u_coeff_q        <= u_coeff_d;
            reduced_xny_q    <= reduced_xny_d;
            div2_xny_q       <= div2_xny_d;
            div2_coeff_odd_q <= div2_coeff_odd_d;
        end
    end

endmodule
