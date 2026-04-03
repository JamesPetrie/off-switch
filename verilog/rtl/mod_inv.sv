// Mod_inv - Modular inverse via Binary Extended GCD
//
// Computes a^(-1) mod modulus, or reports that the inverse does not exist.
// Assumes the modulus is an odd prime (secp256k1 field prime or curve order).
//
// Drives an external mod_add instance for all arithmetic; the caller wires
// mod_add_{valid,a,b,subtract} to the mod_add inputs and feeds
// mod_add_{result,ready,adjust} back as inputs to this module.
//
// Protocol:
//   1. Assert valid and hold a, modulus stable until ready pulses
//   2. Wire the external mod_add as directed by mod_add_valid/a/b/subtract
//   3. Feed mod_add_result, mod_add_ready, mod_add_adjust back as inputs
//   4. ready pulses high for one cycle when result is available
//   5. When ready, check exists: 1 = result is the inverse, 0 = no inverse
//
// State machine:
//               _________________________________________________________
//              |    _______________________                              |
//              |   |                       |                             |
//              |   |    -> StDiv2Add -> StDiv2P1                         |
//              v   v   |                                                 |
//   StIdle -> StOpSel -|-> StSubRems -> StSubRemsRev (Conditional) -> StSubCoeffs
//                      |
//                       -> StDone    -> StIdle


module mod_inv
    import arith_pkg::*; // import in module header to be used in port list
(
    input  logic             clk,
    input  logic             rst_n,
    // Control
    input  logic             valid,
    input  logic [WIDTH-1:0] a,
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

    // Remainders and a's Bezout coefficients
    // followings must always hold:
    //      a*s == u mod m (same as a*s + m*x == u mod m)
    //      a*t == v mod m (same as a*t + m*y == v mod m)
    // Note: coefficient for the modulus vanishes due to mod m arithmetic, so no need to track those
    logic [WIDTH-1:0] u_rem_q,           u_rem_d;
    logic [WIDTH-1:0] v_rem_q,           v_rem_d;
    logic [WIDTH-1:0] s_coeff_q,         s_coeff_d;
    logic [WIDTH-1:0] t_coeff_q,         t_coeff_d;

    // Helper flags
    logic             reduced_unv_q,     reduced_unv_d;     // 1 = u was reduced, 0 = v was reduced
    logic             div2_unv_q,        div2_unv_d;        // 1 = dividing u/s, 0 = dividing v/t
    logic             div2_coeff_odd_q,  div2_coeff_odd_d;  // was the original coefficient odd?

    // ---------------------------------------------------------------------------
    // Combinational helpers
    // ---------------------------------------------------------------------------

    // Select current coefficient based on div2_unv
    wire  [WIDTH-1:0] div2_coeff = div2_unv_q ? s_coeff_q : t_coeff_q;

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
        u_rem_d          = u_rem_q;
        v_rem_d          = v_rem_q;
        s_coeff_d        = s_coeff_q;
        t_coeff_d        = t_coeff_q;
        reduced_unv_d    = reduced_unv_q;
        div2_unv_d       = div2_unv_q;
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
                    u_rem_d   = a;          // u = a
                    v_rem_d   = modulus;    // v = b = modulus
                    s_coeff_d = 1;          // a*s == u mod m => s = 1
                    t_coeff_d = 0;          // a*t == v mod m => t = 0
                    state_d   = StOpSel;
                end
            end

            // -----------------------------------------------------------------
            StOpSel: begin
                if (u_rem_q == '0) begin
                    // Termination: gcd found
                    state_d  = StDone;
                end else if (!u_rem_q[0]) begin
                    // u is even: divide u/s pair
                    div2_unv_d       = 1'b1;
                    div2_coeff_odd_d = s_coeff_q[0];
                    state_d          = StDiv2Add;
                end else if (!v_rem_q[0]) begin
                    // v is even: divide v/t pair
                    div2_unv_d       = 1'b0;
                    div2_coeff_odd_d = t_coeff_q[0];
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
                // Drive mod_add: (c >> 1) + (mod >> 1) in case it's needed
                mod_add_valid    = 1'b1;
                mod_add_a        = div2_coeff >> 1;
                mod_add_b        = modulus >> 1;
                mod_add_subtract = 1'b0;

                if (mod_add_ready) begin
                    // Shift the remainder and update coefficient based on parity
                    if (div2_unv_q) begin
                        u_rem_d   = u_rem_q >> 1;
                        s_coeff_d = div2_coeff[0] ? mod_add_result : (div2_coeff >> 1);
                    end else begin
                        v_rem_d   = v_rem_q >> 1;
                        t_coeff_d = div2_coeff[0] ? mod_add_result : (div2_coeff >> 1);
                    end
                    div2_coeff_odd_d = div2_coeff[0];
                    state_d          = StDiv2P1;
                end
            end

            // -----------------------------------------------------------------
            StDiv2P1: begin
                // Drive mod_add: c + 1 in case it's needed
                mod_add_valid = 1'b1;
                mod_add_a     = div2_coeff;
                mod_add_b     = 1;

                if (mod_add_ready) begin
                    // Apply the +1 only if original coefficient was odd
                    if (div2_coeff_odd_q) begin
                        if (div2_unv_q)
                            s_coeff_d = mod_add_result;
                        else
                            t_coeff_d = mod_add_result;
                    end
                    state_d = StOpSel;
                end
            end

            // -----------------------------------------------------------------
            StSubRems: begin
                // Try u - v
                mod_add_valid    = 1'b1;
                mod_add_a        = u_rem_q;
                mod_add_b        = v_rem_q;
                mod_add_subtract = 1'b1;

                if (mod_add_ready) begin
                    if (!mod_add_adjust) begin
                        // No underflow: u >= v
                        u_rem_d       = mod_add_result;
                        reduced_unv_d = 1'b1;
                        state_d       = StSubCoeffs;
                    end else begin
                        // Underflow: u < v, need reverse subtraction
                        state_d = StSubRemsRev;
                    end
                end
            end

            // -----------------------------------------------------------------
            StSubRemsRev: begin
                // v - u (guaranteed no underflow)
                mod_add_valid    = 1'b1;
                mod_add_a        = v_rem_q;
                mod_add_b        = u_rem_q;
                mod_add_subtract = 1'b1;

                if (mod_add_ready) begin
                    v_rem_d       = mod_add_result;
                    reduced_unv_d = 1'b0;
                    state_d       = StSubCoeffs;
                end
            end

            // -----------------------------------------------------------------
            StSubCoeffs: begin
                // If u was reduced: s = s - t, else: t = t - s
                mod_add_valid    = 1'b1;
                mod_add_a        = reduced_unv_q ? s_coeff_q : t_coeff_q;
                mod_add_b        = reduced_unv_q ? t_coeff_q : s_coeff_q;
                mod_add_subtract = 1'b1;

                if (mod_add_ready) begin
                    if (reduced_unv_q)
                        s_coeff_d = mod_add_result;
                    else
                        t_coeff_d = mod_add_result;
                    state_d = StOpSel;
                end
            end

            // -----------------------------------------------------------------
            StDone: begin
                state_d = StIdle;
                ready   = 1'b1;
                exists  = (v_rem_q == 256'd1); // ignoring m = 0/1 cases (assuming large prime)
                result  = exists ? t_coeff_q : '0;
            end

            default: ; // empty — defaults are set outside the case statement
        endcase
    end

    // ---------------------------------------------------------------------------
    // Sequential: register updates, asynchronous active-low reset
    // ---------------------------------------------------------------------------

    // State register
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) state_q <= StIdle;
        else        state_q <= state_d;
    end

    // Remainders and coefficients registers
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            u_rem_q          <= '0;
            v_rem_q          <= '0;
            s_coeff_q        <= '0;
            t_coeff_q        <= '0;
        end else begin
            u_rem_q          <= u_rem_d;
            v_rem_q          <= v_rem_d;
            s_coeff_q        <= s_coeff_d;
            t_coeff_q        <= t_coeff_d;
        end
    end

    // Helper flags registers
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            reduced_unv_q    <= 1'b0;
            div2_unv_q       <= 1'b0;
            div2_coeff_odd_q <= 1'b0;
        end else begin
            reduced_unv_q    <= reduced_unv_d;
            div2_unv_q       <= div2_unv_d;
            div2_coeff_odd_q <= div2_coeff_odd_d;
        end
    end

endmodule
