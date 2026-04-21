// ECDSA - Signature verification for secp256k1
//
// Verifies ECDSA signatures using:
//   R = u1*G + u2*Q
// where:
//   u1 = z * s^(-1) mod n
//   u2 = r * s^(-1) mod n
//
// Signature is valid if R.x mod n == r
//
// Uses Renes-Costello-Batina complete addition formula in projective coordinates.
// Uses Shamir's trick for simultaneous scalar multiplication (processes u1/u2
// bits in parallel, selecting G/Q/G+Q/infinity per iteration).
//
// Hardcoded: G (generator), Q = 2G (public key), G+Q = 3G (precomputed sum)
//
// Protocol:
//   1. Assert valid and hold z, r, s stable until ready pulses
//   2. ready pulses high for one cycle when verification completes
//   3. When ready, check verif_passed: 1 = signature verification passed, 0 = signature verification failed
//
// FSM:
//
//   StIdle -> StPrepare -> StAdd -> StDouble -> StAdd -> StFinalize -> StIdle
//                                      ^          |
//                                      |__________|
//
// Note: could do the same skip StAdd optimization as in mod_mul but
// PC loading does not currently support re-running the same state (StDouble after StDouble)


module ecdsa
    import arith_pkg::*; // import in module header to be used in port list
    import ecdsa_pkg::*;
(
    input  logic             clk,
    input  logic             rst_n,
    input  logic             valid,
    input  logic [WIDTH-1:0] z,     // message
    input  logic [WIDTH-1:0] r,     // signature r
    input  logic [WIDTH-1:0] s,     // signature s
    // z coordinates are fixed 1 on affine -> projective so skip those here
    input  logic [WIDTH-1:0] q_x,   // public key Q_X
    input  logic [WIDTH-1:0] q_y,   // public key Q_Y
    input  logic [WIDTH-1:0] gpq_x, // G + Q point X coordinate, precomputed
    input  logic [WIDTH-1:0] gpq_y, // G + Q point Y coordinate, precomputed

    output logic             ready,
    output logic             verif_passed
);

    // -------------------------------------------------------------------------
    // Types and Constants
    // -------------------------------------------------------------------------

    typedef logic [4:0] all_addr_t;

    // Register file indices
    typedef enum all_addr_t {
        T0, T1, T2, T3, T4, T5,
        X3, Y3, Z3,
        X1, Y1, Z1,
        X2, Y2, Z2,
        A1, B3,   // constants, not actual registers
        NUM_ADDRS // last element to contain the total number of addresses
    } all_addr_e;

    localparam int NUM_CONSTS = 2;
    localparam int NUM_REGS   = int'(NUM_ADDRS) - NUM_CONSTS; // number of actual registers

    typedef logic [$clog2(NUM_REGS)-1:0] reg_addr_t;

    localparam int BITCNT_W = $clog2(WIDTH); // Bit Counter Width

    // Point at infinity (z = 0)
    localparam logic [WIDTH-1:0]
    INF_X = 0,
    INF_Y = 1,
    INF_Z = 0;

    // -------------------------------------------------------------------------
    // Instruction ROM
    // -------------------------------------------------------------------------

    typedef struct packed {
        op_e        op;
        all_addr_t  src1;
        all_addr_t  src2;
        all_addr_t  dst;
        // Note: dst can only be register (not constant) so reg_addr_t could also work,
        //       but the reg enums are using all_addr_t, so using that avoids casting
    } instr_t;

    // Segment lengths and PC width
    // Note: tried assigning the programs to separate arrays to qurry lengths
    //       but verilator had issues with concatenating those
    localparam int PREPARE_LEN   = 3;
    localparam int POINT_ADD_LEN = 40;
    localparam int FINALIZE_LEN  = 3;
    localparam int ROM_SIZE      = PREPARE_LEN + POINT_ADD_LEN + FINALIZE_LEN;
    localparam int PC_WIDTH      = $clog2(ROM_SIZE);

    typedef logic [PC_WIDTH-1:0] pc_t;

    localparam instr_t PROGRAM [ROM_SIZE] = '{

        // --- Prepare (mod n) ---
        // w = s^(-1) mod n;  u1 = z*w mod n;  u2 = r*w mod n
        // Assumes t0=s, t1=z, t2=r
        /* 1 */ '{op: OP_INV, src1: T0, src2: T0, dst: T0},   // t0 = inv(t0)
        /* 2 */ '{op: OP_MUL, src1: T1, src2: T0, dst: T1},   // t1 = t1 * t0
        /* 3 */ '{op: OP_MUL, src1: T2, src2: T0, dst: T2},   // t2 = t2 * t0

        // --- Point addition (Renes-Costello-Batina, 40 steps) ---
        /*  1 */ '{op: OP_MUL, src1: X1, src2: X2, dst: T0},   // t0 = x1*x2
        /*  2 */ '{op: OP_MUL, src1: Y1, src2: Y2, dst: T1},   // t1 = y1*y2
        /*  3 */ '{op: OP_MUL, src1: Z1, src2: Z2, dst: T2},   // t2 = z1*z2
        /*  4 */ '{op: OP_ADD, src1: X1, src2: Y1, dst: T3},   // t3 = x1+y1
        /*  5 */ '{op: OP_ADD, src1: X2, src2: Y2, dst: T4},   // t4 = x2+y2
        /*  6 */ '{op: OP_MUL, src1: T3, src2: T4, dst: T3},   // t3 = t3*t4
        /*  7 */ '{op: OP_ADD, src1: T0, src2: T1, dst: T4},   // t4 = t0+t1
        /*  8 */ '{op: OP_SUB, src1: T3, src2: T4, dst: T3},   // t3 = t3-t4
        /*  9 */ '{op: OP_ADD, src1: X1, src2: Z1, dst: T4},   // t4 = x1+z1
        /* 10 */ '{op: OP_ADD, src1: X2, src2: Z2, dst: T5},   // t5 = x2+z2
        /* 11 */ '{op: OP_MUL, src1: T4, src2: T5, dst: T4},   // t4 = t4*t5
        /* 12 */ '{op: OP_ADD, src1: T0, src2: T2, dst: T5},   // t5 = t0+t2
        /* 13 */ '{op: OP_SUB, src1: T4, src2: T5, dst: T4},   // t4 = t4-t5
        /* 14 */ '{op: OP_ADD, src1: Y1, src2: Z1, dst: T5},   // t5 = y1+z1
        /* 15 */ '{op: OP_ADD, src1: Y2, src2: Z2, dst: X3},   // x3 = y2+z2
        /* 16 */ '{op: OP_MUL, src1: T5, src2: X3, dst: T5},   // t5 = t5*x3
        /* 17 */ '{op: OP_ADD, src1: T1, src2: T2, dst: X3},   // x3 = t1+t2
        /* 18 */ '{op: OP_SUB, src1: T5, src2: X3, dst: T5},   // t5 = t5-x3
        /* 19 */ '{op: OP_MUL, src1: A1, src2: T4, dst: Z3},   // z3 = a1*t4
        /* 20 */ '{op: OP_MUL, src1: B3, src2: T2, dst: X3},   // x3 = b3*t2
        /* 21 */ '{op: OP_ADD, src1: X3, src2: Z3, dst: Z3},   // z3 = x3+z3
        /* 22 */ '{op: OP_SUB, src1: T1, src2: Z3, dst: X3},   // x3 = t1-z3
        /* 23 */ '{op: OP_ADD, src1: T1, src2: Z3, dst: Z3},   // z3 = t1+z3
        /* 24 */ '{op: OP_MUL, src1: X3, src2: Z3, dst: Y3},   // y3 = x3*z3
        /* 25 */ '{op: OP_ADD, src1: T0, src2: T0, dst: T1},   // t1 = t0+t0
        /* 26 */ '{op: OP_ADD, src1: T1, src2: T0, dst: T1},   // t1 = t1+t0
        /* 27 */ '{op: OP_MUL, src1: A1, src2: T2, dst: T2},   // t2 = a1*t2
        /* 28 */ '{op: OP_MUL, src1: B3, src2: T4, dst: T4},   // t4 = b3*t4
        /* 29 */ '{op: OP_ADD, src1: T1, src2: T2, dst: T1},   // t1 = t1+t2
        /* 30 */ '{op: OP_SUB, src1: T0, src2: T2, dst: T2},   // t2 = t0-t2
        /* 31 */ '{op: OP_MUL, src1: A1, src2: T2, dst: T2},   // t2 = a1*t2
        /* 32 */ '{op: OP_ADD, src1: T4, src2: T2, dst: T4},   // t4 = t4+t2
        /* 33 */ '{op: OP_MUL, src1: T1, src2: T4, dst: T0},   // t0 = t1*t4
        /* 34 */ '{op: OP_ADD, src1: Y3, src2: T0, dst: Y1},   // y1 = y3+t0
        /* 35 */ '{op: OP_MUL, src1: T5, src2: T4, dst: T0},   // t0 = t5*t4
        /* 36 */ '{op: OP_MUL, src1: T3, src2: X3, dst: X3},   // x3 = t3*x3
        /* 37 */ '{op: OP_SUB, src1: X3, src2: T0, dst: X1},   // x1 = x3-t0
        /* 38 */ '{op: OP_MUL, src1: T3, src2: T1, dst: T0},   // t0 = t3*t1
        /* 39 */ '{op: OP_MUL, src1: T5, src2: Z3, dst: Z3},   // z3 = t5*z3
        /* 40 */ '{op: OP_ADD, src1: Z3, src2: T0, dst: Z1},   // z1 = z3+t0

        // --- Finalize (mod p) ---
        // z_inv = z1^(-1) mod p;  x_affine = x1*z_inv;  result = x_affine - r
        // Assumes t2=r (restored from r input before entering finalize)
        /* 1 */ '{op: OP_INV, src1: Z1, src2: Z1, dst: T0},   // t0 = inv(z1)
        /* 2 */ '{op: OP_MUL, src1: X1, src2: T0, dst: T0},   // t0 = x1*t0
        /* 3 */ '{op: OP_SUB, src1: T0, src2: T2, dst: T0}    // t0 = t0-t2
    };

    // Segment boundaries
    localparam int ROM_START       = 0;
    localparam int PREPARE_START   = ROM_START;
    localparam int PREPARE_END     = PREPARE_START   + PREPARE_LEN   - 1;
    localparam int POINT_ADD_START = PREPARE_END     + 1;
    localparam int POINT_ADD_END   = POINT_ADD_START + POINT_ADD_LEN - 1;
    localparam int FINALIZE_START  = POINT_ADD_END   + 1;
    localparam int FINALIZE_END    = FINALIZE_START  + FINALIZE_LEN  - 1;

    // Array to collect the PC values where execution should automatically stop
    localparam int PROGRAM_ENDS [3] = '{PREPARE_END, POINT_ADD_END, FINALIZE_END};

    // -------------------------------------------------------------------------
    // FSM states
    // -------------------------------------------------------------------------

    typedef enum logic [2:0] {
        StIdle,
        StPrepare,
        StAdd,
        StDouble,
        StFinalize
    } state_e;

    // -------------------------------------------------------------------------
    // Registers
    // -------------------------------------------------------------------------

    // FSM state
    state_e state_q, state_d;

    // Register file
    logic [WIDTH-1:0] reg_file_q [NUM_REGS];
    logic [WIDTH-1:0] reg_file_d [NUM_REGS];

    // Other registers
    pc_t                  pc_q,       pc_d;
    logic [WIDTH-1:0]     u1_q,       u1_d;
    logic [WIDTH-1:0]     u2_q,       u2_d;
    logic [BITCNT_W-1:0]  bit_pos_q,  bit_pos_d;

    // -------------------------------------------------------------------------
    // Instruction decode
    // -------------------------------------------------------------------------

    instr_t current_instr;
    assign current_instr = PROGRAM[pc_q];

    // only Prepare requires PRIME_N
    wire [WIDTH-1:0] modulus = (int'(pc_q) <= PREPARE_END) ? PRIME_N : PRIME_P;

    // -------------------------------------------------------------------------
    // Register file access helpers
    // -------------------------------------------------------------------------

    function automatic logic [WIDTH-1:0] reg_read(input all_addr_t addr);
        case (addr)
            // A1 and B3 are constants, not part of the actual register file
            A1      : return CURVE_A1;
            B3      : return CURVE_B3;
            // casting might be needed if the actual register file array requires less bit(s) for indexing
            default : return reg_file_q[reg_addr_t'(addr)];
        endcase
    endfunction

    function automatic void reg_write(input all_addr_t addr, input logic [WIDTH-1:0] val);
        // Making it explicit to lint that discarding MSB is fine when the widths differ
        // (The addresses of the constants should not be used for reg_write)
        if ( addr[$size(all_addr_t)-1] ||
            !addr[$size(all_addr_t)-1]) begin

            // casting might be needed if the actual register file array requires less bit(s) for indexing
            reg_file_d[reg_addr_t'(addr)] = val;
        end
    endfunction

    // -------------------------------------------------------------------------
    // Arith instance
    // -------------------------------------------------------------------------

    // arith block enable register
    logic arith_valid_q, arith_valid_d;

    // Outputs, used in FSM always_comb
    logic             arith_ready;   // used to increment the PC and sample arith_result
    logic [WIDTH-1:0] arith_result;  // stored in current_instr.dst register

    arith u_arith (
        .clk       (clk),
        .rst_n     (rst_n),
        .valid     (arith_valid_q),
        .op        (current_instr.op),
        .a         (reg_read(current_instr.src1)),
        .b         (reg_read(current_instr.src2)),
        .modulus   (modulus),
        .ready     (arith_ready),
        .result    (arith_result)
    );

    // -------------------------------------------------------------------------
    // Shamir's trick point selection
    // -------------------------------------------------------------------------

    logic [WIDTH-1:0] sel_x, sel_y, sel_z;

    always_comb begin
        // REVISIT - shift register approach to access u1 and u2 bits could be much less gates
        unique case ({u2_q[bit_pos_q], u1_q[bit_pos_q]})
            2'b00:   begin sel_x = INF_X; sel_y = INF_Y; sel_z = INF_Z;     end
            2'b01:   begin sel_x = G_X;   sel_y = G_Y;   sel_z = G_Z;       end
            2'b10:   begin sel_x = q_x;   sel_y = q_y;   sel_z = AFFINE_Z;  end
            2'b11:   begin sel_x = gpq_x; sel_y = gpq_y; sel_z = AFFINE_Z;  end
            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // PC — combinational next-state
    // -------------------------------------------------------------------------
    always_comb begin
        // hold by default
        pc_d           = pc_q;

        if (arith_ready) begin
            // Increment whenever arithmetic block ready
            pc_d = pc_q + 1;
        end else if (state_d != state_q) begin
            // Load new value when FSM state changes (should not coincide with arith_ready)
            case (state_d)
                StPrepare:  pc_d = pc_t'(PREPARE_START);
                StAdd:      pc_d = pc_t'(POINT_ADD_START);
                StDouble:   pc_d = pc_t'(POINT_ADD_START);
                StFinalize: pc_d = pc_t'(FINALIZE_START);
                default: ; // no need to load for the other states
            endcase
        end
    end

    // -------------------------------------------------------------------------
    // FSM — combinational next-state and data path
    // -------------------------------------------------------------------------

    always_comb begin
        // Outputs (inactive by default)
        ready        = 1'b0;
        verif_passed = 1'b0;

        // Simple registers (hold by default)
        state_d        = state_q;
        u1_d           = u1_q;
        u2_d           = u2_q;
        bit_pos_d      = bit_pos_q;
        arith_valid_d  = arith_valid_q;

        foreach (reg_file_d[i]) begin
            reg_file_d[i] = reg_file_q[i];
        end

        // Handle running the program here centrally for all states
        if (arith_ready) begin

            // When arith block ready, store result
            reg_write(current_instr.dst, arith_result);

            // If end of program reached, stop the program
            if (int'(pc_q) inside {PROGRAM_ENDS}) begin
                arith_valid_d = 1'b0;
            end
        end

        // State machine
        unique case (state_q)
            // -----------------------------------------------------------------
            StIdle: begin
                if (valid) begin
                    // Initialize P1 accumulator to point infinity
                    reg_write(X1, INF_X);
                    reg_write(Y1, INF_Y);
                    reg_write(Z1, INF_Z);

                    // Move to next state
                    state_d = StPrepare;
                end
            end

            // -----------------------------------------------------------------
            StPrepare: begin

                // PC loading handled in separate always_comb

                // If program not started yet, load the inputs and start the program
                if (!arith_valid_q && int'(pc_q) == PREPARE_START) begin

                    reg_write(T0, s);
                    reg_write(T1, z);
                    reg_write(T2, r);

                    arith_valid_d = 1'b1;
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, store the u1, u2 results, initialize loop counter and move to next state
                if (!arith_valid_q && int'(pc_q) != PREPARE_START) begin
                    u1_d      = reg_read(T1);
                    u2_d      = reg_read(T2);
                    bit_pos_d = BITCNT_W'(WIDTH-1);

                    state_d = StAdd;
                end
            end

            // -----------------------------------------------------------------
            StAdd: begin

                // PC loading handled in separate always_comb

                // If program not started yet, load the inputs and start the program
                if (!arith_valid_q && int'(pc_q) == POINT_ADD_START) begin
                    // P2 = selected_point (for P1 += P2)
                    reg_write(X2, sel_x);
                    reg_write(Y2, sel_y);
                    reg_write(Z2, sel_z);

                    arith_valid_d = 1'b1;
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, move to next state (results already in the P1 accumulator)
                if (!arith_valid_q && int'(pc_q) != POINT_ADD_START) begin
                    // Stop condition: last bit (doubling not needed then)
                    state_d = (bit_pos_q != '0) ? StDouble : StFinalize;
                end
            end

            // -----------------------------------------------------------------
            StDouble: begin

                // PC loading handled in separate always_comb

                // If program not started yet, load the inputs and start the program
                if (!arith_valid_q && int'(pc_q) == POINT_ADD_START) begin
                    // P2 = P1 (for P1 + P2 = 2*P1)
                    reg_write(X2, reg_read(X1));
                    reg_write(Y2, reg_read(Y1));
                    reg_write(Z2, reg_read(Z1));

                    arith_valid_d = 1'b1;
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, move back to add state and decrement bit counter (results already in the P1 accumulator)
                if (!arith_valid_q && int'(pc_q) != POINT_ADD_START) begin
                    bit_pos_d = bit_pos_q - 1;
                    // Note: could do the same skip StAdd optimization as in mod_mul but
                    // PC loading does not currently support re-running the same state (StDouble after StDouble)
                    state_d = StAdd;
                end
            end

            // -----------------------------------------------------------------
            StFinalize: begin

                // PC loading handled in separate always_comb

                // If program not started yet, load the inputs and start the program
                if (!arith_valid_q && int'(pc_q) == FINALIZE_START) begin
                    // X1, Y1, Z1 are already in the corresponding registers
                    reg_write(T2, r);

                    arith_valid_d = 1'b1;
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, check the result and move back to idle
                if (!arith_valid_q && int'(pc_q) != FINALIZE_START) begin
                    ready        = 1'b1;
                    verif_passed = (reg_read(T0) == '0);

                    state_d = StIdle;
                end
            end

            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // Sequential: register updates, asynchronous active-low reset
    // -------------------------------------------------------------------------

    // Register file registers
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 0; i < NUM_REGS; i++) begin
                reg_file_q[i] <= '0;
            end
        end else begin
            for (int i = 0; i < NUM_REGS; i++) begin
                reg_file_q[i] <= reg_file_d[i];
            end
        end
    end

    // FSM state register
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) state_q <= StIdle;
        else        state_q <= state_d;
    end

    // PC register
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pc_q <= '0;
        else        pc_q <= pc_d;
    end

    // arith_valid register
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) arith_valid_q <= 1'b0;
        else        arith_valid_q <= arith_valid_d;
    end

    // u1, u2, and bit_pos registers
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            u1_q           <= '0;
            u2_q           <= '0;
            bit_pos_q      <= '0;
        end else begin
            u1_q           <= u1_d;
            u2_q           <= u2_d;
            bit_pos_q      <= bit_pos_d;
        end
    end

endmodule
