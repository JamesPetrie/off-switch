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
    input  logic [WIDTH-1:0] z,
    input  logic [WIDTH-1:0] r,
    input  logic [WIDTH-1:0] s,

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

    // Public key Q (derived from G and Private key d)
    localparam logic [WIDTH-1:0]
    Q_X = 256'hc6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5,
    Q_Y = 256'h1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a,
    Q_Z = 1;

    // Precomputed G + Q (assumed to be computed together with Q, so not implementing the addition here)
    localparam logic [WIDTH-1:0]
    GPQ_X = 256'hf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9,
    GPQ_Y = 256'h388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672,
    GPQ_Z = 1;

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
    localparam int BLANK_START     = FINALIZE_END    + 1;

    // Arrays to collect the PC values
    localparam int PROGRAM_STARTS [4] = '{PREPARE_START, POINT_ADD_START, FINALIZE_START, BLANK_START};
    localparam int PROGRAM_ENDS   [3] = '{PREPARE_END,   POINT_ADD_END,   FINALIZE_END};

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
    // Register file RAM
    // -------------------------------------------------------------------------


    // Register file, Two-port (2 read, 1 write) RAM
    (* ram_style = "block" *) logic [WIDTH-1:0] reg_file [NUM_REGS];
    // Port A
    reg_addr_t        addr_a;
    logic             we_a;
    logic [WIDTH-1:0] wdata_a;
    logic [WIDTH-1:0] rdata_a;
    // Port B (using port B for single reads when possible)
    reg_addr_t        addr_b;
    logic [WIDTH-1:0] rdata_b;

    // Port A: read src1 / write dst
    always_ff @(posedge clk) begin
        if (we_a)
            reg_file[addr_a] <= wdata_a;
        rdata_a <= reg_file[addr_a];
    end

    // Port B: read src2 (read-only)
    always_ff @(posedge clk) begin
        rdata_b <= reg_file[addr_b];
    end

    // -------------------------------------------------------------------------
    // Registers
    // -------------------------------------------------------------------------

    // FSM state
    state_e state_q, state_d;

    // Register file handling helper registers
    typedef enum logic [2:0] {
        // using encoding such that cnt[idx] shows if the given coordinate is due (0) or done (1)
        DO_X = 3'b000, // bit 2 = x, if all 3 is due, start with X
        DO_Y = 3'b100, // bit 1 = y, if X is done, do Y next
        DO_Z = 3'b110, // bit 0 = z, do Z last
        DONE = 3'b111
    } xyz_cnt_t;

    logic     rdata_phase_q, rdata_phase_d;
    // Lint: two functions (reg_write, reg_copy) in the same always_comb drive these
    /* verilator lint_off MULTIDRIVEN */
    xyz_cnt_t xyz_cnt_q    , xyz_cnt_d;
    /* verilator lint_on MULTIDRIVEN */

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

    // Address A1 and B3 has static parameters, add a mux to use those intead of RAM rdata when selected
    logic [WIDTH-1:0] op_a;
    logic [WIDTH-1:0] op_b;

    assign op_a = current_instr.src1 == A1 ? CURVE_A1 :
                  current_instr.src1 == B3 ? CURVE_B3 :
                                             rdata_a;

    assign op_b = current_instr.src2 == A1 ? CURVE_A1 :
                  current_instr.src2 == B3 ? CURVE_B3 :
                                             rdata_b;

    function automatic void reg_write(input all_addr_t addr, input logic [WIDTH-1:0] val);
        addr_a  = reg_addr_t'(addr);
        we_a    = 1'b1;
        wdata_a = val;
    endfunction

    function automatic xyz_cnt_t get_xyz_condition(input all_addr_t addr);
        // based on the address, which counter value the register operation should activate on
        case (addr)
            // reused for T2/1/0 as well (using xyz = 210 so x/idx[2] corresponts to T2)
            X1, X2, X3, T2: return DO_X;
            Y1, Y2, Y3, T1: return DO_Y;
            Z1, Z2, Z3, T0: return DO_Z;
            default:        return DONE;
        endcase
    endfunction

    function automatic void xyz_cnt_next(logic increment);
        xyz_cnt_d = increment ? xyz_cnt_q.next() : xyz_cnt_q;
    endfunction

    function automatic void xyz_reg_write(input all_addr_t addr, input logic [WIDTH-1:0] val);
        xyz_cnt_t xyz_condition = get_xyz_condition(addr);

        // activate the write and increment the counter on the active value and it's not "all done"
        if (xyz_cnt_q == xyz_condition && xyz_cnt_q != DONE) begin
            reg_write(addr, val);
            xyz_cnt_next(.increment(1'b1));
        end
    endfunction

    function automatic void xyz_reg_copy(input all_addr_t to, input all_addr_t from);
        xyz_cnt_t xyz_condition = get_xyz_condition(to);

        // activate the copy when at the active value and it's not "all done"
        if (xyz_cnt_q == xyz_condition && xyz_cnt_q != DONE) begin
            // Note: Using Port A to write, Port B to read, but not overlapping them
            //       Write happens in the read's data phase
            if (!rdata_phase_q) begin
                addr_b        = reg_addr_t'(from);
                rdata_phase_d = 1'b1;
                // ensure that counter value is maintained this cycle (it would reset to 0 by default!)
                xyz_cnt_next(.increment(1'b0));
            end else begin
                reg_write(to, rdata_b);
                xyz_cnt_next(.increment(1'b1));
            end
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
        .a         (op_a),
        .b         (op_b),
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
            2'b00:   begin sel_x = INF_X; sel_y = INF_Y; sel_z = INF_Z; end
            2'b01:   begin sel_x = G_X;   sel_y = G_Y;   sel_z = G_Z;   end
            2'b10:   begin sel_x = Q_X;   sel_y = Q_Y;   sel_z = Q_Z;   end
            2'b11:   begin sel_x = GPQ_X; sel_y = GPQ_Y; sel_z = GPQ_Z; end
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

        // Register file addresses, write enable and write data
        // Do instruction operand reads by default
        addr_a  = reg_addr_t'(current_instr.src1);
        addr_b  = reg_addr_t'(current_instr.src2);
        we_a    = 1'b0;
        wdata_a = '0;

        // Register file helpers
        rdata_phase_d = 1'b0;   // address phase by default
        xyz_cnt_d     = DO_X; // default state

        // Handle running the program here centrally for all states
        // Starting the program is the responsibility of the individual FSM states,
        // mid-program handling and stopping happens here

        // Store result when arith ready
        if (arith_ready) begin
            addr_a  = reg_addr_t'(current_instr.dst);
            we_a    = 1'b1;
            wdata_a = arith_result;

            // Stop and wait for the next instruction to be loaded
            arith_valid_d = 1'b0;
        end

        // Load and auto-start next instruction when not at a program boundary
        if ( !(int'(pc_q) inside {PROGRAM_STARTS}) ) begin // assuming programs are tightly packed in the ROM
            if (!arith_valid_q) begin
                if(rdata_phase_q == 1'b0) begin
                    rdata_phase_d = 1'b1;
                    arith_valid_d = 1'b1; // arith_valid should be asserted together with data phase
                end
            end
        end

        // State machine
        unique case (state_q)
            // -----------------------------------------------------------------
            StIdle: begin
                if (valid) begin
                    // Initialize P1 accumulator to point infinity then move to next state
                    // Note: counter based write coordinate selection and counter increment are hidden in xyz_reg_write
                    xyz_reg_write(X1, INF_X);
                    xyz_reg_write(Y1, INF_Y);
                    xyz_reg_write(Z1, INF_Z);

                    // Move to next state when all of x, y, and z are initialized
                    if (xyz_cnt_q == DONE) begin
                        state_d = StPrepare;
                    end
                end
            end

            // -----------------------------------------------------------------
            StPrepare: begin

                // PC loading handled in separate always_comb

                // If program not started yet, load the inputs and start the program
                if (!arith_valid_q && int'(pc_q) == PREPARE_START) begin

                    // Note: counter based write coordinate selection and counter increment are hidden in xyz_reg_write
                    // Reusing xyz_reg_write for T2/1/0 a well
                    xyz_reg_write(T0, s);
                    xyz_reg_write(T1, z);
                    xyz_reg_write(T2, r);

                    // Start only when all of x, y, and z are initialized
                    if (xyz_cnt_q == DONE) begin
                        arith_valid_d = 1'b1;
                    end
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, store the u1, u2 results, initialize loop counter and move to next state
                if (!arith_valid_q && int'(pc_q) == PREPARE_END+1) begin
                    // Split into address and data phase
                    if (!rdata_phase_q) begin
                        // Address phase

                        addr_a = reg_addr_t'(T1);
                        addr_b = reg_addr_t'(T2);
                        rdata_phase_d = 1'b1;
                    end else begin
                        // Data phase

                        u1_d      = rdata_a;
                        u2_d      = rdata_b;

                        bit_pos_d = BITCNT_W'(WIDTH-1);

                        state_d = StAdd;
                    end
                end
            end

            // -----------------------------------------------------------------
            StAdd: begin

                // PC loading handled in separate always_comb

                // If program not started yet, load the inputs and start the program
                if (!arith_valid_q && int'(pc_q) == POINT_ADD_START) begin
                    // P2 = selected_point (for P1 += P2)
                    // Note: counter based write coordinate selection and counter increment are hidden in xyz_reg_write
                    xyz_reg_write(X2, sel_x);
                    xyz_reg_write(Y2, sel_y);
                    xyz_reg_write(Z2, sel_z);

                    // Start only when all of x, y, and z are initialized
                    if (xyz_cnt_q == DONE) begin
                        arith_valid_d = 1'b1;
                    end
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, move to next state (results already in the P1 accumulator)
                if (!arith_valid_q && int'(pc_q) == POINT_ADD_END+1) begin
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
                    // Note: counter based coordinate selection and counter increment are hidden in xyz_reg_copy
                    xyz_reg_copy(X2, X1);
                    xyz_reg_copy(Y2, Y1);
                    xyz_reg_copy(Z2, Z1);

                    // Start only when all of x, y, and z are loaded
                    if (xyz_cnt_q == 3'b111) begin
                        arith_valid_d = 1'b1;
                    end
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, move back to add state and decrement bit counter (results already in the P1 accumulator)
                if (!arith_valid_q && int'(pc_q) == POINT_ADD_END+1) begin
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
                    xyz_reg_write(T2, r); // Reusing xyz_reg_write for T2/1/0 a well

                    // Start only when T2 is loaded
                    if (xyz_cnt_q[2]) begin
                        arith_valid_d = 1'b1;
                    end
                end

                // Nothing to do here when the program running, it's handled outside the case statement

                // When program finished, check the result and move back to idle
                if (!arith_valid_q && int'(pc_q) == FINALIZE_END+1) begin
                    // Read is split into address and data phase
                    if (!rdata_phase_q) begin
                        // Address phase

                        addr_b = reg_addr_t'(T0);
                        rdata_phase_d = 1'b1;
                    end else begin
                        // Data phase

                        ready        = 1'b1;
                        verif_passed = (rdata_b == '0);

                        state_d = StIdle;
                    end
                end
            end

            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // Sequential: register updates, asynchronous active-low reset
    // -------------------------------------------------------------------------

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

    // register file helpers
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rdata_phase_q <= 1'b0;
            xyz_cnt_q     <= DO_X;
        end else begin
            rdata_phase_q <= rdata_phase_d;
            xyz_cnt_q     <= xyz_cnt_d;
        end
    end


endmodule
