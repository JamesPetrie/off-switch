package arith_pkg;

    parameter int unsigned WIDTH = 256;

    typedef enum logic [1:0] {
        OP_ADD = 2'd0,
        OP_SUB = 2'd1,
        OP_MUL = 2'd2,
        OP_INV = 2'd3
    } op_e;

endpackage
