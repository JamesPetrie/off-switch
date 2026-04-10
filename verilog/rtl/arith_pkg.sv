package arith_pkg;

    parameter int unsigned WIDTH = 256;

    typedef enum logic [1:0] {
        OP_ADD, // modular addition:        a + b mod p
        OP_SUB, // modular subtraction:     a - b mod p
        OP_MUL, // modular multiplication:  a * b mod p
        OP_INV  // modular inverse:         a^-1  mod p (b ignored)
    } op_e;

endpackage
