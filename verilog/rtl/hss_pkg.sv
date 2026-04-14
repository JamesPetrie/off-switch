package hss_pkg;

    import arith_pkg::*;

    // -------------------------------------------------------------------------
    // RFC 8554 domain separation constants
    // -------------------------------------------------------------------------

    localparam logic [15:0] D_MESG = 16'h8181;
    localparam logic [15:0] D_PBLC = 16'h8080;
    localparam logic [15:0] D_LEAF = 16'h8282;
    localparam logic [15:0] D_INTR = 16'h8383;

    // -------------------------------------------------------------------------
    // WOTS+ parameters (w=8 - 1 chain is used for 8 bits)
    // -------------------------------------------------------------------------

    localparam int unsigned WOTS_W  = 8;            // Winternitz parameter
    localparam int unsigned WOTS_P1 = 32;           // 256 bits / 8 bits per chain
    localparam int unsigned WOTS_P2 = 2;            // checksum digits
    localparam int unsigned WOTS_P  = WOTS_P1 + WOTS_P2;

    localparam logic [WOTS_W-1:0] MAX_COEF = '1;

    // -------------------------------------------------------------------------
    // HSS/LMS configuration
    // -------------------------------------------------------------------------

    // Note: the design hardcodes L=1 (single Merkle tree), so the "levels" parameter is not adjustable
    /* verilator lint_off UNUSEDPARAM */
    localparam int unsigned HSS_LEVELS =  1;  // single Merkle tree (L=1)
    /* verilator lint_on UNUSEDPARAM */
    localparam int unsigned MAX_HEIGHT = 10;  // max height of the Merkle tree

    // -------------------------------------------------------------------------
    // Public key (tree identity + root hash)
    // -------------------------------------------------------------------------

    // Arbitrary identifier, should be randomly generated in production (for uniqueness)
    localparam logic [127:0]     IDENTIFIER   =
        128'h01010101_01010101_01010101_01010101;

    // Derived from the private key used in the testbench
    localparam logic [WIDTH-1:0] ROOT_PUB_KEY =
        256'h1c2e06bc6fe6bc3f7c6e7fc998277f82fd8fa2f79aa26eb1d3c66999083d4ce5;

    // Arbitrary value <= MAX_HEIGHT
    localparam int unsigned TREE_HEIGHT = 4;

    // -------------------------------------------------------------------------
    // HSS-LMS license format
    // -------------------------------------------------------------------------

    typedef struct packed {
        // Using packed arrays to comply with packed struct requirements
        //    Dimension 2     Dimension 1
        logic                      [31:0]  leaf_index;
        logic                 [WIDTH-1:0]  randomizer;
        logic [WOTS_P-1:0]    [WIDTH-1:0]  sig_chains;
        logic [MAX_HEIGHT-1:0][WIDTH-1:0]  auth_path;
    } license_t;

endpackage
