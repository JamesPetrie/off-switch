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
    // RFC 8554 typecodes
    // -------------------------------------------------------------------------

    // RFC 8554 §5.1 LMS / LM-OTS typecodes  — embedded in inter-layer signed pub.
    // Values only need to agree between signer and verifier; RFC registry values
    // are used as reasonable defaults.

    typedef enum logic[31:0] {
        LMOTS_RESERVED       = 0,
        LMOTS_SHA256_N32_W1  = 1,
        LMOTS_SHA256_N32_W2  = 2,
        LMOTS_SHA256_N32_W4  = 3,
        LMOTS_SHA256_N32_W8  = 4
    } lmots_algorithm_t;

    typedef enum logic[31:0] {
        LMS_RESERVED       = 0,
        LMS_SHA256_N32_H5  = 5,
        LMS_SHA256_N32_H10 = 6,
        LMS_SHA256_N32_H15 = 7,
        LMS_SHA256_N32_H20 = 8,
        LMS_SHA256_N32_H25 = 9
    } lms_algorithm_t;

    // -------------------------------------------------------------------------
    // Fixed parameters for simplification
    // -------------------------------------------------------------------------

    // Note: w could vary for each layer of the tree stack according to the standard.
    //       But implementation fixes it for simplicity.
    localparam int unsigned WOTS_W      = 8;                     // Winternitz parameter
    localparam logic [31:0] LMOTS_TYPE  = LMOTS_SHA256_N32_W8;   // LMOTS algorithm identifier
    localparam int unsigned WOTS_P1     = 32;                    // data digits per chain
    localparam int unsigned WOTS_P2     = 2;                     // checksum digits per chain


    // Note: h could vary for each layer of the tree stack according to the standard.
    //       But implementation fixes it for simplicity.
    localparam int unsigned TREE_H      = 5;
    localparam logic [31:0] LMS_TYPE    = LMS_SHA256_N32_H5;    // LMS algorithm identifier
    localparam int unsigned TREE_H_MAX  = 25;                   // max of lms_algorithm_type

    // -------------------------------------------------------------------------
    // Public key (HSS layers, top-tree identity, and root hash)
    // -------------------------------------------------------------------------

    // Number of stacked tree layers
    localparam int unsigned HSS_LEVELS  = 2;

    // Arbitrary identifier for the TOP tree; lower-tree identifiers ride in
    // the license as sub_I[1..HSS_LEVELS-1].
    localparam logic [127:0]     TOP_IDENTIFIER =
        128'h01010101_01010101_01010101_01010101;

    // Derived from the private key used in the testbench (top-tree root T[1])
    localparam logic [WIDTH-1:0] ROOT_PUB_KEY =
        256'h0d5d8e8b6c4eb1f474f02048e9cbecbeb66eb10fd307ea7d86f5d485b2e9b45f;

    // -------------------------------------------------------------------------
    // Derived parameters
    // -------------------------------------------------------------------------

    // layer counter width
    localparam int unsigned LAYER_CNT_W = (HSS_LEVELS>1) ? $clog2(HSS_LEVELS) : 1;

    // total digits per chain
    localparam int unsigned WOTS_P = WOTS_P1 + WOTS_P2;

    // WOTS digit maximum value (all 1s)
    localparam logic [WOTS_W-1:0] WOTS_MAX_COEF = '1;

    // -------------------------------------------------------------------------
    // HSS-LMS license format
    // -------------------------------------------------------------------------

    typedef struct packed {
        // Per-layer OTS + Merkle material. lv=0 is the top tree; lv=HSS_LEVELS-1
        // is the leaf tree that signs the user message.
        //    Dimension 3     Dimension 2     Dimension 1
        logic [HSS_LEVELS-1:0]                [31:0]       leaf_index;
        logic [HSS_LEVELS-1:0]                [WIDTH-1:0]  randomizer;
        logic [HSS_LEVELS-1:0][WOTS_P-1:0]    [WIDTH-1:0]  sig_chains;
        logic [HSS_LEVELS-1:0][TREE_H_MAX-1:0][WIDTH-1:0]  auth_path;
        // Subtree identifiers. sub_I[0] is unused
        // (the top tree uses TOP_IDENTIFIER)
        logic [HSS_LEVELS-1:0]                [127:0]      sub_I;
    } license_t;

endpackage
