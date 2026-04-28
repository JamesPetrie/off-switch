package sphincs_pkg;

    localparam int unsigned WIDTH = 128;   // security level in bits

    // -------------------------------------------------------------------------
    // TODO 128f?
    // SPHINCS+-256f FORS parameters
    // -------------------------------------------------------------------------

    localparam int unsigned FORS_K          = 35;
    localparam int unsigned FORS_A          = 9;
    localparam int unsigned FORS_TREE_CNT_W = $clog2(FORS_K);       // 6
    localparam int unsigned FORS_LVL_W      = $clog2(FORS_A + 1);   // 4
    localparam int unsigned FORS_NODE_W     = FORS_A + 1;           // 10 (heap idx)

    // -------------------------------------------------------------------------
    // Public key format and values
    // -------------------------------------------------------------------------
    typedef struct packed {
        logic [WIDTH-1:0] seed;   // generator seed
        logic [WIDTH-1:0] root;   // top-tree root
    } sphincs_pk_t;

    localparam sphincs_pk_t TEST_PK = '{seed: 128'h01010101_01010101_01010101_01010101,
                                        root: 128'h0}; // TODO update when generated

    // -------------------------------------------------------------------------
    // FORS license format: per-tree sk element + auth path. The K leaf
    // indices into the FORS trees are derived from the message Q hash, not
    // carried in the signature.
    // -------------------------------------------------------------------------

    typedef struct packed {
        // Message hash
        logic [WIDTH-1:0] r;   // per-signature randomness
        // FORS
        //    Dimension 3 Dimension 2 Dimension 1
        logic [FORS_K-1:0]            [WIDTH-1:0]  fors_sk;
        logic [FORS_K-1:0][FORS_A-1:0][WIDTH-1:0]  fors_auth;
    } sphincs_lic_t;

    // -------------------------------------------------------------------------
    // SHA256 message formats
    // -------------------------------------------------------------------------

    // ADRS-type tags used by FORS hashing.
    // NOTE: this is a flat substitute, NOT the canonical 22-/32-byte ADRS.
    // Test-only deviation; revisit when SPHINCS+ interop becomes a goal.
    localparam logic [31:0] ADRS_FORS_TREE  = 32'd3;
    localparam logic [31:0] ADRS_FORS_ROOTS = 32'd4;

    // H_msg: MGF1-SHA-256(𝑅 ∥ PK.seed ∥ SHA-256(𝑅 ∥ PK.seed ∥ PK.root ∥ 𝑀 ), 𝑚)
    typedef struct packed {
        logic [WIDTH-1:0] r;         // per signature randomizer
        logic [WIDTH-1:0] pk_seed;   // public_key seed
        logic [WIDTH-1:0] pk_root;   // public_key root
        logic     [255:0] m;         // message being signed (nonce)
    } h_msg_inner_t;
    typedef struct packed {
        // MGF1 SEED
        logic   [WIDTH-1:0] r;         // per signature randomizer
        logic   [WIDTH-1:0] pk_seed;   // public_key seed
        logic [2*WIDTH-1:0] inner;     // inner hash output
        // counter
        logic        [31:0] cntr;      // MGF1 counter
    } h_msg_outer_t;

endpackage
