package sphincs_pkg;

    import arith_pkg::*;

    // -------------------------------------------------------------------------
    // SPHINCS+-256f FORS parameters
    // -------------------------------------------------------------------------

    localparam int unsigned FORS_K          = 35;
    localparam int unsigned FORS_A          = 9;
    localparam int unsigned FORS_TREE_CNT_W = $clog2(FORS_K);       // 6
    localparam int unsigned FORS_LVL_W      = $clog2(FORS_A + 1);   // 4
    localparam int unsigned FORS_NODE_W     = FORS_A + 1;           // 10 (heap idx)

    // Public seed (placeholder; finalised when SPHINCS+ reference lands)
    localparam logic [WIDTH-1:0] PUB_SEED = 256'd0;

    // ADRS-type tags used by FORS hashing.
    // NOTE: this is a flat substitute, NOT the canonical 22-/32-byte ADRS.
    // Test-only deviation; revisit when SPHINCS+ interop becomes a goal.
    localparam logic [31:0] ADRS_FORS_TREE  = 32'd3;
    localparam logic [31:0] ADRS_FORS_ROOTS = 32'd4;

    // -------------------------------------------------------------------------
    // FORS license format: per-tree sk element + auth path. The K leaf
    // indices into the FORS trees are derived from the message Q hash, not
    // carried in the signature.
    // -------------------------------------------------------------------------

    typedef struct packed {
        //    Dimension 3 Dimension 2 Dimension 1
        logic [FORS_K-1:0]            [WIDTH-1:0]  sk;
        logic [FORS_K-1:0][FORS_A-1:0][WIDTH-1:0]  auth;
    } sphincs_lic_t;

endpackage
