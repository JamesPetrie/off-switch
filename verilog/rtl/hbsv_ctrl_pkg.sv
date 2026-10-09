// Hash-based signature verifier — shared control bundle.
//
// The counters that address a hash — hypertree layer, OTS chain and hash
// index, Merkle level, leaf index, (SLH) hypertree index and the tree and
// leaf the Merkle walk starts from — as one bundle, so that a scheme package
// can turn them into that scheme's message fields. Each field is sized for
// the widest scheme the bundle serves; a narrower scheme zero-extends into
// it.

package hbsv_ctrl_pkg;

    // Signature scheme a verifier is elaborated for
    typedef enum int unsigned {
        SCHEME_HSS      = 0,   // RFC 8554 HSS/LMS
        SCHEME_SLH_128S = 1,   // FIPS 205 SLH-DSA-SHA2-128s
        SCHEME_MAX      = 2    // number of schemes; loops over them end here
    } sch_e;

    // Field widths; the verifier's counters are declared with the same ones
    localparam int unsigned HT_LAYER_W   = 3;
    localparam int unsigned CHAIN_IDX_W  = 7;
    localparam int unsigned HASH_IDX_W   = 8;
    localparam int unsigned MRKL_LEVEL_W = 5;
    localparam int unsigned LEAF_IDX_W   = 32;
    localparam int unsigned TREE_IDX_W   = 54;
    localparam int unsigned MRKL_TREE_W  = 6;

    typedef struct packed {
        logic [HT_LAYER_W-1:0]   ht_layer;    // hypertree layer, counted from the top: 0 holds
                                              // the public key, the highest signs the message
        logic [CHAIN_IDX_W-1:0]  chain_idx;   // OTS chain index
        logic [HASH_IDX_W-1:0]   hash_idx;    // OTS hash index within the chain
        logic [MRKL_LEVEL_W-1:0] mrkl_level;  // Merkle level within the current tree
        logic [LEAF_IDX_W-1:0]   leaf_idx;    // leaf index of the key pair in the current tree
        logic [TREE_IDX_W-1:0]   tree_idx;    // SLH index of the current tree in its layer
        logic [MRKL_TREE_W-1:0]  mrkl_tree;   // tree the Merkle walk is in: the layer's only
                                              // one (0), or one of the FORS trees
        logic [LEAF_IDX_W-1:0]   mrkl_leaf;   // leaf it starts from: the key pair's, or the one
                                              // that FORS tree reveals
    } ctrl_t;

endpackage
