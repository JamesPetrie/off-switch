// Hash-based signature verifier — shared control bundle.
//
// The counters that address a hash — hypertree layer, OTS chain and hash
// index, Merkle level and node index, leaf index — as one bundle, so that a
// scheme package can turn them into that scheme's message fields. Each field
// is sized for the widest scheme the bundle serves; a narrower scheme
// zero-extends into it.

package hbsv_ctrl_pkg;

    // Signature scheme a verifier is elaborated for
    typedef enum int unsigned {
        SCHEME_HSS = 0    // RFC 8554 HSS/LMS
    } sch_e;

    // Field widths; the verifier's counters are declared with the same ones
    localparam int unsigned HT_LAYER_W   = 3;
    localparam int unsigned CHAIN_IDX_W  = 7;
    localparam int unsigned HASH_IDX_W   = 8;
    localparam int unsigned MRKL_LEVEL_W = 5;
    localparam int unsigned LEAF_IDX_W   = 32;

    typedef struct packed {
        logic [HT_LAYER_W-1:0]   ht_layer;    // hypertree layer; 0 signs the message
        logic [CHAIN_IDX_W-1:0]  chain_idx;   // OTS chain index
        logic [HASH_IDX_W-1:0]   hash_idx;    // OTS hash index within the chain
        logic [MRKL_LEVEL_W-1:0] mrkl_level;  // Merkle level within the current tree
        logic [LEAF_IDX_W-1:0]   leaf_idx;    // leaf index within the current tree
    } ctrl_t;

endpackage
