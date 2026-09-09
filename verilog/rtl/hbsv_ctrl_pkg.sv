// Hash-based signature verifier — shared control bundle.
//
// The counters that address a hash — hypertree layer, OTS chain and step,
// Merkle level and node index, leaf index — as one bundle, so that a scheme
// package can turn them into that scheme's message fields. Each field is
// sized for the widest scheme the bundle serves; a narrower scheme
// zero-extends into it.

package hbsv_ctrl_pkg;

    // Signature scheme a verifier is elaborated for
    typedef enum int unsigned {
        SCHEME_LMS = 0    // RFC 8554 HSS/LMS
    } sch_e;

    typedef struct packed {
        logic [2:0]  layer;   // hypertree layer in processing order; 0 signs the message
        logic [6:0]  chain;   // OTS chain index
        logic [7:0]  step;    // OTS hash step within the chain
        logic [4:0]  level;   // Merkle level within the current tree
        logic [31:0] nidx;    // Merkle node index
        logic [31:0] leaf;    // leaf index within the current tree
    } ctrl_t;

endpackage
