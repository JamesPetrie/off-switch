// Hash-based signature verifier — scheme dispatch.
//
// Everything hss_verify needs to know about a signature scheme is an
// elaboration-time function of its SCH parameter: the hash-message widths
// and the message builders that turn the shared ctrl_t bundle plus the live
// data values into the scheme's byte layout. The layout structs themselves
// belong to the scheme package (hss_pkg). Builders return the widest layout
// across schemes, right-aligned; the caller narrows with a width cast.

package hbsv_schs_pkg;

    import hbsv_ctrl_pkg::*;

    // Key context of the current tree: the LMS identifier I
    localparam int unsigned KCTX_W = hss_pkg::IDENT_W;

    // Data values (message, randomizer, nodes) are passed at this width
    localparam int unsigned DATA_W = arith_pkg::WIDTH;

    // -------------------------------------------------------------------------
    // Hash-message widths
    // -------------------------------------------------------------------------

    localparam int unsigned MAX_MSG_HASH_BITS  = $bits(hss_pkg::lms_q_sub_msg_t);  // 880
    localparam int unsigned MAX_CHAIN_MSG_BITS = $bits(hss_pkg::lms_chain_msg_t);  // 440
    localparam int unsigned MAX_LEAF_MSG_BITS  = $bits(hss_pkg::lms_leaf_msg_t);   // 432
    localparam int unsigned MAX_TREE_MSG_BITS  = $bits(hss_pkg::lms_intr_msg_t);   // 688

    // Prefix of the OTS public-key accumulation (LMS: I || q || D_PBLC)
    localparam int unsigned ACC_PREFIX_W = $bits(hss_pkg::lms_prefix_t);          // 176

    // Bits actually presented for each message
    function automatic int unsigned msg_hash_bits(input sch_e s, input bit sub);
        case (s)
            SCHEME_LMS: return sub ? $bits(hss_pkg::lms_q_sub_msg_t)
                                   : $bits(hss_pkg::lms_q_msg_t);           // 880 / 688
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned chain_msg_bits(input sch_e s);
        case (s)
            SCHEME_LMS: return $bits(hss_pkg::lms_chain_msg_t);             // 440
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned leaf_msg_bits(input sch_e s);
        case (s)
            SCHEME_LMS: return $bits(hss_pkg::lms_leaf_msg_t);              // 432
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned tree_msg_bits(input sch_e s);
        case (s)
            SCHEME_LMS: return $bits(hss_pkg::lms_intr_msg_t);              // 688
            default:    return 0;
        endcase
    endfunction

    // Not every scheme reads every field of the bundle or every argument.
    /* verilator lint_off UNUSEDSIGNAL */

    // -------------------------------------------------------------------------
    // ctrl_t -> scheme fields
    // -------------------------------------------------------------------------

    // LMS: the u32 field is the leaf index q, or the parent node number
    // during a Merkle step (nodes are numbered 2n / 2n+1 from parent n).
    function automatic logic [31:0] ctrl2q(input ctrl_t c);
        return c.leaf;
    endfunction
    function automatic logic [31:0] ctrl2node(input ctrl_t c);
        return c.nidx >> 1;
    endfunction

    // -------------------------------------------------------------------------
    // Message builders
    // -------------------------------------------------------------------------

    // Per-layer message hash. LMS: Q over the randomizer and either the user
    // message or, at an upper layer (sub), the serialised public key of the
    // layer below: its identifier and the root just computed for it.
    function automatic logic [MAX_MSG_HASH_BITS-1:0] msg_hash_msg(
        input sch_e              s,
        input logic [KCTX_W-1:0] kctx,
        input ctrl_t             c,
        input logic [DATA_W-1:0] rand_beat,
        input logic [DATA_W-1:0] message,
        input bit                sub,
        input logic [KCTX_W-1:0] prev_kctx,
        input logic [DATA_W-1:0] prev_root);
        case (s)
            SCHEME_LMS: begin
                if (sub) return MAX_MSG_HASH_BITS'(hss_pkg::lms_q_sub_msg_t'{
                    pre:        hss_pkg::lms_prefix_t'{i: kctx, q: ctrl2q(c), d: hss_pkg::D_MESG},
                    c:          rand_beat,
                    lms_type:   hss_pkg::LMS_TYPE,
                    lmots_type: hss_pkg::LMOTS_TYPE,
                    sub_i:      prev_kctx,
                    root:       prev_root});
                else     return MAX_MSG_HASH_BITS'(hss_pkg::lms_q_msg_t'{
                    pre:        hss_pkg::lms_prefix_t'{i: kctx, q: ctrl2q(c), d: hss_pkg::D_MESG},
                    c:          rand_beat,
                    msg:        message});
            end
            default: return '0;
        endcase
    endfunction

    // OTS chain step: LMS H(I || q || i || j || tmp)
    function automatic logic [MAX_CHAIN_MSG_BITS-1:0] ots_chain_msg(
        input sch_e              s,
        input logic [KCTX_W-1:0] kctx,
        input ctrl_t             c,
        input logic [DATA_W-1:0] tmp);
        case (s)
            SCHEME_LMS: return MAX_CHAIN_MSG_BITS'(hss_pkg::lms_chain_msg_t'{
                    i: kctx, q: ctrl2q(c), chain: 16'(c.chain), step: c.step, tmp: tmp});
            default:    return '0;
        endcase
    endfunction

    // Prefix of the OTS public-key accumulation: LMS I || q || D_PBLC
    function automatic logic [ACC_PREFIX_W-1:0] ots_pk_prefix(
        input sch_e              s,
        input logic [KCTX_W-1:0] kctx,
        input ctrl_t             c);
        case (s)
            SCHEME_LMS: return hss_pkg::lms_prefix_t'{i: kctx, q: ctrl2q(c), d: hss_pkg::D_PBLC};
            default:    return '0;
        endcase
    endfunction

    // LMS only: leaf H(I || q || D_LEAF || Kc)
    function automatic logic [MAX_LEAF_MSG_BITS-1:0] leaf_msg(
        input logic [KCTX_W-1:0] kctx,
        input ctrl_t             c,
        input logic [DATA_W-1:0] kc);
        return MAX_LEAF_MSG_BITS'(hss_pkg::lms_leaf_msg_t'{
                    pre: hss_pkg::lms_prefix_t'{i: kctx, q: ctrl2q(c), d: hss_pkg::D_LEAF},
                    kc:  kc});
    endfunction

    // Merkle interior node: LMS H(I || node || D_INTR || left || right)
    function automatic logic [MAX_TREE_MSG_BITS-1:0] ots_tree_join_msg(
        input sch_e              s,
        input logic [KCTX_W-1:0] kctx,
        input ctrl_t             c,
        input logic [DATA_W-1:0] left,
        input logic [DATA_W-1:0] right);
        case (s)
            SCHEME_LMS: return MAX_TREE_MSG_BITS'(hss_pkg::lms_intr_msg_t'{
                    i: kctx, node: ctrl2node(c), d: hss_pkg::D_INTR, left: left, right: right});
            default:    return '0;
        endcase
    endfunction

    /* verilator lint_on UNUSEDSIGNAL */

endpackage
