// Hash-based signature verifier — scheme dispatch.
//
// Everything hss_verify needs to know about a signature scheme is an
// elaboration-time function of its SCH parameter: the scheme constants and,
// per hash message, its width and the builder that turns the shared ctrl_t
// bundle plus the live data values into the scheme's layout. The layout
// structs themselves belong to the scheme package (hss_pkg). Builders return
// the widest layout across schemes, right-aligned; the caller narrows with a
// width cast.

package hbsv_schs_pkg;

    import hbsv_ctrl_pkg::*;

    // Builder arguments are passed at the widest width across schemes; the
    // verifier narrows key context and data to kctx_w / digest_w of its SCH.
    localparam int unsigned MAX_KCTX_W = hss_pkg::IDENT_W;
    localparam int unsigned MAX_DATA_W = arith_pkg::WIDTH;

    // -------------------------------------------------------------------------
    // Scheme parameters
    // -------------------------------------------------------------------------

    // Node / signature-element / licence-beat width
    function automatic int unsigned digest_w(input sch_e sch);
        case (sch)
            SCHEME_HSS: return arith_pkg::WIDTH;
            default:    return 0;
        endcase
    endfunction

    // Key context of the current tree (LMS: the identifier I)
    function automatic int unsigned kctx_w(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::IDENT_W;
            default:    return 0;
        endcase
    endfunction

    // Winternitz digit width and chain count (data digits, checksum digits)
    function automatic int unsigned digit_w(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_W;
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len1(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_P1;
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len2(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_P2;
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_P;
            default:    return 0;
        endcase
    endfunction
    // Hypertree: number of layers and tree height
    function automatic int unsigned layers(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::HSS_LEVELS;
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned tree_h(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::TREE_H;
            default:    return 0;
        endcase
    endfunction
    // Header beats at the start of each layer's signature
    function automatic int unsigned hdr_beats(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::LAYER_HDR_BEATS;
            default:    return 0;
        endcase
    endfunction

    // Prefix of the OTS public-key accumulation
    localparam int unsigned ACC_PREFIX_W = $bits(hss_pkg::lms_pk_prefix_t);

    // -------------------------------------------------------------------------
    // ctrl_t -> scheme fields
    // -------------------------------------------------------------------------

    // These two take the whole bundle but read only the fields the scheme
    // needs; verilator -Wall flags the unread bits of the argument.
    /* verilator lint_off UNUSEDSIGNAL */

    // LMS: the u32 field is the leaf index q, or the parent node number
    // during a Merkle step. Leaf q is node 2^h + q; each level up halves
    // the node number.
    function automatic logic [hss_pkg::Q_W-1:0] ctrl2q(input ctrl_t ctrl);
        return ctrl.leaf_idx;
    endfunction
    function automatic logic [hss_pkg::Q_W-1:0] ctrl2node(input sch_e sch, input ctrl_t ctrl);
        return ((hss_pkg::Q_W'(1) << tree_h(sch)) | ctrl.leaf_idx) >> ctrl.mrkl_level >> 1;
    endfunction

    /* verilator lint_on UNUSEDSIGNAL */

    // -------------------------------------------------------------------------
    // Message hash of the layer that signs the message (LMS: Q)
    // -------------------------------------------------------------------------

    localparam int unsigned MAX_MSG_HASH_MSG_BITS = $bits(hss_pkg::lms_q_msg_t);

    function automatic int unsigned msg_hash_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS: return $bits(hss_pkg::lms_q_msg_t);
            default:    return 0;
        endcase
    endfunction

    function automatic logic [MAX_MSG_HASH_MSG_BITS-1:0] msg_hash_msg(
        input sch_e              sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t             ctrl,
        input logic [MAX_DATA_W-1:0] randomizer,
        input logic [MAX_DATA_W-1:0] message);
        case (sch)
            SCHEME_HSS: return MAX_MSG_HASH_MSG_BITS'(hss_pkg::lms_q_msg_t'{
                    i:      kctx,
                    q:      ctrl2q(ctrl),
                    d_mesg: hss_pkg::D_MESG,
                    c:      randomizer,
                    msg:    message});
            default:    return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Message hash of an upper hypertree layer (HSS only): Q over the
    // serialised public key of the layer below, given its identifier and
    // the root just computed for it
    // -------------------------------------------------------------------------

    localparam int unsigned SUB_PK_HASH_MSG_BITS = $bits(hss_pkg::lms_q_sub_msg_t);

    function automatic logic [SUB_PK_HASH_MSG_BITS-1:0] sub_pk_hash_msg(
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t             ctrl,
        input logic [MAX_DATA_W-1:0] randomizer,
        input logic [MAX_KCTX_W-1:0] sub_kctx,
        input logic [MAX_DATA_W-1:0] sub_root);
        return hss_pkg::lms_q_sub_msg_t'{
                    i:          kctx,
                    q:          ctrl2q(ctrl),
                    d_mesg:     hss_pkg::D_MESG,
                    c:          randomizer,
                    lms_type:   hss_pkg::LMS_TYPE,
                    lmots_type: hss_pkg::LMOTS_TYPE,
                    sub_i:      sub_kctx,
                    root:       sub_root};
    endfunction

    // -------------------------------------------------------------------------
    // OTS chain step
    // -------------------------------------------------------------------------

    localparam int unsigned MAX_OTS_CHAIN_MSG_BITS = $bits(hss_pkg::lms_chain_msg_t);

    function automatic int unsigned ots_chain_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS: return $bits(hss_pkg::lms_chain_msg_t);
            default:    return 0;
        endcase
    endfunction

    function automatic logic [MAX_OTS_CHAIN_MSG_BITS-1:0] ots_chain_msg(
        input sch_e              sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t             ctrl,
        input logic [MAX_DATA_W-1:0] tmp);
        case (sch)
            SCHEME_HSS: return MAX_OTS_CHAIN_MSG_BITS'(hss_pkg::lms_chain_msg_t'{
                    i:     kctx,
                    q:     ctrl2q(ctrl),
                    chain: hss_pkg::CHAIN_W'(ctrl.chain_idx),
                    step:  hss_pkg::STEP_W'(ctrl.hash_idx),
                    tmp:   tmp});
            default:    return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // OTS public-key hash: the prefix the chain endpoints are accumulated
    // behind
    // -------------------------------------------------------------------------

    function automatic logic [ACC_PREFIX_W-1:0] ots_pk_prefix(
        input sch_e              sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t             ctrl);
        case (sch)
            SCHEME_HSS: return hss_pkg::lms_pk_prefix_t'{
                    i:      kctx,
                    q:      ctrl2q(ctrl),
                    d_pblc: hss_pkg::D_PBLC};
            default:    return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Merkle leaf (HSS only): the leaf hash over the OTS public key
    // -------------------------------------------------------------------------

    localparam int unsigned MSS_LEAF_MSG_BITS = $bits(hss_pkg::lms_leaf_msg_t);

    function automatic logic [MSS_LEAF_MSG_BITS-1:0] mss_leaf_msg(
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t             ctrl,
        input logic [MAX_DATA_W-1:0] kc);
        return hss_pkg::lms_leaf_msg_t'{
                    i:      kctx,
                    q:      ctrl2q(ctrl),
                    d_leaf: hss_pkg::D_LEAF,
                    kc:     kc};
    endfunction

    // -------------------------------------------------------------------------
    // Merkle interior node: the parent hash over a left and a right child
    // -------------------------------------------------------------------------

    localparam int unsigned MAX_MSS_JOIN_MSG_BITS = $bits(hss_pkg::lms_intr_msg_t);

    function automatic int unsigned mss_join_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS: return $bits(hss_pkg::lms_intr_msg_t);
            default:    return 0;
        endcase
    endfunction

    function automatic logic [MAX_MSS_JOIN_MSG_BITS-1:0] mss_join_msg(
        input sch_e              sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t             ctrl,
        input logic [MAX_DATA_W-1:0] left,
        input logic [MAX_DATA_W-1:0] right);
        case (sch)
            SCHEME_HSS: return MAX_MSS_JOIN_MSG_BITS'(hss_pkg::lms_intr_msg_t'{
                    i:      kctx,
                    node:   ctrl2node(sch, ctrl),
                    d_intr: hss_pkg::D_INTR,
                    left:   left,
                    right:  right});
            default:    return '0;
        endcase
    endfunction

endpackage
