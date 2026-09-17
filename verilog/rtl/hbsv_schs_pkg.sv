// Hash-based signature verifier — scheme dispatch.
//
// Everything hss_verify needs to know about a signature scheme is an
// elaboration-time function of its SCH parameter: the scheme constants and,
// per hash message, its width and the builder that turns the shared ctrl_t
// bundle plus the live data values into the scheme's layout. The layout
// structs themselves belong to the scheme packages (hss_pkg, slh_pkg).
// Builders return the widest layout across schemes, right-aligned; the
// caller narrows with a width cast.

package hbsv_schs_pkg;

    import hbsv_ctrl_pkg::*;

    // Builder arguments are passed at the widest width across schemes; the
    // verifier narrows key context and data to kctx_w / digest_w of its SCH.
    localparam int unsigned MAX_KCTX_W    = hss_pkg::IDENT_W;
    localparam int unsigned MAX_DATA_W    = arith_pkg::WIDTH;
    localparam int unsigned MAX_MSG_REG_W = arith_pkg::WIDTH;   // widest msg_reg_w

    localparam int unsigned SHA256_BLOCK_W = 512;

    // -------------------------------------------------------------------------
    // Scheme parameters
    // -------------------------------------------------------------------------

    // Node / signature-element / licence-beat width
    function automatic int unsigned digest_w(input sch_e sch);
        case (sch)
            SCHEME_HSS: return arith_pkg::WIDTH;
            default:    return slh_pkg::SLH_NW;
        endcase
    endfunction

    // Key context of the current tree (LMS: the identifier I)
    function automatic int unsigned kctx_w(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::IDENT_W;
            default:    return slh_pkg::SLH_NW;
        endcase
    endfunction

    // Winternitz digit width and chain count (data digits, checksum digits)
    function automatic int unsigned digit_w(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_W;
            default:    return slh_pkg::SLH_LGW;
        endcase
    endfunction
    function automatic int unsigned ots_len1(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_P1;
            default:    return slh_pkg::SLH_LEN1;
        endcase
    endfunction
    function automatic int unsigned ots_len2(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_P2;
            default:    return slh_pkg::SLH_LEN2;
        endcase
    endfunction
    function automatic int unsigned ots_len(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::WOTS_P;
            default:    return slh_pkg::SLH_LEN;
        endcase
    endfunction
    // Hypertree: number of layers and tree height
    function automatic int unsigned layers(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::HSS_LEVELS;
            default:    return slh_pkg::SLH_D;
        endcase
    endfunction
    function automatic int unsigned tree_h(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::TREE_H;
            default:    return slh_pkg::SLH_HP;
        endcase
    endfunction
    // Header beats at the start of each layer's signature
    function automatic int unsigned hdr_beats(input sch_e sch);
        case (sch)
            SCHEME_HSS: return hss_pkg::LAYER_HDR_BEATS;
            default:    return 0;
        endcase
    endfunction
    // FORS (SLH only): number of trees and their height
    function automatic int unsigned fors_k(input sch_e sch);
        case (sch)
            SCHEME_SLH_128S: return slh_pkg::SLH_K;
            default:         return 0;
        endcase
    endfunction
    function automatic int unsigned fors_h(input sch_e sch);
        case (sch)
            SCHEME_SLH_128S: return slh_pkg::SLH_A;
            default:         return 0;
        endcase
    endfunction
    // Width of the message the OTS chains sign, held through a layer: HSS the
    // Q digest; SLH the FORS message digest md, then the FORS public key and
    // each layer's root
    function automatic int unsigned msg_reg_w(input sch_e sch);
        case (sch)
            SCHEME_HSS: return arith_pkg::WIDTH;
            default:    return slh_pkg::SLH_MD_W;
        endcase
    endfunction
    // Bits the SHA-256 length field counts for a precomputed first block: SLH
    // resumes every F, H and T call from the midstate of PK.seed || 0^48
    function automatic int unsigned midstate_bits(input sch_e sch);
        case (sch)
            SCHEME_SLH_128S: return SHA256_BLOCK_W;
            default:         return 0;
        endcase
    endfunction
    function automatic bit resumes_from_midstate(input sch_e sch);
        return (midstate_bits(sch) != 0);
    endfunction
    // Whether the OTS public key is hashed once more for the Merkle leaf
    // (HSS) or is the leaf itself (SLH)
    function automatic bit has_mss_leaf_hash(input sch_e sch);
        return (sch == SCHEME_HSS);
    endfunction

    // -------------------------------------------------------------------------
    // Accumulation of the OTS public key (HSS Kc; SLH T_len, and T_k over the
    // FORS roots)
    //
    // The endpoints are hashed behind a prefix of ACC_PREFIX_W bits (HSS
    // I || q || D_PBLC, SLH ADRSc: 22 bytes in both) in 64-byte SHA-256
    // blocks, so with E-byte endpoints a block boundary falls (64 - 22) mod E
    // bytes into an endpoint. The verifier banks endpoints until one
    // straddles a boundary, absorbs the block with that endpoint's head, and
    // carries its tail into the next block. With E = 32 (HSS) block 0 holds
    // the prefix, one endpoint and the head of the next, and every later
    // block the previous tail, one endpoint and the next head. With E = 16
    // (SLH-128s) block 0 holds the prefix, two endpoints and a head, and
    // every later block a tail, three endpoints and a head. The final block
    // holds the last tail, the endpoints still banked (none after 34 or 35
    // endpoints, three after the 14 FORS roots) and the padding.
    // -------------------------------------------------------------------------

    localparam int unsigned ACC_PREFIX_W = $bits(hss_pkg::lms_pk_prefix_t);

    // Endpoints banked before the first block closes, and before each later one
    function automatic int unsigned acc_first_full(input sch_e sch);
        return (SHA256_BLOCK_W - ACC_PREFIX_W) / digest_w(sch);
    endfunction
    function automatic int unsigned acc_mid_full(input sch_e sch);
        return SHA256_BLOCK_W / digest_w(sch) - 1;
    endfunction
    // Bits of the endpoint closing a block that fit in it, and the rest
    function automatic int unsigned acc_head_w(input sch_e sch);
        return (SHA256_BLOCK_W - ACC_PREFIX_W) % digest_w(sch);
    endfunction
    function automatic int unsigned acc_carry_w(input sch_e sch);
        return digest_w(sch) - acc_head_w(sch);
    endfunction
    // Endpoints still banked when a stream of count endpoints ends
    function automatic int unsigned acc_tail_elems(input sch_e sch, input int unsigned count);
        if (count <= acc_first_full(sch)) return count;
        return (count - 1 - acc_first_full(sch)) % (acc_mid_full(sch) + 1);
    endfunction

    // -------------------------------------------------------------------------
    // ctrl_t -> scheme fields
    // -------------------------------------------------------------------------

    // These take the whole bundle, or the whole digest, but read only the
    // fields the scheme needs, so -Wall would flag the unread bits.
    /* verilator lint_off UNUSEDSIGNAL */

    // LMS: the u32 field is the leaf index q, or the parent node number
    // during a Merkle step. Leaf q is node 2^h + q; each level up halves
    // the node number. SLH numbers the nodes of a level from zero, so the
    // parent's tree index is the leaf index halved once per level.
    function automatic logic [hss_pkg::Q_W-1:0] ctrl2q(input ctrl_t ctrl);
        return ctrl.leaf_idx;
    endfunction
    function automatic logic [hss_pkg::Q_W-1:0] ctrl2node(input sch_e sch, input ctrl_t ctrl);
        case (sch)
            SCHEME_HSS: return ((hss_pkg::Q_W'(1) << tree_h(sch)) | ctrl.leaf_idx)
                               >> ctrl.mrkl_level >> 1;
            default:    return hss_pkg::Q_W'(ctrl.leaf_idx) >> ctrl.mrkl_level >> 1;
        endcase
    endfunction

    // SLH: the compressed address of a hash. The layer address counts from
    // the bottom of the hypertree while ht_layer counts from the top; the
    // key-pair address is the leaf index except in tree hashes; the last two
    // words are the caller's, their meaning depends on the type.
    function automatic slh_pkg::slh_adrs_c_t slh_adrs(
        input sch_e                            sch,
        input ctrl_t                           ctrl,
        input logic [slh_pkg::ADRS_TYPE_W-1:0] adrs_type,
        input logic [slh_pkg::ADRS_WORD_W-1:0] chain_or_height,
        input logic [slh_pkg::ADRS_WORD_W-1:0] hash_or_index);
        return slh_pkg::slh_adrs_c_t'{
                    layer_addr:      slh_pkg::ADRS_LAYER_W'(layers(sch) - 1
                                                            - int'(ctrl.ht_layer)),
                    tree_addr:       slh_pkg::ADRS_TREE_W'(ctrl.tree_idx),
                    adrs_type:       adrs_type,
                    keypair_addr:    (adrs_type == slh_pkg::ADRS_TREE)
                                         ? '0 : slh_pkg::ADRS_WORD_W'(ctrl.leaf_idx),
                    chain_or_height: chain_or_height,
                    hash_or_index:   hash_or_index};
    endfunction

    // SLH: the H_msg digest split into md and the two indices (Alg 20); the
    // bits above each index and beyond the m bytes of the digest are unread
    typedef struct packed {
        logic [MAX_MSG_REG_W-1:0] md;
        logic [TREE_IDX_W-1:0]    tree_idx;
        logic [LEAF_IDX_W-1:0]    leaf_idx;
    } hmsg_split_t;

    function automatic hmsg_split_t slh_hmsg_split(
        input logic [slh_pkg::SHA256_STATE_W-1:0] digest);
        slh_pkg::slh_digest_t fields;
        fields = digest;
        return hmsg_split_t'{
                    md:       MAX_MSG_REG_W'(fields.md),
                    tree_idx: TREE_IDX_W'(fields.tree_field[slh_pkg::SLH_IDX_TREE_W-1:0]),
                    leaf_idx: LEAF_IDX_W'(fields.leaf_field[slh_pkg::SLH_IDX_LEAF_W-1:0])};
    endfunction

    /* verilator lint_on UNUSEDSIGNAL */

    // -------------------------------------------------------------------------
    // Message hash of the layer that signs the message: LMS Q, built here;
    // SLH the inner hash of H_msg, which also needs PK.root and so has its
    // own builder, slh_hmsg_msg. msg_hash_msg_bits gives either width.
    // -------------------------------------------------------------------------

    localparam int unsigned MAX_MSG_HASH_MSG_BITS = $bits(hss_pkg::lms_q_msg_t);

    function automatic int unsigned msg_hash_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS: return $bits(hss_pkg::lms_q_msg_t);
            default:    return $bits(slh_pkg::slh_hmsg_msg_t);
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
    // H_msg, inner hash (SLH only): over R, the public key and the message
    // -------------------------------------------------------------------------

    localparam int unsigned SLH_HMSG_MSG_BITS = $bits(slh_pkg::slh_hmsg_msg_t);

    function automatic logic [SLH_HMSG_MSG_BITS-1:0] slh_hmsg_msg(
        input logic [MAX_KCTX_W-1:0]     kctx,
        input logic [slh_pkg::SLH_NW-1:0] pk_root,
        input logic [slh_pkg::SLH_NW-1:0] randomizer,
        input logic [MAX_DATA_W-1:0]     message);
        return slh_pkg::slh_hmsg_msg_t'{
                    r:        randomizer,
                    seed:     kctx[MAX_KCTX_W-1 -: slh_pkg::SLH_NW],
                    root:     pk_root,
                    mode_ctx: '0,
                    message:  message};
    endfunction

    // -------------------------------------------------------------------------
    // H_msg, outer hash (SLH only): MGF1 over R, PK.seed and the inner digest
    // -------------------------------------------------------------------------

    localparam int unsigned SLH_MGF1_MSG_BITS = $bits(slh_pkg::slh_mgf1_msg_t);

    function automatic logic [SLH_MGF1_MSG_BITS-1:0] slh_mgf1_msg(
        input logic [MAX_KCTX_W-1:0]             kctx,
        input logic [slh_pkg::SLH_NW-1:0]         randomizer,
        input logic [slh_pkg::SHA256_STATE_W-1:0] inner_digest);
        return slh_pkg::slh_mgf1_msg_t'{
                    r:            randomizer,
                    seed:         kctx[MAX_KCTX_W-1 -: slh_pkg::SLH_NW],
                    inner_digest: inner_digest,
                    counter:      '0};
    endfunction

    // -------------------------------------------------------------------------
    // OTS chain step
    // -------------------------------------------------------------------------

    localparam int unsigned MAX_OTS_CHAIN_MSG_BITS = $bits(hss_pkg::lms_chain_msg_t);

    function automatic int unsigned ots_chain_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS: return $bits(hss_pkg::lms_chain_msg_t);
            default:    return $bits(slh_pkg::slh_f_msg_t);
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
            default:    return MAX_OTS_CHAIN_MSG_BITS'(slh_pkg::slh_f_msg_t'{
                    adrs: slh_adrs(.sch             (sch),
                                   .ctrl            (ctrl),
                                   .adrs_type       (slh_pkg::ADRS_WOTS_HASH),
                                   .chain_or_height (slh_pkg::ADRS_WORD_W'(ctrl.chain_idx)),
                                   .hash_or_index   (slh_pkg::ADRS_WORD_W'(ctrl.hash_idx))),
                    m1:   tmp[MAX_DATA_W-1 -: slh_pkg::SLH_NW]});
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // FORS leaf (SLH only): F over the secret element, addressed by the leaf's
    // index in the forest, tree * 2^a + digit
    // -------------------------------------------------------------------------

    localparam int unsigned FORS_LEAF_MSG_BITS = $bits(slh_pkg::slh_f_msg_t);
    localparam int unsigned FORS_LEAF_IDX_W    = slh_pkg::ADRS_WORD_W;   // an ADRS word

    function automatic logic [FORS_LEAF_MSG_BITS-1:0] fors_leaf_msg(
        input sch_e                        sch,
        input ctrl_t                       ctrl,
        input logic [FORS_LEAF_IDX_W-1:0]  fors_leaf,
        input logic [slh_pkg::SLH_NW-1:0]  secret);
        return slh_pkg::slh_f_msg_t'{
                    adrs: slh_adrs(.sch             (sch),
                                   .ctrl            (ctrl),
                                   .adrs_type       (slh_pkg::ADRS_FORS_TREE),
                                   .chain_or_height ('0),
                                   .hash_or_index   (fors_leaf)),
                    m1:   secret};
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
            default:    return slh_adrs(.sch             (sch),
                                        .ctrl            (ctrl),
                                        .adrs_type       (slh_pkg::ADRS_WOTS_PK),
                                        .chain_or_height ('0),
                                        .hash_or_index   ('0));
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // FORS public-key hash (SLH only): the prefix the FORS roots are
    // accumulated behind
    // -------------------------------------------------------------------------

    function automatic logic [ACC_PREFIX_W-1:0] fors_pk_prefix(input sch_e sch, input ctrl_t ctrl);
        return slh_adrs(.sch             (sch),
                        .ctrl            (ctrl),
                        .adrs_type       (slh_pkg::ADRS_FORS_ROOTS),
                        .chain_or_height ('0),
                        .hash_or_index   ('0));
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
            default:    return $bits(slh_pkg::slh_h_msg_t);
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
            default:    return MAX_MSS_JOIN_MSG_BITS'(slh_pkg::slh_h_msg_t'{
                    adrs:  slh_adrs(.sch             (sch),
                                    .ctrl            (ctrl),
                                    .adrs_type       (slh_pkg::ADRS_TREE),
                                    .chain_or_height (slh_pkg::ADRS_WORD_W'(ctrl.mrkl_level)
                                                      + slh_pkg::ADRS_WORD_W'(1)),
                                    .hash_or_index   (ctrl2node(sch, ctrl))),
                    left:  left[MAX_DATA_W-1 -: slh_pkg::SLH_NW],
                    right: right[MAX_DATA_W-1 -: slh_pkg::SLH_NW]});
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // FORS interior node (SLH only): the parent hash within one FORS tree,
    // addressed by the leaf's index in the forest halved once per level
    // -------------------------------------------------------------------------

    localparam int unsigned FORS_JOIN_MSG_BITS = $bits(slh_pkg::slh_h_msg_t);

    function automatic logic [FORS_JOIN_MSG_BITS-1:0] fors_join_msg(
        input sch_e                        sch,
        input ctrl_t                       ctrl,
        input logic [FORS_LEAF_IDX_W-1:0]  fors_leaf,
        input logic [slh_pkg::SLH_NW-1:0]  left,
        input logic [slh_pkg::SLH_NW-1:0]  right);
        return slh_pkg::slh_h_msg_t'{
                    adrs:  slh_adrs(.sch             (sch),
                                    .ctrl            (ctrl),
                                    .adrs_type       (slh_pkg::ADRS_FORS_TREE),
                                    .chain_or_height (slh_pkg::ADRS_WORD_W'(ctrl.mrkl_level)
                                                      + slh_pkg::ADRS_WORD_W'(1)),
                                    .hash_or_index   (fors_leaf >> ctrl.mrkl_level >> 1)),
                    left:  left,
                    right: right};
    endfunction

endpackage
