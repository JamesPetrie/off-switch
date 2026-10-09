// Hash-based signature verifier — scheme dispatch.
//
// Everything hss_verify needs to know about a signature scheme is an
// elaboration-time function of its SCH parameter: the scheme constants and,
// per hash message, its width and the builder that turns the shared ctrl_t
// bundle plus the live data values into the scheme's layout. The layout
// structs themselves belong to the scheme packages (hss_pkg, slh_pkg).
// Builders take their arguments and return the layout at the widest width
// across schemes, right-aligned; the caller widens and narrows with width
// casts. A message a scheme does not have is all zeroes.

package hbsv_schs_pkg;

    import hbsv_ctrl_pkg::*;

    // -------------------------------------------------------------------------
    // Scheme parameters
    // -------------------------------------------------------------------------

    // Node / signature-element / licence-beat width
    function automatic int unsigned digest_w(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return arith_pkg::WIDTH;
            SCHEME_SLH_128S: return slh_pkg::SLH_NW;
            default:         return 0;
        endcase
    endfunction

    // Key context of a tree (LMS: the identifier I; SLH: PK.seed)
    function automatic int unsigned kctx_w(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::IDENT_W;
            SCHEME_SLH_128S: return slh_pkg::SLH_NW;
            default:         return 0;
        endcase
    endfunction

    // Winternitz digit width and chain count (data digits, checksum digits)
    function automatic int unsigned digit_w(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::WOTS_W;
            SCHEME_SLH_128S: return slh_pkg::SLH_LGW;
            default:         return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len1(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::WOTS_P1;
            SCHEME_SLH_128S: return slh_pkg::SLH_LEN1;
            default:         return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len2(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::WOTS_P2;
            SCHEME_SLH_128S: return slh_pkg::SLH_LEN2;
            default:         return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::WOTS_P;
            SCHEME_SLH_128S: return slh_pkg::SLH_LEN;
            default:         return 0;
        endcase
    endfunction
    // Hypertree: number of layers and tree height
    function automatic int unsigned layers(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::HSS_LEVELS;
            SCHEME_SLH_128S: return slh_pkg::SLH_D;
            default:         return 0;
        endcase
    endfunction
    function automatic int unsigned tree_h(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::TREE_H;
            SCHEME_SLH_128S: return slh_pkg::SLH_HP;
            default:         return 0;
        endcase
    endfunction
    // Header beats at the start of each layer's signature
    function automatic int unsigned hdr_beats(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return hss_pkg::LAYER_HDR_BEATS;
            SCHEME_SLH_128S: return 0;
            default:         return 0;
        endcase
    endfunction
    // FORS: number of trees and their height (none: 0)
    function automatic int unsigned fors_k(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return 0;
            SCHEME_SLH_128S: return slh_pkg::SLH_K;
            default:         return 0;
        endcase
    endfunction
    function automatic int unsigned fors_h(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return 0;
            SCHEME_SLH_128S: return slh_pkg::SLH_A;
            default:         return 0;
        endcase
    endfunction
    // Width of the signand, the value the OTS chains or the FORS trees sign
    // directly
    function automatic int unsigned signand_w(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return arith_pkg::WIDTH;
            SCHEME_SLH_128S: return slh_pkg::SLH_MD_W;
            default:         return 0;
        endcase
    endfunction
    // Whether the OTS public key is hashed once more for the Merkle leaf, or
    // is the leaf itself
    function automatic bit has_mss_leaf_hash(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return 1'b1;
            SCHEME_SLH_128S: return 1'b0;
            default:         return 1'b0;
        endcase
    endfunction
    // Whether the root of a tree is hashed into another message for the layer
    // above to sign, or is signed directly
    function automatic bit has_sub_pk_hash(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return 1'b1;
            SCHEME_SLH_128S: return 1'b0;
            default:         return 1'b0;
        endcase
    endfunction
    // Whether the message digest is extended by a second hash to more bits
    // than one hash produces
    function automatic bit extends_digest(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return 1'b0;
            SCHEME_SLH_128S: return 1'b1;
            default:         return 1'b0;
        endcase
    endfunction

    // Widest across the schemes
    function automatic int unsigned max2(input int unsigned a, input int unsigned b);
        return (a > b) ? a : b;
    endfunction
    function automatic int unsigned kctx_w_max();
        kctx_w_max = 0;
        for (int i = 0; i < SCHEME_MAX; i++) begin
            kctx_w_max = max2(kctx_w_max, kctx_w(sch_e'(i)));
        end
    endfunction
    function automatic int unsigned digest_w_max();
        digest_w_max = 0;
        for (int i = 0; i < SCHEME_MAX; i++) begin
            digest_w_max = max2(digest_w_max, digest_w(sch_e'(i)));
        end
    endfunction
    function automatic int unsigned signand_w_max();
        signand_w_max = 0;
        for (int i = 0; i < SCHEME_MAX; i++) begin
            signand_w_max = max2(signand_w_max, signand_w(sch_e'(i)));
        end
    endfunction

    localparam int unsigned MAX_KCTX_W    = kctx_w_max();
    localparam int unsigned MAX_DATA_W    = digest_w_max();
    localparam int unsigned MAX_SIGNAND_W = signand_w_max();

    localparam int unsigned MESSAGE_W = arith_pkg::WIDTH;
    localparam int unsigned SHA_DIGEST_W = sha2_wrap_pkg::SHA256_DIGEST_W;

    // The builders take their arguments at the widest width and the whole
    // control bundle, and a scheme reads only its part of them, so -Wall
    // would flag the unread bits.
    /* verilator lint_off UNUSEDSIGNAL */

    // -------------------------------------------------------------------------
    // ctrl_t -> scheme fields
    // -------------------------------------------------------------------------

    // LMS: the u32 field is the leaf index q, or the parent node number
    // during a Merkle step. The leaf the walk starts from is node 2^h + q;
    // each level up halves the node number.
    function automatic logic [hss_pkg::Q_W-1:0] ctrl2q(input ctrl_t ctrl);
        return ctrl.leaf_idx;
    endfunction
    function automatic logic [hss_pkg::Q_W-1:0] ctrl2node(input sch_e sch, input ctrl_t ctrl);
        return ((hss_pkg::Q_W'(1) << tree_h(sch)) | ctrl.mrkl_leaf) >> ctrl.mrkl_level >> 1;
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

    // SLH: the nodes of a tree level are numbered from zero, so the parent's
    // index is the index of the leaf the walk starts from, halved once per
    // level. The FORS trees share one numbering: the leaves of tree t follow
    // those of tree t - 1.
    function automatic logic [slh_pkg::ADRS_WORD_W-1:0] ctrl2height(input ctrl_t ctrl);
        return slh_pkg::ADRS_WORD_W'(ctrl.mrkl_level) + slh_pkg::ADRS_WORD_W'(1);
    endfunction
    function automatic logic [slh_pkg::ADRS_WORD_W-1:0] ctrl2tree_index(input ctrl_t ctrl);
        return slh_pkg::ADRS_WORD_W'(ctrl.mrkl_leaf) >> ctrl.mrkl_level >> 1;
    endfunction
    function automatic logic [slh_pkg::ADRS_WORD_W-1:0] ctrl2fors_leaf(input ctrl_t ctrl);
        return (slh_pkg::ADRS_WORD_W'(ctrl.mrkl_tree) << slh_pkg::SLH_A)
               | slh_pkg::ADRS_WORD_W'(ctrl.mrkl_leaf);
    endfunction
    function automatic logic [slh_pkg::ADRS_WORD_W-1:0] ctrl2fors_index(input ctrl_t ctrl);
        return ctrl2fors_leaf(ctrl) >> ctrl.mrkl_level >> 1;
    endfunction

    // SLH: PK.seed, zero-padded to a whole block
    function automatic slh_pkg::slh_seed_block_t slh_seed_block(
        input logic [MAX_KCTX_W-1:0] kctx);
        return slh_pkg::slh_seed_block_t'{
                    seed:     slh_pkg::SLH_NW'(kctx),
                    zero_pad: '0};
    endfunction

    // -------------------------------------------------------------------------
    // Message hash of the layer that signs the message
    // -------------------------------------------------------------------------

    function automatic int unsigned msg_hash_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return $bits(hss_pkg::lms_q_msg_t);
            SCHEME_SLH_128S: return $bits(slh_pkg::slh_hmsg_msg_t);
            default:         return 0;
        endcase
    endfunction

    function automatic int unsigned msg_hash_msg_bits_max();
        msg_hash_msg_bits_max = 0;
        for (int i = 0; i < SCHEME_MAX; i++) begin
            msg_hash_msg_bits_max = max2(msg_hash_msg_bits_max, msg_hash_msg_bits(sch_e'(i)));
        end
    endfunction

    localparam int unsigned MAX_MSG_HASH_MSG_BITS = msg_hash_msg_bits_max();

    function automatic logic [MAX_MSG_HASH_MSG_BITS-1:0] msg_hash_msg(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl,
        input logic [MAX_DATA_W-1:0] randomizer,
        input logic [MAX_DATA_W-1:0] pk_root,
        input logic [MESSAGE_W-1:0]  message);
        case (sch)
            SCHEME_HSS:      return MAX_MSG_HASH_MSG_BITS'(hss_pkg::lms_q_msg_t'{
                    i:      kctx,
                    q:      ctrl2q(ctrl),
                    d_mesg: hss_pkg::D_MESG,
                    c:      randomizer,
                    msg:    message});
            SCHEME_SLH_128S: return MAX_MSG_HASH_MSG_BITS'(slh_pkg::slh_hmsg_msg_t'{
                    r:        slh_pkg::SLH_NW'(randomizer),
                    seed:     slh_pkg::SLH_NW'(kctx),
                    root:     slh_pkg::SLH_NW'(pk_root),
                    mode_ctx: '0,
                    message:  message});
            default:         return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Message hash of an upper hypertree layer: over the serialised public
    // key of the layer below, given its key context and the root just
    // computed for it
    // -------------------------------------------------------------------------

    localparam int unsigned SUB_PK_HASH_MSG_BITS = $bits(hss_pkg::lms_q_sub_msg_t);

    function automatic logic [SUB_PK_HASH_MSG_BITS-1:0] sub_pk_hash_msg(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl,
        input logic [MAX_DATA_W-1:0] randomizer,
        input logic [MAX_KCTX_W-1:0] sub_kctx,
        input logic [MAX_DATA_W-1:0] sub_root);
        case (sch)
            SCHEME_HSS:      return hss_pkg::lms_q_sub_msg_t'{
                    i:          kctx,
                    q:          ctrl2q(ctrl),
                    d_mesg:     hss_pkg::D_MESG,
                    c:          randomizer,
                    lms_type:   hss_pkg::LMS_TYPE,
                    lmots_type: hss_pkg::LMOTS_TYPE,
                    sub_i:      sub_kctx,
                    root:       sub_root};
            SCHEME_SLH_128S: return '0;
            default:         return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Message digest extension: a second hash over the randomizer and the
    // digest of the message hash, then the split of its digest into the
    // signand and the indices of the tree and the leaf that sign it
    // -------------------------------------------------------------------------

    localparam int unsigned DIGEST_EXT_MSG_BITS = $bits(slh_pkg::slh_mgf1_msg_t);

    function automatic logic [DIGEST_EXT_MSG_BITS-1:0] digest_ext_msg(
        input sch_e                    sch,
        input logic [MAX_KCTX_W-1:0]   kctx,
        input logic [MAX_DATA_W-1:0]   randomizer,
        input logic [SHA_DIGEST_W-1:0] digest);
        case (sch)
            SCHEME_HSS:      return '0;
            SCHEME_SLH_128S: return slh_pkg::slh_mgf1_msg_t'{
                    r:            slh_pkg::SLH_NW'(randomizer),
                    seed:         slh_pkg::SLH_NW'(kctx),
                    inner_digest: digest,
                    counter:      '0};
            default:         return '0;
        endcase
    endfunction

    typedef struct packed {
        logic [MAX_SIGNAND_W-1:0] signand;
        logic [TREE_IDX_W-1:0]    tree_idx;
        logic [LEAF_IDX_W-1:0]    leaf_idx;
    } msg_digest_t;

    function automatic msg_digest_t msg_digest_split(
        input sch_e                    sch,
        input logic [SHA_DIGEST_W-1:0] digest);
        slh_pkg::slh_digest_t slh_digest;
        slh_digest = digest;
        case (sch)
            SCHEME_HSS:      return '0;
            SCHEME_SLH_128S: return msg_digest_t'{
                    signand:  MAX_SIGNAND_W'(slh_digest.md),
                    tree_idx: TREE_IDX_W'(slh_digest.tree_field[slh_pkg::SLH_IDX_TREE_W-1:0]),
                    leaf_idx: LEAF_IDX_W'(slh_digest.leaf_field[slh_pkg::SLH_IDX_LEAF_W-1:0])};
            default:         return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // OTS chain step
    // -------------------------------------------------------------------------

    function automatic int unsigned ots_chain_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return $bits(hss_pkg::lms_chain_msg_t);
            SCHEME_SLH_128S: return $bits(slh_pkg::slh_f_msg_t);
            default:         return 0;
        endcase
    endfunction

    function automatic int unsigned ots_chain_msg_bits_max();
        ots_chain_msg_bits_max = 0;
        for (int i = 0; i < SCHEME_MAX; i++) begin
            ots_chain_msg_bits_max = max2(ots_chain_msg_bits_max, ots_chain_msg_bits(sch_e'(i)));
        end
    endfunction

    localparam int unsigned MAX_OTS_CHAIN_MSG_BITS = ots_chain_msg_bits_max();

    function automatic logic [MAX_OTS_CHAIN_MSG_BITS-1:0] ots_chain_msg(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl,
        input logic [MAX_DATA_W-1:0] tmp);
        case (sch)
            SCHEME_HSS:      return MAX_OTS_CHAIN_MSG_BITS'(hss_pkg::lms_chain_msg_t'{
                    i:     kctx,
                    q:     ctrl2q(ctrl),
                    chain: hss_pkg::CHAIN_W'(ctrl.chain_idx),
                    step:  hss_pkg::STEP_W'(ctrl.hash_idx),
                    tmp:   tmp});
            SCHEME_SLH_128S: return MAX_OTS_CHAIN_MSG_BITS'(slh_pkg::slh_f_msg_t'{
                    seed_block: slh_seed_block(kctx),
                    adrs:       slh_adrs(.sch             (sch),
                                         .ctrl            (ctrl),
                                         .adrs_type       (slh_pkg::ADRS_WOTS_HASH),
                                         .chain_or_height (slh_pkg::ADRS_WORD_W'(ctrl.chain_idx)),
                                         .hash_or_index   (slh_pkg::ADRS_WORD_W'(ctrl.hash_idx))),
                    m1:         slh_pkg::SLH_NW'(tmp)});
            default:         return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // OTS public-key hash: the prefix the chain endpoints are accumulated
    // behind
    // -------------------------------------------------------------------------

    function automatic int unsigned ots_pk_prefix_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return $bits(hss_pkg::lms_pk_prefix_t);
            SCHEME_SLH_128S: return $bits(slh_pkg::slh_t_prefix_t);
            default:         return 0;
        endcase
    endfunction

    function automatic int unsigned ots_pk_prefix_bits_max();
        ots_pk_prefix_bits_max = 0;
        for (int i = 0; i < SCHEME_MAX; i++) begin
            ots_pk_prefix_bits_max = max2(ots_pk_prefix_bits_max, ots_pk_prefix_bits(sch_e'(i)));
        end
    endfunction

    localparam int unsigned MAX_OTS_PK_PREFIX_BITS = ots_pk_prefix_bits_max();

    function automatic logic [MAX_OTS_PK_PREFIX_BITS-1:0] ots_pk_prefix(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl);
        case (sch)
            SCHEME_HSS:      return MAX_OTS_PK_PREFIX_BITS'(hss_pkg::lms_pk_prefix_t'{
                    i:      kctx,
                    q:      ctrl2q(ctrl),
                    d_pblc: hss_pkg::D_PBLC});
            SCHEME_SLH_128S: return MAX_OTS_PK_PREFIX_BITS'(slh_pkg::slh_t_prefix_t'{
                    seed_block: slh_seed_block(kctx),
                    adrs:       slh_adrs(.sch             (sch),
                                         .ctrl            (ctrl),
                                         .adrs_type       (slh_pkg::ADRS_WOTS_PK),
                                         .chain_or_height ('0),
                                         .hash_or_index   ('0))});
            default:         return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Accumulation of a public key (the OTS one over the chain endpoints, the
    // FORS one over the tree roots)
    //
    // The elements are hashed behind the prefix in SHA-256 blocks, so every
    // block boundary falls somewhere inside an element. The verifier banks
    // elements until one straddles a boundary, absorbs what is complete with
    // the top of that element, and carries the rest of it into the next
    // block. The first absorb covers the prefix and the elements up to the
    // first boundary after it; every later one is a single block of a carry,
    // whole elements and a top. The final block holds the last carry, the
    // elements still banked, and the padding.
    // -------------------------------------------------------------------------

    // Blocks of the first absorb, and the bits of its last block left for
    // elements
    function automatic int unsigned acc_first_blocks(input sch_e sch);
        return ots_pk_prefix_bits(sch) / sha2_wrap_pkg::SHA256_BLOCK_W + 1;
    endfunction
    function automatic int unsigned acc_first_room(input sch_e sch);
        return sha2_wrap_pkg::SHA256_BLOCK_W
             - ots_pk_prefix_bits(sch) % sha2_wrap_pkg::SHA256_BLOCK_W;
    endfunction
    // Elements banked before the first absorb, and before each later one
    function automatic int unsigned acc_first_full(input sch_e sch);
        return acc_first_room(sch) / digest_w(sch);
    endfunction
    function automatic int unsigned acc_mid_full(input sch_e sch);
        return sha2_wrap_pkg::SHA256_BLOCK_W / digest_w(sch) - 1;
    endfunction
    // Bits of the element closing a block that fit in it, and the rest
    function automatic int unsigned acc_top_w(input sch_e sch);
        return acc_first_room(sch) % digest_w(sch);
    endfunction
    function automatic int unsigned acc_carry_w(input sch_e sch);
        return digest_w(sch) - acc_top_w(sch);
    endfunction
    // Elements still banked when a stream of count elements ends
    function automatic int unsigned acc_tail_elems(input sch_e sch, input int unsigned count);
        if (count <= acc_first_full(sch)) return count;
        return (count - 1 - acc_first_full(sch)) % (acc_mid_full(sch) + 1);
    endfunction

    // -------------------------------------------------------------------------
    // Merkle leaf: the leaf hash over the OTS public key
    // -------------------------------------------------------------------------

    localparam int unsigned MSS_LEAF_MSG_BITS = $bits(hss_pkg::lms_leaf_msg_t);

    function automatic logic [MSS_LEAF_MSG_BITS-1:0] mss_leaf_msg(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl,
        input logic [MAX_DATA_W-1:0] kc);
        case (sch)
            SCHEME_HSS:      return hss_pkg::lms_leaf_msg_t'{
                    i:      kctx,
                    q:      ctrl2q(ctrl),
                    d_leaf: hss_pkg::D_LEAF,
                    kc:     kc};
            SCHEME_SLH_128S: return '0;
            default:         return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Merkle interior node: the parent hash over a left and a right child
    // -------------------------------------------------------------------------

    function automatic int unsigned mss_join_msg_bits(input sch_e sch);
        case (sch)
            SCHEME_HSS:      return $bits(hss_pkg::lms_intr_msg_t);
            SCHEME_SLH_128S: return $bits(slh_pkg::slh_h_msg_t);
            default:         return 0;
        endcase
    endfunction

    function automatic int unsigned mss_join_msg_bits_max();
        mss_join_msg_bits_max = 0;
        for (int i = 0; i < SCHEME_MAX; i++) begin
            mss_join_msg_bits_max = max2(mss_join_msg_bits_max, mss_join_msg_bits(sch_e'(i)));
        end
    endfunction

    localparam int unsigned MAX_MSS_JOIN_MSG_BITS = mss_join_msg_bits_max();

    function automatic logic [MAX_MSS_JOIN_MSG_BITS-1:0] mss_join_msg(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl,
        input logic [MAX_DATA_W-1:0] left,
        input logic [MAX_DATA_W-1:0] right);
        case (sch)
            SCHEME_HSS:      return MAX_MSS_JOIN_MSG_BITS'(hss_pkg::lms_intr_msg_t'{
                    i:      kctx,
                    node:   ctrl2node(sch, ctrl),
                    d_intr: hss_pkg::D_INTR,
                    left:   left,
                    right:  right});
            SCHEME_SLH_128S: return MAX_MSS_JOIN_MSG_BITS'(slh_pkg::slh_h_msg_t'{
                    seed_block: slh_seed_block(kctx),
                    adrs:       slh_adrs(.sch             (sch),
                                         .ctrl            (ctrl),
                                         .adrs_type       (slh_pkg::ADRS_TREE),
                                         .chain_or_height (ctrl2height(ctrl)),
                                         .hash_or_index   (ctrl2tree_index(ctrl))),
                    left:       slh_pkg::SLH_NW'(left),
                    right:      slh_pkg::SLH_NW'(right)});
            default:         return '0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // FORS: the index of the leaf a tree reveals (its digit of the signand,
    // the most significant first), the leaf hash over the secret element,
    // the parent hash, and the prefix the tree roots are accumulated behind
    // (as wide as the OTS one, the accumulation is shared)
    // -------------------------------------------------------------------------

    function automatic logic [LEAF_IDX_W-1:0] fors_leaf_idx(
        input sch_e                     sch,
        input logic [MAX_SIGNAND_W-1:0] signand,
        input logic [MRKL_TREE_W-1:0]   fors_tree);
        logic [slh_pkg::SLH_A-1:0] slh_digit;
        slh_digit = '0;
        for (int unsigned i = 0; i < slh_pkg::SLH_K; i++) begin
            if (fors_tree == MRKL_TREE_W'(i)) begin
                slh_digit = signand[slh_pkg::SLH_MD_W-1 - slh_pkg::SLH_A*i -: slh_pkg::SLH_A];
            end
        end
        case (sch)
            SCHEME_HSS:      return '0;
            SCHEME_SLH_128S: return LEAF_IDX_W'(slh_digit);
            default:         return '0;
        endcase
    endfunction

    localparam int unsigned FORS_LEAF_MSG_BITS = $bits(slh_pkg::slh_f_msg_t);

    function automatic logic [FORS_LEAF_MSG_BITS-1:0] fors_leaf_msg(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl,
        input logic [MAX_DATA_W-1:0] secret);
        case (sch)
            SCHEME_HSS:      return '0;
            SCHEME_SLH_128S: return slh_pkg::slh_f_msg_t'{
                    seed_block: slh_seed_block(kctx),
                    adrs:       slh_adrs(.sch             (sch),
                                         .ctrl            (ctrl),
                                         .adrs_type       (slh_pkg::ADRS_FORS_TREE),
                                         .chain_or_height ('0),
                                         .hash_or_index   (ctrl2fors_leaf(ctrl))),
                    m1:         slh_pkg::SLH_NW'(secret)};
            default:         return '0;
        endcase
    endfunction

    localparam int unsigned FORS_JOIN_MSG_BITS = $bits(slh_pkg::slh_h_msg_t);

    function automatic logic [FORS_JOIN_MSG_BITS-1:0] fors_join_msg(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl,
        input logic [MAX_DATA_W-1:0] left,
        input logic [MAX_DATA_W-1:0] right);
        case (sch)
            SCHEME_HSS:      return '0;
            SCHEME_SLH_128S: return slh_pkg::slh_h_msg_t'{
                    seed_block: slh_seed_block(kctx),
                    adrs:       slh_adrs(.sch             (sch),
                                         .ctrl            (ctrl),
                                         .adrs_type       (slh_pkg::ADRS_FORS_TREE),
                                         .chain_or_height (ctrl2height(ctrl)),
                                         .hash_or_index   (ctrl2fors_index(ctrl))),
                    left:       slh_pkg::SLH_NW'(left),
                    right:      slh_pkg::SLH_NW'(right)};
            default:         return '0;
        endcase
    endfunction

    function automatic logic [MAX_OTS_PK_PREFIX_BITS-1:0] fors_pk_prefix(
        input sch_e                  sch,
        input logic [MAX_KCTX_W-1:0] kctx,
        input ctrl_t                 ctrl);
        case (sch)
            SCHEME_HSS:      return '0;
            SCHEME_SLH_128S: return MAX_OTS_PK_PREFIX_BITS'(slh_pkg::slh_t_prefix_t'{
                    seed_block: slh_seed_block(kctx),
                    adrs:       slh_adrs(.sch             (sch),
                                         .ctrl            (ctrl),
                                         .adrs_type       (slh_pkg::ADRS_FORS_ROOTS),
                                         .chain_or_height ('0),
                                         .hash_or_index   ('0))});
            default:         return '0;
        endcase
    endfunction

    /* verilator lint_on UNUSEDSIGNAL */

endpackage
