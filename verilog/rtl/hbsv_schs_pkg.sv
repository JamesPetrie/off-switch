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
    // Scheme parameters
    // -------------------------------------------------------------------------

    // Node / signature-element / licence-beat width
    function automatic int unsigned digest_w(input sch_e s);
        case (s)
            SCHEME_LMS: return arith_pkg::WIDTH;                            // 256
            default:    return 0;
        endcase
    endfunction

    // Winternitz digit width and chain count (data digits, checksum digits)
    function automatic int unsigned digit_w(input sch_e s);
        case (s)
            SCHEME_LMS: return hss_pkg::WOTS_W;                             // 8
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len1(input sch_e s);
        case (s)
            SCHEME_LMS: return hss_pkg::WOTS_P1;                            // 32
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len2(input sch_e s);
        case (s)
            SCHEME_LMS: return hss_pkg::WOTS_P2;                            // 2
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned ots_len(input sch_e s);
        case (s)
            SCHEME_LMS: return hss_pkg::WOTS_P;                             // 34
            default:    return 0;
        endcase
    endfunction
    // Left shift of the 16-bit checksum before its digits are taken
    // (RFC 8554 ls = 16 - len2 * w)
    function automatic int unsigned csum_shift(input sch_e s);
        return 16 - ots_len2(s) * digit_w(s);                               // 0
    endfunction

    // Hypertree: number of layers, tree height, and the level counter width
    function automatic int unsigned layers(input sch_e s);
        case (s)
            SCHEME_LMS: return hss_pkg::HSS_LEVELS;                         // 2
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned tree_h(input sch_e s);
        case (s)
            SCHEME_LMS: return hss_pkg::TREE_H;                             // 5
            default:    return 0;
        endcase
    endfunction
    function automatic int unsigned level_w(input sch_e s);
        case (s)
            SCHEME_LMS: return $clog2(hss_pkg::TREE_H_MAX);                 // 5
            default:    return 1;
        endcase
    endfunction

    // Header beats at the start of each layer's signature
    function automatic int unsigned hdr_beats(input sch_e s);
        case (s)
            SCHEME_LMS: return hss_pkg::LAYER_HDR_BEATS;                    // 2
            default:    return 0;
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Hash-message widths
    // -------------------------------------------------------------------------

    localparam int unsigned MAX_MSG_HASH_BITS  = $bits(hss_pkg::lms_q_sub_msg_t);  // 880
    localparam int unsigned MAX_CHAIN_MSG_BITS = $bits(hss_pkg::lms_chain_msg_t);  // 440
    localparam int unsigned MAX_LEAF_MSG_BITS  = $bits(hss_pkg::lms_leaf_msg_t);   // 432
    localparam int unsigned MAX_TREE_MSG_BITS  = $bits(hss_pkg::lms_intr_msg_t);   // 688

    // Prefix of the OTS public-key accumulation (LMS: I || q || D_PBLC)
    localparam int unsigned ACC_PREFIX_W = $bits(hss_pkg::lms_prefix_t);          // 176

    // -------------------------------------------------------------------------
    // Endpoint accumulation geometry (LMS Kc)
    //
    // The prefix is 22 bytes, so with E-byte elements a 64-byte block
    // boundary falls (42 mod E) bytes into an element: every absorbed block
    // ends with that element head and leaves the rest of the element as the
    // carry into the next block. The first block holds the prefix and
    // (64-22)/E full elements, later blocks the carry and 64/E - 1 full
    // elements; the final padding block holds the carry and whatever
    // elements are still banked when the stream ends.
    // -------------------------------------------------------------------------

    function automatic int unsigned acc_first_full(input sch_e s);
        return (512 - ACC_PREFIX_W) / digest_w(s);                          // 1
    endfunction
    function automatic int unsigned acc_mid_full(input sch_e s);
        return 512 / digest_w(s) - 1;                                       // 1
    endfunction
    function automatic int unsigned acc_head_w(input sch_e s);
        return (512 - ACC_PREFIX_W) % digest_w(s);                          // 80
    endfunction
    function automatic int unsigned acc_carry_w(input sch_e s);
        return digest_w(s) - acc_head_w(s);                                 // 176
    endfunction
    // Elements still banked when a stream of n elements ends: absorbs fall
    // at element acc_first_full, then every acc_mid_full + 1 elements.
    function automatic int unsigned acc_tail_elems(input sch_e s, input int unsigned n);
        if (n <= acc_first_full(s)) return n;
        return (n - 1 - acc_first_full(s)) % (acc_mid_full(s) + 1);        // 0
    endfunction

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
