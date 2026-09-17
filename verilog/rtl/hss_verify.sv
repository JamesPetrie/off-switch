// Hash-based Signature Verification: RFC 8554 HSS/LMS, or FIPS 205 SLH-DSA
// on the same structure (see the SLH-DSA paragraph below).
//
// Single-module implementation of RFC 8554 HSS/LMS verification.
// One SHA-256 core shared by all phases, sequenced by a main FSM:
//
//   Sequencer  — phases: Idle → Q → Wots → KcFinal → Leaf → Merkle → Done
//   Q          — hash for message digest Q
//   WOTS       — hash WOTS chains forward to their public keys (sub-FSM),
//                folding each finished pk into the Kc hash as it appears
//   KcFinal    — resume the Kc hash one final time for the padding block
//   Leaf       — hash for leaf node
//   Merkle     — walk auth path from leaf to root (sub-FSM)
//
// Kc accumulation is interleaved with the WOTS chains via the SHA wrapper's
// save/restore feature: whenever two more chain endpoints complete a full
// 512-bit block of the Kc message, that block is absorbed into the suspended
// Kc hash and the running state is saved again (256 bits) while chain hashing
// continues. This replaces storing all OTS_LEN endpoints (34 x 256 bits) with
// one saved state, one banked endpoint and two partial-block carries
// (current and staged).
//
// Note: Deviation from the standard!
// Verification runs bottom-up: start at layer LAYERS-1 (leaf tree that
// signs the user message), and on each mrkl_complete either move up one layer
// (restart Q→...→Merkle with hash_reg_q carrying the just-computed root as
// the next layer's signed-message input) or, at layer 0, compare the result
// against ROOT_PUB_KEY. Intermediate root consistency is verified implicitly
// by each upper layer's WOTS+Merkle succeeding with that root as its Q input.
// This is the opposite direction of the standard but allows area saving.
//
// SLH-DSA (SCH = SCHEME_SLH_128S) runs the same sequencer on the FIPS 205
// layouts from hbsv_schs_pkg: Q becomes H_msg (an inner hash, then MGF1 in
// StMgf1); the FORS phase (StFors) runs each of the k trees through the
// WOTS and Merkle sub-FSMs -- one F step, an a-level auth path, the root
// banked into the T_k accumulation; T_len takes the place of Kc with no
// leaf hash after it; and every F, H and T call resumes from the
// precomputed midstate of the constant first block PK.seed || 0^48.
//
// Protocol:
//   1. Hold message stable for the whole verification
//   2. Supply the license on valid/ready/data, one beat per accepted cycle.
//      The first beat starts the verification; there is no separate start
//      signal. ready is only asserted while a beat is actually wanted, so the
//      state machine keeps running between beats (hashing does not stall).
//   3. verify_done pulses high for one cycle when verification completes
//   4. With verify_done, check verif_passed: 1 = valid, 0 = invalid

module hss_verify
    import arith_pkg::*;
    import hbsv_ctrl_pkg::*;
    import hbsv_schs_pkg::*;
#(
    // Signature scheme; every constant and message layout below is a
    // function of it
    parameter sch_e SCH = SCHEME_HSS,

    // Node, signature-element and licence-beat width
    localparam int unsigned DW     = digest_w(SCH),
    localparam int unsigned KCTX_W = kctx_w(SCH)
) (
    input  logic               clk,
    input  logic               rst_n,
    input  logic [WIDTH-1:0]   message,
    // TODO replace individual public key inputs with the struct
    input  logic [KCTX_W-1:0]  identifier,   // HSS: tree identifier I; SLH: PK.seed
    input  logic [DW-1:0]      root_pub_key, // HSS: root public key; SLH: PK.root
    /* verilator lint_off UNUSEDSIGNAL */
    input  logic [255:0]       midstate,     // SLH: SHA-256 state of PK.seed || 0^48 (HSS: unread)
    /* verilator lint_on UNUSEDSIGNAL */

    // License beat stream, in the field order of the standard signature
    // format (see hss_pkg). Per layer, from LAYERS-1 down to 0: a header
    // beat carrying {leaf_index, sub_I}, the randomizer, OTS_LEN chain
    // signatures, then TREE_HT auth path siblings. Each beat is consumed where
    // it is needed, so only the current layer's identity is held.
    // SLH (see slh_pkg): R, the FORS elements, then per layer the chain
    // elements and the auth path siblings. R and the siblings are read
    // straight into hash blocks and released when the core captures the
    // last block that reads them (sha2_wrap's taken).
    input  logic               valid,
    output logic               ready,
    input  logic [DW-1:0]      data,

    output logic               verify_done,
    output logic               verif_passed
);

    // -------------------------------------------------------------------------
    // Scheme constants
    // -------------------------------------------------------------------------

    localparam int unsigned LAYERS   = layers(SCH);     // hypertree layers
    localparam int unsigned TREE_HT  = tree_h(SCH);     // Merkle tree height
    localparam int unsigned OTS_LEN1 = ots_len1(SCH);   // WOTS data digits
    localparam int unsigned OTS_LEN  = ots_len(SCH);    // WOTS chains (data + checksum)
    localparam int unsigned DIGIT_W  = digit_w(SCH);    // Winternitz digit width

    localparam int unsigned OTS_LEN2 = ots_len2(SCH);   // WOTS checksum digits
    localparam int unsigned CSUM_W   = OTS_LEN2 * DIGIT_W;

    // WOTS digit maximum value (all 1s)
    localparam logic [DIGIT_W-1:0] DIGIT_MAX = '1;

    localparam bit          IS_HSS    = (SCH == SCHEME_HSS);
    localparam bit          IS_SLH    = (SCH == SCHEME_SLH_128S);
    localparam bit          LEAF_HASH = has_mss_leaf_hash(SCH); // Kc hashed once more for the leaf
    localparam bit          MIDSTATE  = resumes_from_midstate(SCH); // F/H/T start from that block
    localparam int unsigned MS_BITS   = midstate_bits(SCH);     // length-field bits of that block
    localparam int unsigned FORS_K    = fors_k(SCH);            // FORS trees (SLH)
    localparam int unsigned FORS_H    = fors_h(SCH);            // FORS tree height (SLH)
    localparam int unsigned MSG_W     = msg_reg_w(SCH);         // message-being-signed register

    // FORS digit width and the last tree and level, sized for HSS too
    localparam int unsigned FORS_DIGIT_W = (FORS_H > 0) ? FORS_H : 1;
    localparam int unsigned FORS_K_LAST  = (FORS_K > 0) ? FORS_K - 1 : 0;
    localparam int unsigned FORS_H_LAST  = (FORS_H > 0) ? FORS_H - 1 : 0;

    // -------------------------------------------------------------------------
    // FSM state types
    // -------------------------------------------------------------------------

    // StMgf1 and StFors are SLH-only: the second H_msg hash and the FORS phase
    typedef enum logic [3:0] {
        StIdle, StQ, StMgf1, StFors, StWots, StKcFinal, StLeaf, StMerkle, StDone
    } seq_state_e;

    // Header beats per layer: {leaf_index, sub_I} then the randomizer.
    localparam int unsigned HDR_BEATS = hdr_beats(SCH);
    localparam int unsigned HDR_CNT_W = (HDR_BEATS > 0) ? $clog2(HDR_BEATS + 1) : 1;
    localparam logic [HDR_CNT_W-1:0] HDR_DONE = HDR_CNT_W'(HDR_BEATS);

    typedef enum logic [1:0] {
        StWotsInit, StWotsLoad, StWotsHash, StWotsAccum
    } wots_state_e;


    typedef enum logic {
        StMrklInit, StMrklHash
    } mrkl_state_e;

    // -------------------------------------------------------------------------
    // Kc interleaving geometry
    //
    // The Kc message is the prefix (I || q || D_PBLC; SLH: ADRSc) followed by
    // the OTS_LEN chain endpoints; hbsv_schs_pkg derives the block geometry
    // from the endpoint width and documents it. Endpoints are banked until
    // one closes a block: KC_FIRST of them after the prefix, KC_MID after the
    // carry of the previous block. The endpoint closing a block contributes
    // its KC_TOP_W head bits and leaves its tail as the next carry. The
    // final padding block carries the last carry and the KC_TAIL endpoints
    // still banked. SLH runs the accumulation twice per layer set, over the
    // FORS roots (T_k) and over the chain endpoints (T_len).
    // -------------------------------------------------------------------------

    localparam int unsigned KC_PREFIX_W  = ACC_PREFIX_W;
    localparam int unsigned KC_FIRST     = acc_first_full(SCH);
    localparam int unsigned KC_MID       = acc_mid_full(SCH);
    localparam int unsigned KC_TOP_W     = acc_head_w(SCH);
    localparam int unsigned KC_CARRY_W   = acc_carry_w(SCH);
    localparam int unsigned KC_TAIL      = acc_tail_elems(.sch (SCH), .count (OTS_LEN));
    localparam int unsigned KC_TAIL_FORS = acc_tail_elems(.sch (SCH), .count (FORS_K));
    localparam int unsigned KC_POS_W     = $clog2(KC_MID + 1);

    // -------------------------------------------------------------------------
    // Registers
    // -------------------------------------------------------------------------

    seq_state_e   seq_q,   seq_d;

    // Current layer's identity, taken from the header beat. sub_I is kept one
    // layer deep: the layer above signs the public key of the one below, so
    // its Q hash needs the identifier this layer used.
    //
    // REVISIT: the randomizer avoids storage by borrowing aux_reg, but neither
    // identifier can do the same. Q_SUB_DATA needs prev_I alongside aux_reg
    // (the randomizer) and hash_reg (the root from the layer below) in one
    // hash input, and cur_I parameterises every hash of the layer, so the only
    // registers idle at that point are the Kc accumulation ones (kc_lo would
    // fit). Worth another look if HSS-LMS is picked up again.
    logic [LEAF_IDX_W-1:0]  leaf_index_q, leaf_index_d;
    logic [KCTX_W-1:0]      cur_I_q,      cur_I_d;
    logic [KCTX_W-1:0]      prev_I_q,     prev_I_d;
    logic [HDR_CNT_W-1:0]   hdr_cnt_q,    hdr_cnt_d;
    wots_state_e  wots_q,  wots_d;
    mrkl_state_e  mrkl_q,  mrkl_d;

    // SLH: the hypertree index idx_tree, shifted down a layer at a time, and
    // the FORS-phase flag that routes the WOTS and Merkle sub-FSMs.
    logic [TREE_IDX_W-1:0]  tree_idx_q,   tree_idx_d;
    logic                   fors_q,       fors_d;

    // Hash register — working hash output across all phases
    logic [DW-1:0]    hash_reg_q,    hash_reg_d;

    // Auxiliary register — companion value alongside hash_reg
    // WOTS: holds the message being signed (HSS the Q hash; SLH md, then the
    // FORS public key, then the root of the layer below)
    logic [MSG_W-1:0] aux_reg_q,     aux_reg_d;

    // Shared block counter — indexes SHA-256 blocks within a multi-block hash
    // REVISIT hardcoded widhts
    logic [4:0]       blk_idx_q,     blk_idx_d;

    // WOTS counters (driven by WOTS sub-FSM)
    logic [CHAIN_IDX_W-1:0] wots_chain_q, wots_chain_d; // chain index 0..OTS_LEN-1
    logic [HASH_IDX_W-1:0]  wots_step_q,  wots_step_d;  // step within chain

    // Merkle tree level (driven by Merkle sub-FSM)
    logic [MRKL_LEVEL_W-1:0] mrkl_level_q, mrkl_level_d;

    // Kc interleaved accumulation (replaces the former 34 x 256-bit pk store):
    // suspended SHA state (SLH: also parks the H_msg inner digest), the
    // banked endpoints, the current carry and the staged next carry (the
    // tail of the endpoint that closed the block), the bank position and a
    // first-block flag.
    //
    // REVISIT: kc_tail stages the next carry so it is not read back from
    // hash_reg after the absorb, the one spot that would otherwise rely on
    // the digest being registered twice (core and verifier — see the
    // design-doc limitation). Revisit together with that limitation.
    logic [255:0]           kc_state_q;
    logic [DW-1:0]          kc_lo_q [KC_MID];
    logic [KC_CARRY_W-1:0]  kc_hi_q;
    logic [KC_CARRY_W-1:0]  kc_tail_q;
    logic [KC_POS_W-1:0]    kc_pos_q,   kc_pos_d;    // endpoints banked since the last absorb
    logic                   kc_first_q, kc_first_d;  // no block absorbed yet

    // Hypertree layer counter
    logic [HT_LAYER_W-1:0] layer_q, layer_d;

    // -------------------------------------------------------------------------
    // SHA-256 wrapper instance
    // -------------------------------------------------------------------------

    logic         sha_valid;
    logic [511:0] sha_block;
    logic         sha_last;
    wire          sha_ready;
    wire          sha_taken;
    wire  [255:0] sha_digest;

    logic         sha_save;
    logic         sha_restore;
    logic [255:0] sha_ctx;

    sha2_wrap u_sha256 (
        .clk     (clk),
        .rst_n   (rst_n),
        .valid   (sha_valid),
        .block   (sha_block),
        .last    (sha_last),
        .save    (sha_save),
        .restore (sha_restore),
        .ctx     (sha_ctx),
        .ready   (sha_ready),
        .taken   (sha_taken),
        .digest  (sha_digest)
    );

    wire hash_complete = sha_last && sha_ready;

    // Trunc_n: the digest's leftmost bytes (all of them for HSS)
    wire [DW-1:0] trunc_digest = sha_digest[255 -: DW];

    // SLH: the H_msg digest split into md and the two indices
    wire hmsg_split_t hmsg = slh_hmsg_split(.digest (sha_digest));

    // The node at the width the shared scheme functions take it,
    // left-aligned; the SLH-only functions take the node width directly
    logic [MAX_DATA_W-1:0] hash_reg_wide;
    logic [MSG_W-1:0]      aux_from_data, aux_from_hash;
    if (DW == MAX_DATA_W) begin : gen_full_width
        assign hash_reg_wide = hash_reg_q;
    end else begin : gen_widen
        assign hash_reg_wide = {hash_reg_q, {(MAX_DATA_W-DW){1'b0}}};
    end

    // HSS header beat: {leaf_index, sub_I} at the front of the beat
    logic [LEAF_IDX_W-1:0] hdr_leaf_idx;
    logic [KCTX_W-1:0]     hdr_sub_ident;
    if (IS_HSS) begin : gen_hdr_fields
        assign hdr_leaf_idx  = data[DW-1 -: LEAF_IDX_W];
        assign hdr_sub_ident = data[DW-1-LEAF_IDX_W -: KCTX_W];
    end else begin : gen_no_hdr
        assign hdr_leaf_idx  = '0;
        assign hdr_sub_ident = '0;
    end
    if (MSG_W == DW) begin : gen_aux_same
        assign aux_from_data = data;
        assign aux_from_hash = hash_reg_q;
    end else begin : gen_aux_widen
        assign aux_from_data = {data,       {(MSG_W-DW){1'b0}}};
        assign aux_from_hash = {hash_reg_q, {(MSG_W-DW){1'b0}}};
    end

    // -------------------------------------------------------------------------
    // Per-layer selectors
    // -------------------------------------------------------------------------

    // Hypertree layer signing the message (bottom)
    wire is_msg_layer = (int'(layer_q) == LAYERS - 1);
    // Hypetree layer corresponing to the Public Key (top)
    wire is_pk_layer  = (layer_q == '0);

    // Top-tree identifier is the package constant; lower trees carry theirs
    // in the license as sub_I[lv] (≥1). sub_I[0] is unused for the top layer.
    // SLH's PK.seed is the port throughout.
    wire [KCTX_W-1:0] cur_I = (IS_HSS && !is_pk_layer) ? cur_I_q
                                                       : identifier;

    // -------------------------------------------------------------------------
    // Control bundle — the counters in the form the hash messages read them
    // -------------------------------------------------------------------------

    wire ctrl_t ctrl = '{ht_layer:   layer_q,
                         chain_idx:  wots_chain_q,
                         hash_idx:   wots_step_q,
                         mrkl_level: mrkl_level_q,
                         leaf_idx:   leaf_index_q,
                         tree_idx:   tree_idx_q};

    // -------------------------------------------------------------------------
    // Data indexed by WOTS chain / Merkle level
    // -------------------------------------------------------------------------

    // In the FORS phase the chain index counts trees and the level counter
    // walks the FORS tree height.
    wire             last_chain    = fors_q ? (int'(wots_chain_q) == FORS_K_LAST)
                                            : (int'(wots_chain_q) == OTS_LEN-1);

    wire             last_level    = fors_q ? (int'(mrkl_level_q) == FORS_H_LAST)
                                            : (int'(mrkl_level_q) == TREE_HT-1);

    wire             mrkl_hashing  = (seq_q == StMerkle) && (mrkl_q == StMrklHash);

    // -------------------------------------------------------------------------
    // Q hash split into digits + checksum — computed combinationally
    // -------------------------------------------------------------------------

    logic [DIGIT_W-1:0] q_digits[OTS_LEN];

    // Using digit-wise shift left to avoid indexing issues
    always_comb begin
        logic [MSG_W-1:0]  hash;    // hash working variable
        logic [CSUM_W-1:0] csum;    // checksum working variable

        hash  = aux_reg_q;
        csum = '0;

        // Load the digits from q_hash and calculate the checksum
        for (int i = 0; i < OTS_LEN1; i++) begin

            // load the digit
            // shift hash left one digit, shift out to q_digits and shift in zeros
            {q_digits[i], hash} = {hash, DIGIT_W'(0)};

            // add the digit's contribution to the checksum
            csum += CSUM_W'(DIGIT_MAX) - CSUM_W'(q_digits[i]);
        end

        // Load the checksum digits
        for (int i = OTS_LEN1; i < OTS_LEN; i++) begin
            // shift csum left one digit, shift out to q_digits and shift in zeros
            {q_digits[i], csum} = {csum, DIGIT_W'(0)};
        end
    end

    localparam int unsigned CHAIN_SEL_W = $clog2(OTS_LEN);
    wire [DIGIT_W-1:0] cur_digit = q_digits[wots_chain_q[CHAIN_SEL_W-1:0]];

    // SLH FORS: the digit of tree wots_chain_q, an a-bit slice of md (most
    // significant first), and that leaf's index in the forest,
    // tree * 2^a + digit
    if (IS_SLH) begin : gen_fors
        logic [FORS_DIGIT_W-1:0] fors_digit;
        always_comb begin
            fors_digit = '0;
            for (int i = 0; i < int'(FORS_K); i++) begin
                if (int'(wots_chain_q) == i) begin
                    fors_digit = aux_reg_q[MSG_W-1 - FORS_DIGIT_W*i -: FORS_DIGIT_W];
                end
            end
        end
        wire [FORS_LEAF_IDX_W-1:0] fors_leaf_idx =
                (FORS_LEAF_IDX_W'(wots_chain_q) << FORS_H) | FORS_LEAF_IDX_W'(fors_digit);
    end

    // -------------------------------------------------------------------------
    // SHA-256 hash inputs — continuous padded bitvectors
    //
    // Each message is built by hbsv_schs_pkg for the scheme SCH, narrowed
    // to the scheme's width, then padded.
    // -------------------------------------------------------------------------

    // Hash input padding
    // SHA256 requires the last block (even if only 1 block is used) to have the following padding:
    //   - 1 bit '1', right after the data
    //   - 0 bits until the last 64 bits of the block (number of 0 padding can be zero)
    //   - The last 64 bits are the length of the data in bits
    // If the padding doesn't fit in the last data block, an additional block is added.

    localparam int unsigned SHA_PAD_OVERHEAD = 1 + 64;

    function automatic int unsigned calc_sha_blocks(input int unsigned data_bits);
        return (data_bits + SHA_PAD_OVERHEAD + 511) / 512; // round up to nearest block
    endfunction
    function automatic int unsigned calc_sha_pad_zeros(input int unsigned data_bits);
        return (calc_sha_blocks(data_bits) * 512) - (data_bits + SHA_PAD_OVERHEAD);
    endfunction

    // -------------------------------------------------------------------------
    // Q: H(I || q || D_MESG || C || <signed payload>)
    //
    // Message layer (is_msg_layer):   signed payload = user message (1 block)
    // Upper layers:                   signed payload = serialised pub[lv+1]
    //                                 = LMS_TYPE || LMOTS_TYPE || sub_I[lv+1] || T[1]
    //                                 where T[1] lives in hash_reg_q (the root
    //                                 just computed by the layer below)
    // -------------------------------------------------------------------------

    localparam int unsigned Q_MSG_W = msg_hash_msg_bits(SCH);
    localparam int unsigned Q_SUB_W = SUB_PK_HASH_MSG_BITS;
    localparam int unsigned MGF1_W  = SLH_MGF1_MSG_BITS;

    // HSS: Q over the randomizer in aux_reg. SLH: the inner hash of H_msg
    // over R (on the bus) and the public key, then (StMgf1) MGF1 over R and
    // the inner digest parked in kc_state.
    logic [Q_MSG_W-1:0] q_msg_data;
    logic [Q_SUB_W-1:0] q_sub_data;
    logic [MGF1_W-1:0]  mgf1_data;
    if (IS_HSS) begin : gen_msg_hash_hss
        assign q_msg_data = Q_MSG_W'(msg_hash_msg(.sch        (SCH),
                                                  .kctx       (cur_I),
                                                  .ctrl       (ctrl),
                                                  .randomizer (aux_reg_q),
                                                  .message    (message)));
        // sub_I is indexed at layer_q+1 (identity of the tree below)
        assign q_sub_data = sub_pk_hash_msg(.kctx       (cur_I),
                                            .ctrl       (ctrl),
                                            .randomizer (aux_reg_q),
                                            .sub_kctx   (prev_I_q),
                                            .sub_root   (hash_reg_q));
        assign mgf1_data  = '0;
    end else begin : gen_msg_hash_slh
        assign q_msg_data = Q_MSG_W'(slh_hmsg_msg(.kctx       (cur_I),
                                                  .pk_root    (root_pub_key),
                                                  .randomizer (data),
                                                  .message    (message)));
        assign q_sub_data = '0;
        assign mgf1_data  = MGF1_W'(slh_mgf1_msg(.kctx         (cur_I),
                                                 .randomizer   (data),
                                                 .inner_digest (kc_state_q)));
    end

    localparam int unsigned Q_MSG_BLOCKS    = calc_sha_blocks($bits(q_msg_data));
    localparam int unsigned Q_MSG_PAD_ZEROS = calc_sha_pad_zeros($bits(q_msg_data));
    localparam int unsigned Q_SUB_BLOCKS    = calc_sha_blocks($bits(q_sub_data));
    localparam int unsigned Q_SUB_PAD_ZEROS = calc_sha_pad_zeros($bits(q_sub_data));
    localparam int unsigned MGF1_BLOCKS     = calc_sha_blocks($bits(mgf1_data));
    localparam int unsigned MGF1_PAD_ZEROS  = calc_sha_pad_zeros($bits(mgf1_data));

    wire [Q_MSG_BLOCKS*512-1:0] q_msg_padded =
            {q_msg_data, 1'b1, {Q_MSG_PAD_ZEROS{1'b0}}, 64'($bits(q_msg_data))};
    wire [Q_SUB_BLOCKS*512-1:0] q_sub_padded =
            {q_sub_data, 1'b1, {Q_SUB_PAD_ZEROS{1'b0}}, 64'($bits(q_sub_data))};
    wire [MGF1_BLOCKS*512-1:0]  mgf1_padded =
            {mgf1_data, 1'b1, {MGF1_PAD_ZEROS{1'b0}}, 64'($bits(mgf1_data))};

    // -------------------------------------------------------------------------
    // WOTS chain: H(I || q || i || j || tmp); SLH F(ADRSc || tmp), and in the
    // FORS phase the leaf F(ADRSc || sk). With a midstate the length field
    // counts the precomputed block too.
    // -------------------------------------------------------------------------

    localparam int unsigned WOTS_MSG_W = ots_chain_msg_bits(SCH);

    logic [WOTS_MSG_W-1:0] wots_data;
    if (IS_HSS) begin : gen_chain_hss
        assign wots_data = WOTS_MSG_W'(ots_chain_msg(.sch  (SCH),
                                                     .kctx (cur_I),
                                                     .ctrl (ctrl),
                                                     .tmp  (hash_reg_q)));
    end else begin : gen_chain_slh
        assign wots_data = fors_q
                ? WOTS_MSG_W'(fors_leaf_msg(.sch       (SCH),
                                            .ctrl      (ctrl),
                                            .fors_leaf (gen_fors.fors_leaf_idx),
                                            .secret    (hash_reg_q)))
                : WOTS_MSG_W'(ots_chain_msg(.sch  (SCH),
                                            .kctx (cur_I),
                                            .ctrl (ctrl),
                                            .tmp  (hash_reg_wide)));
    end

    // WOTS is designed to fit in a single block, assume BLOCKS=1
    //localparam int unsigned WOTS_BLOCKS    = calc_sha_blocks($bits(wots_data));
    localparam int unsigned WOTS_PAD_ZEROS = calc_sha_pad_zeros($bits(wots_data));

    wire [512-1:0] wots_padded =
            {wots_data, 1'b1, {WOTS_PAD_ZEROS{1'b0}}, 64'($bits(wots_data) + MS_BITS)};

    // -------------------------------------------------------------------------
    // Kc: H(I || q || D_PBLC || pk0..pk33), accumulated incrementally
    //
    // Absorbs (StWotsAccum, when the bank holds its quota) assemble a data
    // block from the prefix or the carry, the banked endpoints and the head
    // of the endpoint still sitting in hash_reg; its tail becomes the next
    // carry. StKcFinal then only absorbs the final block: the last carry,
    // whatever is still banked, and padding.
    // -------------------------------------------------------------------------

    localparam int unsigned KC_DATA_BITS = MS_BITS + KC_PREFIX_W + OTS_LEN*DW;  // Kc / T_len
    localparam int unsigned KC_FORS_BITS = MS_BITS + KC_PREFIX_W + FORS_K*DW;   // T_k

    wire kc_final     = (seq_q == StKcFinal);
    wire kc_accum     = ((seq_q == StWots) || (seq_q == StFors)) && (wots_q == StWotsAccum);

    // The endpoint in hash_reg closes a block when the bank holds its quota
    wire kc_closes    = (int'(kc_pos_q) == (kc_first_q ? int'(KC_FIRST) : int'(KC_MID)));
    wire kc_absorbing = (kc_accum && kc_closes) || kc_final;

    // Strobes of the accumulation registers below: every endpoint lands in
    // hash_reg as usual and is copied from there during StWotsAccum, and
    // the suspended state is latched back at each save's ready pulse.
    wire kc_saved = sha_save && sha_ready;

    logic [KC_PREFIX_W-1:0] kc_prefix;
    if (IS_HSS) begin : gen_pk_prefix_hss
        assign kc_prefix = ots_pk_prefix(.sch  (SCH),
                                         .kctx (cur_I),
                                         .ctrl (ctrl));
    end else begin : gen_pk_prefix_slh
        assign kc_prefix = fors_q ? fors_pk_prefix(.sch  (SCH),
                                                   .ctrl (ctrl))
                                  : ots_pk_prefix(.sch  (SCH),
                                                  .kctx (cur_I),
                                                  .ctrl (ctrl));
    end

    // Banked endpoints, the first in the most significant position
    logic [KC_MID*DW-1:0] kc_bank;
    always_comb begin
        for (int i = 0; i < KC_MID; i++) kc_bank[KC_MID*DW-1 - i*DW -: DW] = kc_lo_q[i];
    end

    wire [511:0] kc_first_block = {kc_prefix, kc_bank[KC_MID*DW-1 -: KC_FIRST*DW],
                                   hash_reg_q[DW-1 -: KC_TOP_W]};
    wire [511:0] kc_mid_block   = {kc_hi_q, kc_bank,
                                   hash_reg_q[DW-1 -: KC_TOP_W]};
    wire [511:0] kc_absorb_block = kc_first_q ? kc_first_block : kc_mid_block;

    // Final padding block: the carry, the tail endpoints still banked, then
    // padding -- an elaboration-time layout per accumulation.
    function automatic logic [511:0] kc_final_layout(
        input int unsigned           tail,
        input int unsigned           len_bits,
        input logic [KC_CARRY_W-1:0] carry,
        input logic [KC_MID*DW-1:0]  bank);
        logic [511:0] block;
        block = '0;
        block[511 -: KC_CARRY_W] = carry;
        for (int i = 0; i < KC_MID; i++) begin
            if (i < tail) block[511 - KC_CARRY_W - i*DW -: DW] = bank[KC_MID*DW-1 - i*DW -: DW];
        end
        block[511 - KC_CARRY_W - tail*DW] = 1'b1;
        block[63:0] = 64'(len_bits);
        return block;
    endfunction

    wire [511:0] kc_final_ots  = kc_final_layout(.tail     (KC_TAIL),
                                                 .len_bits (KC_DATA_BITS),
                                                 .carry    (kc_hi_q),
                                                 .bank     (kc_bank));
    wire [511:0] kc_final_fors = kc_final_layout(.tail     (KC_TAIL_FORS),
                                                 .len_bits (KC_FORS_BITS),
                                                 .carry    (kc_hi_q),
                                                 .bank     (kc_bank));
    wire [511:0] kc_final_block = fors_q ? kc_final_fors : kc_final_ots;

    // Every Kc block except the final one suspends the Kc hash; every one
    // after the first resumes it from the saved state. With a midstate, the
    // first Kc block and block 0 of every chain, FORS-leaf and Merkle hash
    // resume from that instead.
    wire fh_hashing = (((seq_q == StWots) || (seq_q == StFors)) && (wots_q == StWotsHash))
                   || mrkl_hashing;
    wire kc_resumes = kc_absorbing && !kc_first_q;     // a Kc block was absorbed before
    assign sha_save    = kc_absorbing && !kc_final;
    assign sha_restore = kc_resumes || (MIDSTATE && (kc_absorbing || fh_hashing));
    if (MIDSTATE) begin : gen_ctx_midstate
        assign sha_ctx = kc_resumes ? kc_state_q : midstate;
    end else begin : gen_ctx_state
        assign sha_ctx = kc_state_q;
    end

    // -------------------------------------------------------------------------
    // Leaf: H(I || q || D_LEAF || Kc)
    // -------------------------------------------------------------------------

    localparam int unsigned LEAF_MSG_W = MSS_LEAF_MSG_BITS;

    wire [LEAF_MSG_W-1:0] leaf_data = mss_leaf_msg(.kctx (cur_I),
                                                   .ctrl (ctrl),
                                                   .kc   (hash_reg_wide));

    localparam int unsigned LEAF_BLOCKS    = calc_sha_blocks($bits(leaf_data));
    localparam int unsigned LEAF_PAD_ZEROS = calc_sha_pad_zeros($bits(leaf_data));

    wire [LEAF_BLOCKS*512-1:0] leaf_padded =
            {leaf_data, 1'b1, {LEAF_PAD_ZEROS{1'b0}}, 64'($bits(leaf_data))};

    // -------------------------------------------------------------------------
    // Merkle helpers
    // -------------------------------------------------------------------------

    // The sibling is read straight off the stream: every block that reads it
    // is only offered while the beat is present, and the beat is released
    // (ready) in the cycle the core captures the last of them.
    wire mrkl_wants = mrkl_hashing && sha_taken && sha_last;

    // Nodes are indexed as 2n (left) and 2n+1 (right) from their parent.
    // The leaf is node 2^h + q and each level up halves the node number, so
    // the node at the current level is a right child iff that bit of q is
    // set; the node number itself is derived the same way in the package.
    // A FORS leaf's index within its tree is the FORS digit, so the level
    // counter (which never exceeds the FORS height there) selects its bit.
    logic is_right;
    if (IS_HSS) begin : gen_is_right_hss
        assign is_right = leaf_index_q[mrkl_level_q];
    end else begin : gen_is_right_slh
        localparam int unsigned FORS_LEVEL_SEL_W = $clog2(FORS_DIGIT_W);
        assign is_right = fors_q ? gen_fors.fors_digit[mrkl_level_q[FORS_LEVEL_SEL_W-1:0]]
                                 : leaf_index_q[mrkl_level_q];
    end

    // The sibling is the beat on the bus
    logic [DW-1:0] left_node;
    logic [DW-1:0] right_node;

    assign {left_node, right_node} = is_right ? {data,       hash_reg_q}
                                              : {hash_reg_q, data};

    // At the width the shared join function takes them
    logic [MAX_DATA_W-1:0] left_wide, right_wide;
    if (DW == MAX_DATA_W) begin : gen_full_nodes
        assign left_wide  = left_node;
        assign right_wide = right_node;
    end else begin : gen_widen_nodes
        assign left_wide  = {left_node,  {(MAX_DATA_W-DW){1'b0}}};
        assign right_wide = {right_node, {(MAX_DATA_W-DW){1'b0}}};
    end

    // -------------------------------------------------------------------------
    // Merkle: H(I || parent || D_INTR || left || right); SLH
    // H(ADRSc || left || right) in the XMSS tree or, in the FORS phase, in
    // the FORS tree
    // -------------------------------------------------------------------------

    localparam int unsigned MRKL_MSG_W = mss_join_msg_bits(SCH);

    logic [MRKL_MSG_W-1:0] mrkl_data;
    if (IS_HSS) begin : gen_join_hss
        assign mrkl_data = MRKL_MSG_W'(mss_join_msg(.sch   (SCH),
                                                    .kctx  (cur_I),
                                                    .ctrl  (ctrl),
                                                    .left  (left_wide),
                                                    .right (right_wide)));
    end else begin : gen_join_slh
        assign mrkl_data = fors_q
                ? MRKL_MSG_W'(fors_join_msg(.sch       (SCH),
                                            .ctrl      (ctrl),
                                            .fors_leaf (gen_fors.fors_leaf_idx),
                                            .left      (left_node),
                                            .right     (right_node)))
                : MRKL_MSG_W'(mss_join_msg(.sch   (SCH),
                                           .kctx  (cur_I),
                                           .ctrl  (ctrl),
                                           .left  (left_wide),
                                           .right (right_wide)));
    end

    localparam int unsigned MRKL_BLOCKS    = calc_sha_blocks($bits(mrkl_data));
    localparam int unsigned MRKL_PAD_ZEROS = calc_sha_pad_zeros($bits(mrkl_data));

    wire [MRKL_BLOCKS*512-1:0] mrkl_padded =
            {mrkl_data, 1'b1, {MRKL_PAD_ZEROS{1'b0}}, 64'($bits(mrkl_data) + MS_BITS)};


    // -------------------------------------------------------------------------
    // SHA block counter, last block flag and block selection
    // -------------------------------------------------------------------------

    // Helper variable
    int unsigned num_blocks;
    int unsigned blk_shift;

    // Unused bits from shift output
    /* verilator lint_off UNUSEDSIGNAL */
    logic [$bits(q_msg_padded)-1:0] q_msg_discard;
    logic [$bits(q_sub_padded)-1:0] q_sub_discard;
    logic [$bits(mgf1_padded)-1:0]  mgf1_discard;
    logic [$bits(leaf_padded)-1:0]  leaf_discard;
    logic [$bits(mrkl_padded)-1:0]  mrkl_discard;
    /* verilator lint_on UNUSEDSIGNAL */

    // Block counter — not used by Kc at all: a Kc block's position in the
    // message is tracked by the chain index instead, and the next chain
    // hash must still see index zero.
    always_comb begin
        blk_idx_d = blk_idx_q;

        if (sha_ready && !kc_absorbing) begin
            blk_idx_d = ~sha_last ? blk_idx_q + 1 : 0;
        end
    end
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            blk_idx_q <= '0;
        end else begin
            blk_idx_q <= blk_idx_d;
        end
    end

    // Last block flag. Kc bypasses the counter: only the final padding
    // block closes the Kc message, every other absorb suspends it.
    assign sha_last = kc_absorbing ? kc_final :
                      (int'(blk_idx_q) == num_blocks-1) ? 1'b1 : 1'b0;

    // Input vector and block selection
    always_comb begin
        blk_shift = int'(blk_idx_q) * 512;

        num_blocks =  0;
        sha_block  = '0;

        q_msg_discard = '0;
        q_sub_discard = '0;
        mgf1_discard  = '0;
        leaf_discard  = '0;
        mrkl_discard  = '0;

        // Append 512'b0 for the shifts on the right side so widths are equal
        unique case (seq_q)
            StQ: begin
                if (is_msg_layer) begin
                    num_blocks = Q_MSG_BLOCKS;
                    {sha_block, q_msg_discard} = {q_msg_padded, 512'b0} << blk_shift;
                end else begin
                    num_blocks = Q_SUB_BLOCKS;
                    {sha_block, q_sub_discard} = {q_sub_padded, 512'b0} << blk_shift;
                end
            end
            StMgf1: begin
                num_blocks = MGF1_BLOCKS;
                {sha_block, mgf1_discard} = {mgf1_padded, 512'b0} << blk_shift;
            end
            StWots, StFors: begin
                if (wots_q == StWotsAccum) begin
                    sha_block = kc_absorb_block;
                end else begin
                    num_blocks = 1;
                    sha_block  = wots_padded;
                end
            end
            StKcFinal: begin
                sha_block = kc_final_block;
            end
            StLeaf: begin
                num_blocks = LEAF_BLOCKS;
                {sha_block, leaf_discard} = {leaf_padded, 512'b0} << blk_shift;
            end
            StMerkle: begin
                num_blocks = MRKL_BLOCKS;
                {sha_block, mrkl_discard} = {mrkl_padded, 512'b0} << blk_shift;
            end
            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // hash_reg — captures sha_digest on completion, or sig chain on WOTS load
    // -------------------------------------------------------------------------

    wire hdr_wants  = (seq_q == StQ) && (hdr_cnt_q != HDR_DONE);
    wire wots_wants = ((seq_q == StWots) || (seq_q == StFors)) && (wots_q == StWotsLoad);
    wire wots_loading = wots_wants && valid;
    // SLH: R stays on the bus through both H_msg hashes and is released when
    // the second one captures its first block, the last to read it
    wire rand_wants = IS_SLH && (seq_q == StMgf1) && (blk_idx_q == '0) && sha_taken;
    wire wants_data = hdr_wants || wots_wants || rand_wants || mrkl_wants;

    // Qualified by valid so ready is never asserted on its own.
    assign ready = valid && wants_data;
    wire hash_reg_en  = wots_loading | hash_complete;

    assign hash_reg_d = (!wots_loading) ? trunc_digest : data;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            hash_reg_q <= '0;
        end else if (hash_reg_en) begin
            hash_reg_q <= hash_reg_d;
        end
    end

    // -------------------------------------------------------------------------
    // aux_reg — the randomizer while Q is set up, then the Q hash through WOTS
    // -------------------------------------------------------------------------

    // Not in the FORS phase: md must survive it
    wire wots_init  = (seq_q == StWots) && (wots_q == StWotsInit);

    // Scratch register, two time-disjoint producers: the randomizer while Q
    // is being set up and the Q digest through WOTS. Reusing it keeps the
    // randomizer out of storage entirely -- it is only ever read by the Q
    // hash. SLH: md, split off the H_msg digest, and later the value in
    // hash_reg at each WOTS start (the FORS public key, then each root).
    wire rand_loading = (seq_q == StQ) && (hdr_cnt_q == HDR_CNT_W'(1)) && valid;
    wire md_split     = IS_SLH && (seq_q == StMgf1) && hash_complete;

    assign aux_reg_d = md_split     ? MSG_W'(hmsg.md) :
                       rand_loading ? aux_from_data : aux_from_hash;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aux_reg_q <= '0;
        end else if (wots_init || rand_loading || md_split) begin
            aux_reg_q <= aux_reg_d;
        end
    end

    // -------------------------------------------------------------------------
    // Kc accumulation registers — banked endpoint, staged carry, saved state
    // -------------------------------------------------------------------------

    // The copy must not extend into the absorb's ready cycle: without the
    // double-registered digest the absorb itself rewrites the digest during
    // WotsAccum, so a late copy would take the Kc state instead of the
    // endpoint. REVISIT: properly this samples in the cycle the core takes
    // the block, which needs the wrapper handshake extended with a done
    // indication; the guard marks the intent until then.
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 0; i < KC_MID; i++) kc_lo_q[i] <= '0;
            kc_tail_q <= '0;
        end else if (kc_accum && (!kc_closes || !sha_ready)) begin
            if (kc_closes) begin
                kc_tail_q <= hash_reg_q[KC_CARRY_W-1:0];
            end else begin
                for (int i = 0; i < KC_MID; i++) begin
                    if (int'(kc_pos_q) == i) kc_lo_q[i] <= hash_reg_q;
                end
            end
        end
    end

    // At the absorb's ready pulse sha_digest holds the resumable state, and
    // the staged tail becomes the carry of the next Kc block. SLH also parks
    // the H_msg inner digest here for MGF1.
    wire kc_state_en = kc_saved || (IS_SLH && (seq_q == StQ) && hash_complete);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            kc_state_q <= '0;
            kc_hi_q    <= '0;
        end else begin
            if (kc_state_en) kc_state_q <= sha_digest;
            if (kc_saved)    kc_hi_q    <= kc_tail_q;
        end
    end

    // -------------------------------------------------------------------------
    // Sub-FSM output signals
    // -------------------------------------------------------------------------

    // WOTS
    logic             wots_sha_valid;
    logic             wots_complete;

    // Merkle
    logic             mrkl_sha_valid;
    logic             mrkl_complete;

    // -------------------------------------------------------------------------
    // WOTS sub-FSM — runs all chains, stores pk
    // -------------------------------------------------------------------------

    always_comb begin
        wots_d         = wots_q;

        wots_chain_d   = wots_chain_q;
        wots_step_d    = wots_step_q;
        kc_pos_d       = kc_pos_q;
        kc_first_d     = kc_first_q;

        wots_sha_valid = 1'b0;
        wots_complete  = 1'b0;

        // Only activate when main FSM is in WOTS state (or SLH's FORS phase,
        // which runs the same Load / Hash / Accum sequence per tree)
        if ((seq_q == StWots) || (seq_q == StFors)) begin

            unique case (wots_q)
                StWotsInit: begin
                    wots_chain_d = '0;
                    wots_step_d  = '0;
                    kc_pos_d     = '0;
                    kc_first_d   = 1'b1;
                    // aux_reg captures hash_reg (Q hash) this cycle also
                    // (outside this always_comb since aux_reg is shared)

                    wots_d = StWotsLoad;
                end

                StWotsLoad: begin
                    // Stall until the next chain element arrives.
                    if (valid) begin
                        // load step counter from the signed digit; a FORS
                        // leaf is exactly one F step
                        wots_step_d = fors_q ? HASH_IDX_W'(DIGIT_MAX - 1'b1)
                                             : HASH_IDX_W'(cur_digit);
                        // hash_reg captures the chain signature this cycle too
                        // (outside this always_comb since hash_reg is shared)

                        // hash unless the digit is already the maximum value
                        wots_d = (fors_q || (cur_digit != DIGIT_MAX)) ? StWotsHash : StWotsAccum;
                    end
                end

                StWotsHash: begin
                    // Start the hash and wait to complete
                    wots_sha_valid = 1'b1;
                    if (sha_ready) begin
                        // increment step counter
                        wots_step_d = wots_step_q + 1;

                        // continue hashing if this was not the last hash,
                        // otherwise move to fold the endpoint into Kc
                        wots_d = (wots_step_q != HASH_IDX_W'(DIGIT_MAX-1)) ? StWotsHash
                                                                           : StWotsAccum;
                    end
                end

                StWotsAccum: begin
                    // Bank the endpoint (kc_lo copies it from hash_reg this
                    // cycle) and advance -- endpoints still banked at the
                    // last chain ride in the final padding block. Or, when
                    // it closes a block: absorb the assembled block into the
                    // suspended Kc hash and advance on its ready;
                    // kc_state/kc_hi latch there too.
                    if (kc_closes) begin
                        wots_sha_valid = 1'b1;
                    end
                    if (!kc_closes || sha_ready) begin
                        kc_pos_d      = kc_closes ? '0 : kc_pos_q + 1'b1;
                        kc_first_d    = kc_first_q && !kc_closes;
                        wots_chain_d  = ~last_chain ? wots_chain_q+1 : '0;
                        wots_d        = ~last_chain ? StWotsLoad     : StWotsInit;

                        // signal completion to main FSM on last chain
                        wots_complete = last_chain;
                    end
                end

                default: ;
            endcase
        end
    end

    // -------------------------------------------------------------------------
    // Merkle sub-FSM — walk auth path from leaf to root
    // -------------------------------------------------------------------------

    always_comb begin
        mrkl_d          = mrkl_q;

        mrkl_level_d    = mrkl_level_q;

        mrkl_sha_valid  = 1'b0;
        mrkl_complete   = 1'b0;

        // Only activate when main FSM is in Merkle state
        if (seq_q == StMerkle) begin

            unique case (mrkl_q)
                StMrklInit: begin
                    mrkl_d = StMrklHash;
                end

                StMrklHash: begin
                    // Hash with the sibling straight off the bus: the core is
                    // fed only while the beat is present, and the beat is
                    // released when the last block is captured.
                    mrkl_sha_valid = valid;
                    if (hash_complete) begin
                        // Increment level count or clear it
                        mrkl_level_d = ~last_level ? mrkl_level_q+1 : '0;
                        mrkl_d = ~last_level ? StMrklHash : StMrklInit;

                        // signal completion to main FSM on last level
                        mrkl_complete = last_level;
                    end
                end

                default: ;
            endcase
        end
    end

    // -------------------------------------------------------------------------
    // Main (Sequencer) FSM
    // -------------------------------------------------------------------------

    always_comb begin
        seq_d         = seq_q;
        layer_d       = layer_q;
        leaf_index_d  = leaf_index_q;
        tree_idx_d    = tree_idx_q;
        fors_d        = fors_q;
        cur_I_d       = cur_I_q;
        prev_I_d      = prev_I_q;
        hdr_cnt_d     = hdr_cnt_q;
        sha_valid     = 1'b0;
        verify_done   = 1'b0;
        verif_passed  = 1'b0;

        unique case (seq_q)

            StIdle: begin
                // The first beat offered starts the verification; it is not
                // consumed here (wants_data is low), so the producer holds it
                // until StQ takes it.
                if (valid) begin
                    // Start at the bottom layer (signs the user message)
                    layer_d   = HT_LAYER_W'(LAYERS - 1);
                    hdr_cnt_d = '0;
                    fors_d    = 1'b0;
                    seq_d     = StQ;
                end
            end

            // The states below are responsible to start the hashing and process the completion
            // The rest (feeding the appropriate inputs to the SHA block) is taken care outisde this
            // always_comb block based on the FSM state and sub-FSM states

            StQ: begin
                // Take this layer's header off the stream first: beat 0 is
                // {leaf_index, sub_I}, beat 1 the randomizer (latched into
                // aux_reg outside this block). Hashing starts once both are in.
                if (hdr_cnt_q != HDR_DONE) begin
                    if (valid) begin
                        if (hdr_cnt_q == '0) begin
                            leaf_index_d = hdr_leaf_idx;
                            // Keep the identifier this layer used: the layer
                            // above signs our public key and needs it.
                            prev_I_d     = cur_I_q;
                            cur_I_d      = hdr_sub_ident;
                        end
                        hdr_cnt_d = hdr_cnt_q + 1'b1;
                    end
                end else begin
                    // hdr_cnt_q == HDR_DONE: both header beats have been captured.
                    // Start Q hash and wait to complete. SLH's inner H_msg
                    // hash reads R off the bus in its first block, so that
                    // block is only offered while the beat is present.
                    sha_valid = (IS_SLH && (blk_idx_q == '0)) ? valid : 1'b1;
                    if (hash_complete) begin
                        hdr_cnt_d = '0;
                        seq_d     = IS_SLH ? StMgf1 : StWots;
                    end
                end
            end

            StMgf1: begin
                // SLH: MGF1 over R and the parked inner digest. Its digest is
                // split at completion: md into aux_reg (outside this block),
                // idx_tree and idx_leaf here; R is released then too.
                sha_valid = (blk_idx_q == '0) ? valid : 1'b1;
                if (hash_complete) begin
                    leaf_index_d = hmsg.leaf_idx;
                    tree_idx_d   = hmsg.tree_idx;
                    fors_d       = 1'b1;
                    seq_d        = StFors;
                end
            end

            StFors: begin
                // SLH: each FORS tree runs through the WOTS sub-FSM (one F
                // step on the secret element) and, when that hash completes,
                // the Merkle sub-FSM (an a-level auth path); the root comes
                // back here to be banked into T_k by StWotsAccum.
                sha_valid = wots_sha_valid;
                if ((wots_q == StWotsHash) && hash_complete) begin
                    seq_d = StMerkle;
                end
                if (wots_complete) begin
                    seq_d = StKcFinal;
                end
            end

            StWots: begin
                // The WOTS step has multiple iterations, delegate hash control to WOTS sub-FSM
                sha_valid = wots_sha_valid;
                if (wots_complete) begin
                    seq_d = StKcFinal;
                end
            end

            StKcFinal: begin
                // Start Kc hash and wait to complete. HSS hashes the result
                // once more for the leaf; SLH's T_k digest is the FORS
                // public key, the message the chains sign next, and its
                // T_len digest is the XMSS leaf.
                sha_valid = 1'b1;
                if (hash_complete) begin
                    if (LEAF_HASH) begin
                        seq_d  = StLeaf;
                    end else if (fors_q) begin
                        fors_d = 1'b0;
                        seq_d  = StWots;
                    end else begin
                        seq_d  = StMerkle;
                    end
                end
            end

            StLeaf: begin
                // Start Leaf hash and wait to complete
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StMerkle;
                end
            end

            StMerkle: begin
                // The Merkle step has multiple iterations, delegate hash control to Merkle sub-FSM
                sha_valid = mrkl_sha_valid;
                if (mrkl_complete) begin
                    if (fors_q) begin
                        // FORS root: back to the tree loop to bank it
                        seq_d = StFors;
                    end else if (is_pk_layer) begin
                        seq_d   = StDone;
                        layer_d = '0;
                    end else begin
                        layer_d = layer_q - 1'b1;
                        if (IS_HSS) begin
                            seq_d = StQ;
                        end else begin
                            // SLH: the root is the next layer's WOTS message
                            // (copied at StWotsInit); the index shifts down a
                            // layer (Alg 13)
                            leaf_index_d = LEAF_IDX_W'(tree_idx_q[TREE_HT-1:0]);
                            tree_idx_d   = tree_idx_q >> TREE_HT;
                            seq_d        = StWots;
                        end
                    end
                end
            end

            StDone: begin
                verify_done  = 1'b1;
                verif_passed = (hash_reg_q == root_pub_key);
                seq_d        = StIdle;
            end

            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // Sequential
    // -------------------------------------------------------------------------

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            leaf_index_q <= '0;
            tree_idx_q   <= '0;
            fors_q       <= 1'b0;
            cur_I_q      <= '0;
            prev_I_q     <= '0;
            hdr_cnt_q    <= '0;
            seq_q         <= StIdle;
            wots_q        <= StWotsInit;
            wots_chain_q  <= '0;
            wots_step_q   <= '0;
            kc_pos_q      <= '0;
            kc_first_q    <= 1'b1;
            mrkl_q        <= StMrklInit;
            mrkl_level_q  <= '0;
            layer_q       <= '0;
        end else begin
            leaf_index_q <= leaf_index_d;
            tree_idx_q   <= tree_idx_d;
            fors_q       <= fors_d;
            cur_I_q      <= cur_I_d;
            prev_I_q     <= prev_I_d;
            hdr_cnt_q    <= hdr_cnt_d;
            seq_q         <= seq_d;
            wots_q        <= wots_d;
            wots_chain_q  <= wots_chain_d;
            wots_step_q   <= wots_step_d;
            kc_pos_q      <= kc_pos_d;
            kc_first_q    <= kc_first_d;
            mrkl_q        <= mrkl_d;
            mrkl_level_q  <= mrkl_level_d;
            layer_q       <= layer_d;
        end
    end

endmodule
