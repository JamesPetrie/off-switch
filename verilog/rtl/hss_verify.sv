// Hash-based Signature Verification: RFC 8554 HSS/LMS, or FIPS 205 SLH-DSA
// on the same structure (see the SLH-DSA paragraph below).
//
// Single-module implementation of RFC 8554 HSS/LMS verification.
// One SHA-256 core shared by all phases, sequenced by a main FSM:
//
//   Sequencer  — phases: Idle → Q → Wots → KcFinal → Mss → Done
//   Q          — hash for message digest Q
//   WOTS       — hash WOTS chains forward to their public keys (sub-FSM),
//                folding each finished pk into the Kc hash as it appears
//   KcFinal    — resume the Kc hash one final time for the padding block
//   Mss        — hash for leaf node, then walk auth path from leaf to root
//                (sub-FSM)
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
// (restart Q→...→Mss with hash_reg_q carrying the just-computed root as
// the next layer's signed-message input) or, at layer 0, compare the result
// against ROOT_PUB_KEY. Intermediate root consistency is verified implicitly
// by each upper layer's WOTS+Merkle succeeding with that root as its Q input.
// This is the opposite direction of the standard but allows area saving.
//
// SLH-DSA (SCH = SCHEME_SLH_128S) runs the same sequencer on the FIPS 205
// layouts from hbsv_schs_pkg, with three more phases between Q and Wots:
//
//   Mgf1       — second hash, extending the message digest
//   Fors       — hash each FORS leaf and walk its auth path (Merkle
//                sub-FSM), folding each root into the FORS public-key hash
//   ForsFinal  — resume that hash one final time for the padding block
//
// The layers above the first sign the root below them directly, so Q runs
// once and Mss leads straight back to Wots.
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
    input  logic [KCTX_W-1:0]  identifier,   // tree identifier
    input  logic [DW-1:0]      root_pub_key,

    // License beat stream, in the field order of the standard signature
    // format (see hss_pkg). Per layer, from LAYERS-1 down to 0: a header
    // beat carrying {leaf_idx, sub_I}, the randomizer, OTS_LEN chain
    // signatures, then TREE_HT auth path siblings. Each beat is consumed where
    // it is needed, so only the current layer's identity is held.
    // SLH (see slh_pkg): the randomizer, the FORS elements, then per layer
    // the chain signatures and the auth path siblings.
    // A beat that is read straight into a hash block is held on the bus by
    // the producer and released (ready) when that hash completes.
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

    localparam int unsigned FORS_K    = fors_k(SCH);    // FORS trees
    localparam int unsigned FORS_H    = fors_h(SCH);    // FORS tree height
    localparam int unsigned SIGNAND_W = signand_w(SCH); // value signed by the chains / FORS

    localparam bit MSS_LEAF_HASH = has_mss_leaf_hash(SCH);  // Kc is hashed once more for the leaf
    localparam bit SUB_PK_HASH   = has_sub_pk_hash(SCH);    // a root is hashed before it is signed
    localparam bit EXTEND_DIGEST = extends_digest(SCH);     // the message digest takes two hashes

    // -------------------------------------------------------------------------
    // FSM state types
    // -------------------------------------------------------------------------

    typedef enum logic [3:0] {
        StIdle, StQ, StMgf1, StFors, StKcFinalFors, StWots, StKcFinalWots, StMss, StDone
    } seq_state_e;

    // Header beats per layer: {leaf_idx, sub_I} then the randomizer.
    // Without a header the key context is the public key's in every layer
    // and the randomizer is read straight off the stream.
    localparam int unsigned HDR_BEATS = hdr_beats(SCH);
    localparam bit          HAS_HDR   = (HDR_BEATS > 0);
    localparam int unsigned HDR_CNT_W = HAS_HDR ? $clog2(HDR_BEATS + 1) : 1;
    localparam logic [HDR_CNT_W-1:0] HDR_DONE = HDR_CNT_W'(HDR_BEATS);

    typedef enum logic [1:0] {
        StWotsInit, StWotsLoad, StWotsHash, StWotsAccum
    } wots_state_e;


    typedef enum logic [1:0] {
        StMrklInit, StMrklLeaf, StMrklJoin, StMrklAccum
    } mrkl_state_e;

    // -------------------------------------------------------------------------
    // Kc interleaving geometry
    //
    // The Kc message is the prefix followed by the OTS_LEN chain endpoints;
    // hbsv_schs_pkg derives the block geometry and documents it. Endpoints
    // are banked until one closes a block: KC_FIRST of them after the
    // prefix, KC_MID after the carry of the previous block. The endpoint
    // closing a block contributes its KC_TOP_W top bits and leaves the rest
    // as the next carry. The first absorb takes KC_FIRST_BLOCKS blocks, the
    // prefix being longer than a block in some schemes. The final padding
    // block carries the last carry and the KC_TAIL endpoints still banked.
    // The FORS roots are accumulated into the FORS public key the same way.
    // -------------------------------------------------------------------------

    localparam int unsigned KC_PREFIX_W     = ots_pk_prefix_bits(SCH);
    localparam int unsigned KC_FIRST_BLOCKS = acc_first_blocks(SCH);
    localparam int unsigned KC_FIRST        = acc_first_full(SCH);
    localparam int unsigned KC_MID          = acc_mid_full(SCH);
    localparam int unsigned KC_TOP_W        = acc_top_w(SCH);
    localparam int unsigned KC_CARRY_W      = acc_carry_w(SCH);
    localparam int unsigned KC_TAIL_OTS     = acc_tail_elems(.sch (SCH), .count (OTS_LEN));
    localparam int unsigned KC_TAIL_FORS    = acc_tail_elems(.sch (SCH), .count (FORS_K));
    localparam int unsigned KC_POS_W        = $clog2(KC_MID + 1);

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
    // registers idle at that point are the Kc accumulation ones (kc_bank would
    // fit). Worth another look if HSS-LMS is picked up again.
    //
    // SLH: the leaf index comes off the message digest together with the
    // index of its tree; both move up a layer at a time.
    // Both at the scheme's width: a leaf index is TREE_HT bits, the tree
    // index the hypertree's remaining (LAYERS-1)*TREE_HT bits (HSS: unused);
    // ctrl_t carries them zero-extended.
    localparam int unsigned LEAF_W = TREE_HT;
    localparam int unsigned TREE_W = (LAYERS - 1) * TREE_HT;
    logic [LEAF_W-1:0]      leaf_idx_q,   leaf_idx_d;
    logic [TREE_W-1:0]      tree_idx_q,   tree_idx_d;
    // HSS: set when the header's q has bits above TREE_HT. It rides into
    // the hashed q at bit TREE_HT, so Kc, and with it the root, can never
    // match: RFC 8554 Alg. 6a step 2.i rejects such a q at parse time, here
    // it fails the hash instead.
    logic                   leaf_ovf_q,   leaf_ovf_d;
    logic [KCTX_W-1:0]      cur_I_q,      cur_I_d;
    logic [KCTX_W-1:0]      prev_I_q,     prev_I_d;
    logic [HDR_CNT_W-1:0]   hdr_cnt_q,    hdr_cnt_d;
    wots_state_e  wots_q,  wots_d;
    mrkl_state_e  mrkl_q,  mrkl_d;

    // Hash register — working hash output across all phases
    logic [DW-1:0]    hash_reg_q,    hash_reg_d;

    // Auxiliary register — companion value alongside hash_reg
    // WOTS: holds Q hash
    // FORS: holds the signand split off the message digest
    logic [SIGNAND_W-1:0] aux_reg_q, aux_reg_d;

    // Shared block counter — indexes SHA-256 blocks within a multi-block hash
    // REVISIT hardcoded widhts
    logic [4:0]       blk_idx_q,     blk_idx_d;

    // WOTS counters (driven by WOTS sub-FSM)
    logic [CHAIN_IDX_W-1:0] wots_chain_q, wots_chain_d; // chain index 0..OTS_LEN-1
    logic [HASH_IDX_W-1:0]  wots_step_q,  wots_step_d;  // step within chain

    // Merkle tree level (driven by Merkle sub-FSM) and FORS tree index
    // (driven by the sequencer)
    logic [MRKL_LEVEL_W-1:0] mrkl_level_q, mrkl_level_d;
    logic [MRKL_TREE_W-1:0]  fors_tree_q,  fors_tree_d;

    // Kc interleaved accumulation (replaces the former 34 x 256-bit pk store):
    // the banked endpoints, the current carry and the staged next carry (the
    // rest of the endpoint that closed the block), the bank position and a
    // first-absorb flag. The suspended SHA state is sha_ctx.
    //
    // REVISIT: kc_tail stages the next carry so it is not read back from
    // hash_reg after the absorb, the one spot that would otherwise rely on
    // the digest being registered twice (core and verifier — see the
    // design-doc limitation). Revisit together with that limitation.
    logic [KC_MID-1:0][DW-1:0] kc_bank_q;
    logic [KC_CARRY_W-1:0]     kc_hi_q;
    logic [KC_CARRY_W-1:0]     kc_tail_q;
    logic [KC_POS_W-1:0]       kc_pos_q,   kc_pos_d;    // endpoints banked since the last absorb
    logic                      kc_first_q, kc_first_d;  // nothing absorbed yet

    // SHA context — the state of a suspended hash (Kc), or the first digest
    // of the message hash while the second extends it
    logic [255:0]           sha_ctx_q;

    // Hypertree layer counter
    logic [HT_LAYER_W-1:0] layer_q, layer_d;

    // -------------------------------------------------------------------------
    // SHA-256 wrapper instance
    // -------------------------------------------------------------------------

    logic         sha_valid;
    logic [511:0] sha_block;
    logic         sha_last;
    wire          sha_ready;
    wire  [255:0] sha_digest;

    logic         sha_save;
    logic         sha_restore;

    sha2_wrap u_sha256 (
        .clk     (clk),
        .rst_n   (rst_n),
        .valid   (sha_valid),
        .block   (sha_block),
        .last    (sha_last),
        .save    (sha_save),
        .restore (sha_restore),
        .ctx     (sha_ctx_q),
        .ready   (sha_ready),
        .digest  (sha_digest)
    );

    wire hash_complete = sha_last && sha_ready;

    // -------------------------------------------------------------------------
    // Per-layer selectors
    // -------------------------------------------------------------------------

    // Hypertree layer signing the message (bottom)
    wire is_msg_layer = (int'(layer_q) == LAYERS - 1);
    // Hypetree layer corresponing to the Public Key (top)
    wire is_pk_layer  = (layer_q == '0);

    // Top-tree identifier is the package constant; lower trees carry theirs
    // in the license as sub_I[lv] (≥1). sub_I[0] is unused for the top layer.
    wire [KCTX_W-1:0] cur_I = (is_pk_layer || !HAS_HDR) ? identifier
                                                        : cur_I_q;

    // Leaf the Merkle walk starts from: the key pair's, or the one the
    // current FORS tree reveals (its digit of the signand)
    wire [LEAF_IDX_W-1:0] mrkl_leaf =
            (seq_q == StFors) ? fors_leaf_idx(.sch       (SCH),
                                              .signand   (MAX_SIGNAND_W'(aux_reg_q)),
                                              .fors_tree (fors_tree_q))
                              : LEAF_IDX_W'(leaf_idx_q);

    // -------------------------------------------------------------------------
    // Control bundle — the counters in the form the hash messages read them
    // -------------------------------------------------------------------------

    wire ctrl_t ctrl = '{ht_layer:   layer_q,
                         chain_idx:  wots_chain_q,
                         hash_idx:   wots_step_q,
                         mrkl_level: mrkl_level_q,
                         leaf_idx:   LEAF_IDX_W'({leaf_ovf_q, leaf_idx_q}),
                         tree_idx:   TREE_IDX_W'(tree_idx_q),
                         mrkl_tree:  fors_tree_q,
                         mrkl_leaf:  mrkl_leaf};

    // -------------------------------------------------------------------------
    // Data indexed by WOTS chain / Merkle level
    // -------------------------------------------------------------------------

    wire             last_chain    = (int'(wots_chain_q) == OTS_LEN-1) ? 1'b1 : 1'b0;

    wire             last_level    = (seq_q == StFors)  ? (int'(mrkl_level_q) == FORS_H-1)
                                                        : (int'(mrkl_level_q) == TREE_HT-1);

    wire             last_fors_tree = (int'(fors_tree_q) == FORS_K-1) ? 1'b1 : 1'b0;

    // -------------------------------------------------------------------------
    // Q hash split into digits + checksum — computed combinationally
    // -------------------------------------------------------------------------

    logic [DIGIT_W-1:0] q_digits[OTS_LEN];

    // Using digit-wise shift left to avoid indexing issues
    always_comb begin
        logic [DW-1:0] hash;        // hash working variable
        logic [CSUM_W-1:0] csum;    // checksum working variable

        hash  = aux_reg_q[DW-1:0];
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
    // Q: the message hash
    //
    // Message layer (is_msg_layer):   over the user message
    // Upper layers:                   over the public key of the layer below,
    //                                 its identifier (prev_I_q) and its root
    //                                 (hash_reg_q, just computed)
    // -------------------------------------------------------------------------

    localparam int unsigned Q_MSG_W = msg_hash_msg_bits(SCH);
    localparam int unsigned Q_SUB_W = SUB_PK_HASH_MSG_BITS;

    // The randomizer was latched into aux_reg with the header or, without
    // one, is the beat on the bus
    wire [DW-1:0] randomizer = HAS_HDR ? aux_reg_q[DW-1:0] : data;

    wire [Q_MSG_W-1:0] q_msg_data = Q_MSG_W'(msg_hash_msg(.sch        (SCH),
                                                          .kctx       (cur_I),
                                                          .ctrl       (ctrl),
                                                          .randomizer (MAX_DATA_W'(randomizer)),
                                                          .pk_root    (MAX_DATA_W'(root_pub_key)),
                                                          .message    (message)));

    // sub_I is indexed at layer_q+1 (identity of the tree below)
    wire [Q_SUB_W-1:0] q_sub_data = sub_pk_hash_msg(.sch        (SCH),
                                                    .kctx       (cur_I),
                                                    .ctrl       (ctrl),
                                                    .randomizer (MAX_DATA_W'(randomizer)),
                                                    .sub_kctx   (prev_I_q),
                                                    .sub_root   (MAX_DATA_W'(hash_reg_q)));

    localparam int unsigned Q_MSG_BLOCKS    = calc_sha_blocks($bits(q_msg_data));
    localparam int unsigned Q_MSG_PAD_ZEROS = calc_sha_pad_zeros($bits(q_msg_data));
    localparam int unsigned Q_SUB_BLOCKS    = calc_sha_blocks($bits(q_sub_data));
    localparam int unsigned Q_SUB_PAD_ZEROS = calc_sha_pad_zeros($bits(q_sub_data));

    wire [Q_MSG_BLOCKS*512-1:0] q_msg_padded =
            {q_msg_data, 1'b1, {Q_MSG_PAD_ZEROS{1'b0}}, 64'($bits(q_msg_data))};
    wire [Q_SUB_BLOCKS*512-1:0] q_sub_padded =
            {q_sub_data, 1'b1, {Q_SUB_PAD_ZEROS{1'b0}}, 64'($bits(q_sub_data))};

    // -------------------------------------------------------------------------
    // MGF1: second hash of the message digest, over the randomizer and the
    // first digest (parked in sha_ctx)
    // -------------------------------------------------------------------------

    localparam int unsigned MGF1_MSG_W = DIGEST_EXT_MSG_BITS;

    wire [MGF1_MSG_W-1:0] mgf1_data = digest_ext_msg(.sch        (SCH),
                                                     .kctx       (cur_I),
                                                     .randomizer (MAX_DATA_W'(randomizer)),
                                                     .digest     (sha_ctx_q));

    localparam int unsigned MGF1_BLOCKS    = calc_sha_blocks($bits(mgf1_data));
    localparam int unsigned MGF1_PAD_ZEROS = calc_sha_pad_zeros($bits(mgf1_data));

    wire [MGF1_BLOCKS*512-1:0] mgf1_padded =
            {mgf1_data, 1'b1, {MGF1_PAD_ZEROS{1'b0}}, 64'($bits(mgf1_data))};

    // Its digest, split into the signand and the tree and leaf indices
    wire msg_digest_t msg_digest = msg_digest_split(.sch    (SCH),
                                                    .digest (sha_digest));

    // -------------------------------------------------------------------------
    // WOTS chain step
    // -------------------------------------------------------------------------

    localparam int unsigned WOTS_MSG_W = ots_chain_msg_bits(SCH);

    wire [WOTS_MSG_W-1:0] wots_data = WOTS_MSG_W'(ots_chain_msg(.sch  (SCH),
                                                                .kctx (cur_I),
                                                                .ctrl (ctrl),
                                                                .tmp  (MAX_DATA_W'(hash_reg_q))));

    localparam int unsigned WOTS_BLOCKS    = calc_sha_blocks($bits(wots_data));
    localparam int unsigned WOTS_PAD_ZEROS = calc_sha_pad_zeros($bits(wots_data));

    wire [WOTS_BLOCKS*512-1:0] wots_padded =
            {wots_data, 1'b1, {WOTS_PAD_ZEROS{1'b0}}, 64'($bits(wots_data))};

    // -------------------------------------------------------------------------
    // Kc: the OTS public-key hash, accumulated incrementally
    //
    // Absorbs (StWotsAccum, when the bank holds its quota) assemble the data
    // from the prefix or the carry, the banked endpoints and the top of the
    // endpoint still sitting in hash_reg; the rest of it becomes the next
    // carry. StKcFinalWots then only absorbs the final block: the last carry,
    // whatever is still banked, and padding.
    //
    // The FORS public key is accumulated over the FORS roots the same way,
    // by StMrklAccum and StKcFinalFors.
    // -------------------------------------------------------------------------

    localparam int unsigned KC_OTS_W  = KC_PREFIX_W + OTS_LEN*DW;
    localparam int unsigned KC_FORS_W = KC_PREFIX_W + FORS_K*DW;

    wire kc_final     = (seq_q == StKcFinalWots) || (seq_q == StKcFinalFors);
    wire kc_accum     = ((seq_q == StWots) && (wots_q == StWotsAccum))
                     || ((seq_q == StFors) && (mrkl_q == StMrklAccum));

    // The endpoint in hash_reg closes a block when the bank holds its quota
    wire kc_closes    = (int'(kc_pos_q) == (kc_first_q ? int'(KC_FIRST) : int'(KC_MID)));
    wire kc_absorbing = (kc_accum && kc_closes) || kc_final;

    // Strobes of the accumulation registers below: every endpoint lands in
    // hash_reg as usual and is copied from there during the Accum step, and
    // the suspended state is latched back at each save's ready pulse.
    wire kc_saved = sha_save && sha_ready;

    // The endpoint is dealt with: banked, or absorbed with its block
    wire kc_done  = !kc_closes || kc_saved;

    wire [KC_PREFIX_W-1:0] kc_prefix =
            (seq_q == StWots) ? KC_PREFIX_W'(ots_pk_prefix( .sch  (SCH),
                                                            .kctx (cur_I),
                                                            .ctrl (ctrl)))
                              : KC_PREFIX_W'(fors_pk_prefix(.sch  (SCH),
                                                            .kctx (cur_I),
                                                            .ctrl (ctrl)));

    // First absorb: the prefix, the first banked endpoints and the top of
    // the closing one. Later ones: the carry, the bank and the next top.
    // Note: KC_MID is the size of kc_bank, but we only use KC_FIRST elements from it here
    wire [KC_FIRST_BLOCKS*512-1:0] kc_first_data =
            {kc_prefix, kc_bank_q[KC_MID-1 -: KC_FIRST], hash_reg_q[DW-1 -: KC_TOP_W]};
    wire [511:0]                   kc_mid_block  =
            {kc_hi_q, kc_bank_q, hash_reg_q[DW-1 -: KC_TOP_W]};

    int unsigned  kc_absorb_blocks;
    logic [511:0] kc_absorb_block;

    /* verilator lint_off UNUSEDSIGNAL */
    logic [$bits(kc_first_data)-1:0] kc_first_discard;
    /* verilator lint_on UNUSEDSIGNAL */

    always_comb begin
        kc_first_discard = '0;

        if (kc_first_q) begin
            kc_absorb_blocks = KC_FIRST_BLOCKS;
            {kc_absorb_block, kc_first_discard} =
                    {kc_first_data, 512'b0} << (int'(blk_idx_q) * 512);
        end else begin
            kc_absorb_blocks = 1;
            kc_absorb_block  = kc_mid_block;
        end
    end

    // Final padding block: the carry, the tail endpoints still banked, then
    // padding -- an elaboration-time layout per accumulation. The loop runs
    // over all KC_MID bank slots because it needs a constant bound, while
    // the tail differs between the OTS and the FORS accumulation.
    function automatic logic [511:0] kc_final_layout(
        input int unsigned               tail,
        input int unsigned               len_bits,
        input logic [KC_CARRY_W-1:0]     carry,
        input logic [KC_MID-1:0][DW-1:0] bank);
        logic [511:0] block;
        block = '0;
        block[511 -: KC_CARRY_W] = carry;
        for (int i = 0; i < KC_MID; i++) begin
            if (i < tail) block[511 - KC_CARRY_W - i*DW -: DW] = bank[KC_MID-1-i];
        end
        block[511 - KC_CARRY_W - tail*DW] = 1'b1;
        block[63:0] = 64'(len_bits);
        return block;
    endfunction

    wire [511:0] kc_final_ots  = kc_final_layout(.tail     (KC_TAIL_OTS),
                                                 .len_bits (KC_OTS_W),
                                                 .carry    (kc_hi_q),
                                                 .bank     (kc_bank_q));
    wire [511:0] kc_final_fors = kc_final_layout(.tail     (KC_TAIL_FORS),
                                                 .len_bits (KC_FORS_W),
                                                 .carry    (kc_hi_q),
                                                 .bank     (kc_bank_q));

    // Every Kc absorb after the first resumes the hash from the saved state.
    assign sha_restore = kc_absorbing && !kc_first_q;

    // -------------------------------------------------------------------------
    // MSS leaf: the hash over Kc
    // -------------------------------------------------------------------------

    localparam int unsigned MSS_LEAF_MSG_W = MSS_LEAF_MSG_BITS;

    wire [MSS_LEAF_MSG_W-1:0] mss_leaf_data = mss_leaf_msg(.sch  (SCH),
                                                           .kctx (cur_I),
                                                           .ctrl (ctrl),
                                                           .kc   (MAX_DATA_W'(hash_reg_q)));

    localparam int unsigned MSS_LEAF_BLOCKS    = calc_sha_blocks($bits(mss_leaf_data));
    localparam int unsigned MSS_LEAF_PAD_ZEROS = calc_sha_pad_zeros($bits(mss_leaf_data));

    wire [MSS_LEAF_BLOCKS*512-1:0] mss_leaf_padded =
            {mss_leaf_data, 1'b1, {MSS_LEAF_PAD_ZEROS{1'b0}}, 64'($bits(mss_leaf_data))};

    // -------------------------------------------------------------------------
    // FORS leaf: hash of the secret element, which is the beat on the bus
    // -------------------------------------------------------------------------

    localparam int unsigned FORS_LEAF_MSG_W = FORS_LEAF_MSG_BITS;

    wire [FORS_LEAF_MSG_W-1:0] fors_leaf_data = fors_leaf_msg(.sch    (SCH),
                                                              .kctx   (cur_I),
                                                              .ctrl   (ctrl),
                                                              .secret (MAX_DATA_W'(data)));

    localparam int unsigned FORS_LEAF_BLOCKS    = calc_sha_blocks($bits(fors_leaf_data));
    localparam int unsigned FORS_LEAF_PAD_ZEROS = calc_sha_pad_zeros($bits(fors_leaf_data));

    wire [FORS_LEAF_BLOCKS*512-1:0] fors_leaf_padded =
            {fors_leaf_data, 1'b1, {FORS_LEAF_PAD_ZEROS{1'b0}}, 64'($bits(fors_leaf_data))};

    // -------------------------------------------------------------------------
    // Merkle helpers
    // -------------------------------------------------------------------------

    // The sibling, and the secret element of a FORS leaf, are read straight
    // off the stream: the hash is only offered to the core while the beat is
    // present, and the beat is released (ready) with the hash's completion,
    // so valid and block stay stable until ready.
    wire data_mss_wants = (mrkl_q == StMrklJoin) ||
                          ((seq_q == StFors) && (mrkl_q == StMrklLeaf));
    wire data_mss_ready = data_mss_wants && hash_complete;

    // Nodes are indexed as 2n (left) and 2n+1 (right) from their parent.
    // The leaf is node 2^h + q and each level up halves the node number, so
    // the node at the current level is a right child iff that bit of q is
    // set; the node number itself is derived the same way in the package.
    wire is_right = mrkl_leaf[mrkl_level_q];

    // The sibling is the beat on the bus
    logic [DW-1:0] left_node;
    logic [DW-1:0] right_node;

    assign {left_node, right_node} = is_right ? {data,       hash_reg_q}
                                              : {hash_reg_q, data};

    // -------------------------------------------------------------------------
    // MSS node: the parent hash over a left and a right child
    // -------------------------------------------------------------------------

    localparam int unsigned MSS_JOIN_MSG_W = mss_join_msg_bits(SCH);

    wire [MSS_JOIN_MSG_W-1:0] mss_join_data =
            MSS_JOIN_MSG_W'(mss_join_msg(.sch   (SCH),
                                         .kctx  (cur_I),
                                         .ctrl  (ctrl),
                                         .left  (MAX_DATA_W'(left_node)),
                                         .right (MAX_DATA_W'(right_node))));

    localparam int unsigned MSS_JOIN_BLOCKS    = calc_sha_blocks($bits(mss_join_data));
    localparam int unsigned MSS_JOIN_PAD_ZEROS = calc_sha_pad_zeros($bits(mss_join_data));

    wire [MSS_JOIN_BLOCKS*512-1:0] mss_join_padded =
            {mss_join_data, 1'b1, {MSS_JOIN_PAD_ZEROS{1'b0}}, 64'($bits(mss_join_data))};

    // -------------------------------------------------------------------------
    // FORS node: parent hash over a left and a right child of a FORS tree
    // -------------------------------------------------------------------------

    localparam int unsigned FORS_JOIN_MSG_W = FORS_JOIN_MSG_BITS;

    wire [FORS_JOIN_MSG_W-1:0] fors_join_data = fors_join_msg(.sch   (SCH),
                                                              .kctx  (cur_I),
                                                              .ctrl  (ctrl),
                                                              .left  (MAX_DATA_W'(left_node)),
                                                              .right (MAX_DATA_W'(right_node)));

    localparam int unsigned FORS_JOIN_BLOCKS    = calc_sha_blocks($bits(fors_join_data));
    localparam int unsigned FORS_JOIN_PAD_ZEROS = calc_sha_pad_zeros($bits(fors_join_data));

    wire [FORS_JOIN_BLOCKS*512-1:0] fors_join_padded =
            {fors_join_data, 1'b1, {FORS_JOIN_PAD_ZEROS{1'b0}}, 64'($bits(fors_join_data))};


    // -------------------------------------------------------------------------
    // SHA block counter, last block flag and block selection
    // -------------------------------------------------------------------------

    // Helper variable
    int unsigned num_blocks;
    int unsigned blk_shift;

    // Unused bits from shift output
    /* verilator lint_off UNUSEDSIGNAL */
    logic [$bits(q_msg_padded)-1:0]     q_msg_discard;
    logic [$bits(q_sub_padded)-1:0]     q_sub_discard;
    logic [$bits(mgf1_padded)-1:0]      mgf1_discard;
    logic [$bits(wots_padded)-1:0]      wots_discard;
    logic [$bits(mss_leaf_padded)-1:0]  mss_leaf_discard;
    logic [$bits(fors_leaf_padded)-1:0] fors_leaf_discard;
    logic [$bits(mss_join_padded)-1:0]  mss_join_discard;
    logic [$bits(fors_join_padded)-1:0] fors_join_discard;
    /* verilator lint_on UNUSEDSIGNAL */

    // Last block of the hash, or of the Kc absorb
    wire blk_last = (int'(blk_idx_q) == num_blocks-1) ? 1'b1 : 1'b0;

    // Block counter — a Kc absorb leaves it at zero like a finished hash
    // does: the next chain hash must still see index zero.
    always_comb begin
        blk_idx_d = blk_idx_q;

        if (sha_ready) begin
            blk_idx_d = ~(sha_last || sha_save) ? blk_idx_q + 1 : 0;
        end
    end
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            blk_idx_q <= '0;
        end else begin
            blk_idx_q <= blk_idx_d;
        end
    end

    // Last block flag. Only the final padding block closes the Kc message,
    // every other absorb suspends it with its last block.
    assign sha_last = kc_absorbing ? kc_final : blk_last;
    assign sha_save = kc_absorbing && !kc_final && blk_last;

    // Input vector and block selection
    always_comb begin
        blk_shift = int'(blk_idx_q) * 512;

        num_blocks =  0;
        sha_block  = '0;

        q_msg_discard     = '0;
        q_sub_discard     = '0;
        mgf1_discard      = '0;
        wots_discard      = '0;
        mss_leaf_discard  = '0;
        fors_leaf_discard = '0;
        mss_join_discard  = '0;
        fors_join_discard = '0;

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
            StFors: begin
                unique case (mrkl_q)
                    StMrklLeaf: begin
                        num_blocks = FORS_LEAF_BLOCKS;
                        {sha_block, fors_leaf_discard} = {fors_leaf_padded, 512'b0} << blk_shift;
                    end
                    StMrklJoin: begin
                        num_blocks = FORS_JOIN_BLOCKS;
                        {sha_block, fors_join_discard} = {fors_join_padded, 512'b0} << blk_shift;
                    end
                    StMrklAccum: begin
                        num_blocks = kc_absorb_blocks;
                        sha_block  = kc_absorb_block;
                    end
                    default: ;
                endcase
            end
            StKcFinalFors: begin
                num_blocks = 1;
                sha_block  = kc_final_fors;
            end
            StWots: begin
                if (wots_q == StWotsAccum) begin
                    num_blocks = kc_absorb_blocks;
                    sha_block  = kc_absorb_block;
                end else begin
                    num_blocks = WOTS_BLOCKS;
                    {sha_block, wots_discard} = {wots_padded, 512'b0} << blk_shift;
                end
            end
            StKcFinalWots: begin
                num_blocks = 1;
                sha_block  = kc_final_ots;
            end
            StMss: begin
                if (mrkl_q == StMrklLeaf) begin
                    num_blocks = MSS_LEAF_BLOCKS;
                    {sha_block, mss_leaf_discard} = {mss_leaf_padded, 512'b0} << blk_shift;
                end else begin
                    num_blocks = MSS_JOIN_BLOCKS;
                    {sha_block, mss_join_discard} = {mss_join_padded, 512'b0} << blk_shift;
                end
            end
            default: ;
        endcase
    end

    // -------------------------------------------------------------------------
    // hash_reg — captures sha_digest on completion, or sig chain on WOTS load
    // -------------------------------------------------------------------------

    // The hash in progress reads the randomizer off the bus: it is offered
    // to the core only while the beat is present, and the beat is released
    // when the last hash that reads it completes.
    wire use_randomizer = !HAS_HDR && ((seq_q == StQ) || (seq_q == StMgf1));

    // The layer header beat as HSS lays it out; not generalized, SLH has no header
    /* verilator lint_off UNUSEDSIGNAL */  // padding
    wire hss_pkg::layer_hdr_t hss_hdr = hss_pkg::layer_hdr_t'(data);
    /* verilator lint_on UNUSEDSIGNAL */

    wire data_hdr_wants    = (seq_q == StQ) && (hdr_cnt_q != HDR_DONE);
    wire data_hdr_ready    = data_hdr_wants; // consumed immediately, unconditionally
    wire data_wots_wants   = (seq_q == StWots) && (wots_q == StWotsLoad);
    wire data_wots_ready   = data_wots_wants; // consumed immediately, unconditionally
    wire data_rand_wants   = use_randomizer && ((seq_q == StMgf1) || !EXTEND_DIGEST);
    wire data_rand_ready   = data_rand_wants && hash_complete;

    assign ready =  valid && (data_hdr_ready  ||
                              data_wots_ready ||
                              data_rand_ready ||
                              data_mss_ready);

    wire wots_loading = data_wots_wants && valid;

    // Qualified by valid so ready is never asserted on its own.

    wire hash_reg_en  = wots_loading | hash_complete;

    // Nodes are the leftmost DW bits of a digest
    assign hash_reg_d = (!wots_loading) ? sha_digest[255 -: DW] : data;

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

    wire wots_init  = (seq_q == StWots) && (wots_q == StWotsInit);

    // Scratch register, two time-disjoint producers: the randomizer while Q
    // is being set up and the Q digest through WOTS. Reusing it keeps the
    // randomizer out of storage entirely -- it is only ever read by the Q
    // hash. With the extended message digest it takes the signand split off
    // that digest for FORS, then whatever the chains sign (wots_init).
    wire rand_loading = (seq_q == StQ) && (hdr_cnt_q == HDR_CNT_W'(1)) && valid;
    wire md_loading   = (seq_q == StMgf1) && hash_complete;

    assign aux_reg_d = md_loading   ? SIGNAND_W'(msg_digest.signand) :
                       rand_loading ? SIGNAND_W'(data) : SIGNAND_W'(hash_reg_q);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aux_reg_q <= '0;
        end else if (wots_init || rand_loading || md_loading) begin
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
            kc_bank_q <= '0;
            kc_tail_q <= '0;
        end else if (kc_accum && (!kc_closes || !sha_ready)) begin
            if (kc_closes) begin
                kc_tail_q <= hash_reg_q[KC_CARRY_W-1:0];
            end else begin
                // the first banked in the most significant position
                for (int i = 0; i < KC_MID; i++) begin
                    if (int'(kc_pos_q) == i) kc_bank_q[KC_MID-1-i] <= hash_reg_q;
                end
            end
        end
    end

    // At the absorb's ready pulse sha_digest holds the resumable state, and
    // the staged tail becomes the carry of the next Kc block. The first
    // digest of an extended message hash is parked here too.
    wire sha_ctx_en = kc_saved || (EXTEND_DIGEST && (seq_q == StQ) && hash_complete);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            sha_ctx_q <= '0;
            kc_hi_q   <= '0;
        end else begin
            if (sha_ctx_en) sha_ctx_q <= sha_digest;
            if (kc_saved)   kc_hi_q   <= kc_tail_q;
        end
    end

    // Bank position and first-absorb flag: stepped with every endpoint,
    // re-armed by the final block for the next accumulation.
    always_comb begin
        kc_pos_d   = kc_pos_q;
        kc_first_d = kc_first_q;

        if (kc_accum && kc_done) begin
            kc_pos_d   = kc_closes ? '0 : kc_pos_q + 1'b1;
            kc_first_d = kc_first_q && !kc_closes;
        end
        if (kc_final && hash_complete) begin
            kc_pos_d   = '0;
            kc_first_d = 1'b1;
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

        wots_sha_valid = 1'b0;
        wots_complete  = 1'b0;

        // Only activate when main FSM is in WOTS state
        if (seq_q == StWots) begin

            unique case (wots_q)
                StWotsInit: begin
                    wots_chain_d = '0;
                    wots_step_d  = '0;
                    // aux_reg captures hash_reg (Q hash) this cycle also
                    // (outside this always_comb since aux_reg is shared)

                    wots_d = StWotsLoad;
                end

                StWotsLoad: begin
                    // Stall until the next chain element arrives.
                    if (valid) begin
                        // load step counter from the signed digit
                        wots_step_d = HASH_IDX_W'(cur_digit);
                        // hash_reg captures the chain signature this cycle too
                        // (outside this always_comb since hash_reg is shared)

                        // hash unless the digit is already the maximum value
                        wots_d = (cur_digit != DIGIT_MAX) ? StWotsHash : StWotsAccum;
                    end
                end

                StWotsHash: begin
                    // Start the hash and wait to complete
                    wots_sha_valid = 1'b1;
                    if (hash_complete) begin
                        // increment step counter
                        wots_step_d = wots_step_q + 1;

                        // continue hashing if this was not the last hash,
                        // otherwise move to fold the endpoint into Kc
                        wots_d = (wots_step_q != HASH_IDX_W'(DIGIT_MAX-1)) ? StWotsHash
                                                                           : StWotsAccum;
                    end
                end

                StWotsAccum: begin
                    // Bank the endpoint (kc_bank copies it from hash_reg
                    // this cycle) and advance — endpoints still banked at
                    // the last chain ride in the final padding block. Or,
                    // when it closes a block: absorb the assembled data
                    // into the suspended Kc hash and advance on its ready;
                    // sha_ctx/kc_hi latch there too.
                    if (kc_closes) begin
                        wots_sha_valid = 1'b1;
                    end
                    if (kc_done) begin
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

        // Only activate when main FSM is in Mss or Fors state
        if ((seq_q == StMss) || (seq_q == StFors)) begin

            unique case (mrkl_q)
                StMrklInit: begin
                    // Hash the MSS tree leaf first if needed, FORS leaf is always hashed first
                    mrkl_d = (MSS_LEAF_HASH || (seq_q == StFors)) ? StMrklLeaf : StMrklJoin;
                end

                StMrklLeaf: begin
                    // Start the leaf hash and wait to complete. A FORS leaf
                    // is hashed straight off the bus, like a sibling.
                    mrkl_sha_valid = (seq_q == StFors) ? valid : 1'b1;
                    if (hash_complete) begin
                        mrkl_d = StMrklJoin;
                    end
                end

                StMrklJoin: begin
                    // Hash with the sibling straight off the bus: the core is
                    // fed only while the beat is present, and the beat is
                    // released with the hash's completion.
                    mrkl_sha_valid = valid;
                    if (hash_complete) begin
                        // Continue joining if not the last level,
                        // otherwise move on:
                        // complete for MSS
                        // accum for FORS
                        mrkl_level_d  = ~last_level ? mrkl_level_q+1 : '0;
                        mrkl_d        = ~last_level        ? StMrklJoin  :
                                        (seq_q != StFors)  ? StMrklInit  : StMrklAccum;
                        mrkl_complete = (mrkl_d == StMrklInit);
                    end
                end

                StMrklAccum: begin
                    // Bank the FORS root or, when it closes a block, absorb
                    // the assembled data and advance on its ready -- as
                    // StWotsAccum does with a chain endpoint. Then on to
                    // the next FORS tree.
                    if (kc_closes) begin
                        mrkl_sha_valid = 1'b1;
                    end
                    if (kc_done) begin
                        mrkl_d        = StMrklInit;
                        mrkl_complete = 1'b1;
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
        leaf_idx_d    = leaf_idx_q;
        leaf_ovf_d    = leaf_ovf_q;
        fors_tree_d   = fors_tree_q;
        tree_idx_d    = tree_idx_q;
        cur_I_d       = cur_I_q;
        prev_I_d      = prev_I_q;
        hdr_cnt_d     = hdr_cnt_q;
        sha_valid     = 1'b0;
        verify_done   = 1'b0;
        verif_passed  = 1'b0;

        unique case (seq_q)

            StIdle: begin
                // The first beat offered starts the verification; it is not
                // consumed here, so the producer holds it
                // until StQ takes it.
                if (valid) begin
                    // Start at the bottom layer (signs the user message)
                    layer_d   = HT_LAYER_W'(LAYERS - 1);
                    hdr_cnt_d = '0;
                    seq_d     = StQ;
                end
            end

            // The states below are responsible to start the hashing and process the completion
            // The rest (feeding the appropriate inputs to the SHA block) is taken care outisde this
            // always_comb block based on the FSM state and sub-FSM states

            StQ: begin
                // Take this layer's header off the stream first: beat 0 is
                // {leaf_idx, sub_I}, beat 1 the randomizer (latched into
                // aux_reg outside this block). Hashing starts once both are in.
                if (hdr_cnt_q != HDR_DONE) begin
                    if (valid) begin
                        if (hdr_cnt_q == '0) begin
                            // Keep the identifier this layer used: the layer
                            // above signs our public key and needs it.
                            prev_I_d     = cur_I_q;
                            cur_I_d      = hss_hdr.sub_i;
                            leaf_idx_d   = LEAF_W'(hss_hdr.leaf_index);
                            leaf_ovf_d   = |(hss_hdr.leaf_index >> LEAF_W);
                        end
                        hdr_cnt_d = hdr_cnt_q + 1'b1;
                    end
                end else begin
                    // hdr_cnt_q == HDR_DONE: both header beats have been captured.
                    // Start Q hash and wait to complete
                    sha_valid = use_randomizer ? valid : 1'b1;
                    if (hash_complete) begin
                        hdr_cnt_d = '0;
                        seq_d     = EXTEND_DIGEST ? StMgf1 : StWots;
                    end
                end
            end

            StMgf1: begin
                // Start the second hash and wait to complete. Its digest
                // gives the signand (latched into aux_reg outside this
                // block) and the tree and leaf that sign it.
                sha_valid = use_randomizer ? valid : 1'b1;
                if (hash_complete) begin
                    tree_idx_d = TREE_W'(msg_digest.tree_idx);
                    leaf_idx_d = LEAF_W'(msg_digest.leaf_idx);
                    seq_d = StFors;
                end
            end

            StFors: begin
                // The FORS step has multiple iterations, delegate hash control to Merkle sub-FSM
                sha_valid = mrkl_sha_valid;
                if (mrkl_complete) begin
                    fors_tree_d = ~last_fors_tree ? fors_tree_q+1 : '0;
                    seq_d       = ~last_fors_tree ? StFors        : StKcFinalFors;
                end
            end

            StKcFinalFors: begin
                // Start the FORS public key hash and wait to complete; the
                // chains sign its digest.
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StWots;
                end
            end

            StWots: begin
                // The WOTS step has multiple iterations, delegate hash control to WOTS sub-FSM
                sha_valid = wots_sha_valid;
                if (wots_complete) begin
                    seq_d = StKcFinalWots;
                end
            end

            StKcFinalWots: begin
                // Start Kc hash and wait to complete
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StMss;
                end
            end

            StMss: begin
                // The Merkle step has multiple iterations, delegate hash control to Merkle sub-FSM
                sha_valid = mrkl_sha_valid;
                if (mrkl_complete) begin
                    // The layer above hashes the root into its own message,
                    // or signs it as it is
                    seq_d   = is_pk_layer ? StDone :
                              SUB_PK_HASH ? StQ    : StWots;
                    layer_d = (~is_pk_layer) ? layer_q - 1'b1 : '0;
                    // and without that message there is no header to bring
                    // its leaf index: the index moves up a layer instead
                    if (!SUB_PK_HASH) begin
                        {tree_idx_d, leaf_idx_d} = (LAYERS * TREE_HT)'(tree_idx_q);
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
            leaf_idx_q <= '0;
            leaf_ovf_q <= 1'b0;
            tree_idx_q   <= '0;
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
            fors_tree_q   <= '0;
            layer_q       <= '0;
        end else begin
            leaf_idx_q <= leaf_idx_d;
            leaf_ovf_q <= leaf_ovf_d;
            tree_idx_q   <= tree_idx_d;
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
            fors_tree_q   <= fors_tree_d;
            layer_q       <= layer_d;
        end
    end

endmodule
