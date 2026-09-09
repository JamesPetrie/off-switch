// HSS-LMS Signature Verification
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
    parameter sch_e SCH = SCHEME_LMS,

    // Node, signature-element and licence-beat width
    localparam int unsigned DW = digest_w(SCH)
) (
    input  logic               clk,
    input  logic               rst_n,
    input  logic [WIDTH-1:0]   message,
    // TODO replace individual public key inputs with the struct
    input  logic [KCTX_W-1:0]  identifier,   // tree identifier
    input  logic [DW-1:0]      root_pub_key,

    // License beat stream, in the field order of the standard signature
    // format (see hss_pkg). Per layer, from LAYERS-1 down to 0: a header
    // beat carrying {leaf_index, sub_I}, the randomizer, OTS_LEN chain
    // signatures, then TREE_HT auth path siblings. Each beat is consumed where
    // it is needed, so only the current layer's identity is held.
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
    localparam int unsigned LEVEL_W  = level_w(SCH);    // Merkle level counter width
    localparam int unsigned OTS_LEN1 = ots_len1(SCH);   // WOTS data digits
    localparam int unsigned OTS_LEN  = ots_len(SCH);    // WOTS chains (data + checksum)
    localparam int unsigned DIGIT_W  = digit_w(SCH);    // Winternitz digit width
    localparam int unsigned CSUM_LS  = csum_shift(SCH); // checksum left shift

    localparam int unsigned LAYER_W = (LAYERS > 1) ? $clog2(LAYERS) : 1;
    localparam int unsigned CHAIN_W = $clog2(OTS_LEN);

    // WOTS digit maximum value (all 1s)
    localparam logic [DIGIT_W-1:0] DIGIT_MAX = '1;

    // -------------------------------------------------------------------------
    // FSM state types
    // -------------------------------------------------------------------------

    typedef enum logic [3:0] {
        StIdle, StQ, StWots, StKcFinal, StLeaf, StMerkle, StDone
    } seq_state_e;

    // Header beats per layer: {leaf_index, sub_I} then the randomizer.
    localparam int unsigned HDR_BEATS = hdr_beats(SCH);
    localparam int unsigned HDR_CNT_W = $clog2(HDR_BEATS + 1);
    localparam logic [HDR_CNT_W-1:0] HDR_DONE = HDR_CNT_W'(HDR_BEATS);

    typedef enum logic [1:0] {
        StWotsInit, StWotsLoad, StWotsHash, StWotsAccum
    } wots_state_e;


    typedef enum logic [1:0] {
        StMrklInit, StMrklLoad, StMrklHash
    } mrkl_state_e;

    // -------------------------------------------------------------------------
    // Kc interleaving geometry
    //
    // The Kc message is I || q || D_PBLC followed by the OTS_LEN chain
    // endpoints; hbsv_schs_pkg derives the block geometry from the endpoint
    // width. With 256-bit endpoints the prefix offsets the endpoint stream so
    // that each endpoint pair completes exactly one 512-bit block, leaving
    // the same KC_CARRY_W-bit carry after every absorb. The final padding
    // block then carries the leftover: just the carry when OTS_LEN is even,
    // or the carry plus the unpaired banked endpoint when OTS_LEN is odd
    // (both fit, with 65 padding bits after 176 respectively 432 bits of
    // data).
    // -------------------------------------------------------------------------

    localparam int unsigned KC_PREFIX_W = ACC_PREFIX_W;                 // I || q || D_PBLC
    localparam int unsigned KC_FIRST    = acc_first_full(SCH);          // banked in block 0
    localparam int unsigned KC_MID      = acc_mid_full(SCH);            // banked in later blocks
    localparam int unsigned KC_TOP_W    = acc_head_w(SCH);              // head bits closing a block
    localparam int unsigned KC_CARRY_W  = acc_carry_w(SCH);             // tail bits carried over
    localparam int unsigned KC_TAIL     = acc_tail_elems(SCH, OTS_LEN); // still banked at the end

    // The interleaving below banks one endpoint per block (the even one)
    // and closes the block with the next. That is the geometry of 256-bit
    // endpoints; a scheme that fits more endpoints per block would need a
    // bank of KC_MID of them.
    if (KC_FIRST != 1 || KC_MID != 1) begin : gen_kc_unsupported
        $error("hss_verify: Kc interleaving supports one banked endpoint per block");
    end

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
    logic [31:0]            leaf_index_q, leaf_index_d;
    logic [KCTX_W-1:0]      cur_I_q,      cur_I_d;
    logic [KCTX_W-1:0]      prev_I_q,     prev_I_d;
    logic [HDR_CNT_W-1:0]   hdr_cnt_q,    hdr_cnt_d;
    wots_state_e  wots_q,  wots_d;
    mrkl_state_e  mrkl_q,  mrkl_d;

    // Hash register — working hash output across all phases
    logic [DW-1:0]    hash_reg_q,    hash_reg_d;

    // Auxiliary register — companion value alongside hash_reg
    // WOTS: holds Q hash
    logic [DW-1:0]    aux_reg_q,     aux_reg_d;

    // Shared block counter — indexes SHA-256 blocks within a multi-block hash
    // REVISIT hardcoded widhts
    logic [4:0]       blk_idx_q,     blk_idx_d;

    // WOTS counters (driven by WOTS sub-FSM)
    logic [CHAIN_W-1:0] wots_chain_q, wots_chain_d; // chain index 0..OTS_LEN-1
    logic [DIGIT_W-1:0] wots_step_q,  wots_step_d;  // step within chain

    // Merkle tree level (driven by Merkle sub-FSM)
    logic [LEVEL_W-1:0] mrkl_level_q, mrkl_level_d;

    // Kc interleaved accumulation (replaces the former 34 x 256-bit pk store):
    // suspended SHA state, the banked even endpoint, the current carry and
    // the staged next carry (the odd endpoint's tail).
    //
    // REVISIT: kc_tail stages the next carry so it is not read back from
    // hash_reg after the absorb, the one spot that would otherwise rely on
    // the digest being registered twice (core and verifier — see the
    // design-doc limitation). Revisit together with that limitation.
    logic [255:0]           kc_state_q;
    logic [DW-1:0]          kc_lo_q;
    logic [KC_CARRY_W-1:0]  kc_hi_q;
    logic [KC_CARRY_W-1:0]  kc_tail_q;

    // Merkle node index
    logic [31:0] node_index_q, node_index_d;

    // Hypertree layer counter
    logic [LAYER_W-1:0] layer_q, layer_d;

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
        .ctx     (kc_state_q),
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
    wire [KCTX_W-1:0] cur_I = is_pk_layer ? identifier
                                          : cur_I_q;

    // -------------------------------------------------------------------------
    // Control bundle — the counters in the form the hash messages read them
    // -------------------------------------------------------------------------

    wire ctrl_t ctrl = '{layer: 3'(layer_q),
                         chain: 7'(wots_chain_q),
                         step:  8'(wots_step_q),
                         level: 5'(mrkl_level_q),
                         nidx:  node_index_q,
                         leaf:  leaf_index_q};

    // -------------------------------------------------------------------------
    // Data indexed by WOTS chain / Merkle level
    // -------------------------------------------------------------------------

    wire             last_chain    = (int'(wots_chain_q) == OTS_LEN-1) ? 1'b1 : 1'b0;

    wire             last_level    = (int'(mrkl_level_q) == TREE_HT-1) ? 1'b1 : 1'b0;

    // -------------------------------------------------------------------------
    // Q hash split into digits + checksum — computed combinationally
    // -------------------------------------------------------------------------

    logic [DIGIT_W-1:0] q_digits[OTS_LEN];

    // Using digit-wise shift left to avoid indexing issues
    always_comb begin
        logic [DW-1:0] hash;        // hash working variable
        logic [15:0]   csum;        // checksum working variable

        hash  = aux_reg_q;
        csum = '0;

        // Load the digits from q_hash and calculate the checksum
        for (int i = 0; i < OTS_LEN1; i++) begin

            // load the digit
            // shift hash left one digit, shift out to q_digits and shift in zeros
            {q_digits[i], hash} = {hash, DIGIT_W'(0)};

            // add the digit's contribution to the checksum
            csum += 16'(DIGIT_MAX) - 16'(q_digits[i]);
        end

        // Left-align the checksum in its digits (no shift for LMS w = 8)
        csum = csum << CSUM_LS;

        // Load the checksum digits
        for (int i = OTS_LEN1; i < OTS_LEN; i++) begin
            // shift csum left one digit, shift out to q_digits and shift in zeros
            {q_digits[i], csum} = {csum, DIGIT_W'(0)};
        end
    end

    wire [DIGIT_W-1:0] cur_digit = q_digits[wots_chain_q];

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

    localparam int unsigned Q_MSG_W = msg_hash_bits(SCH, 1'b0);
    localparam int unsigned Q_SUB_W = msg_hash_bits(SCH, 1'b1);

    wire [Q_MSG_W-1:0] q_msg_data = Q_MSG_W'(msg_hash_msg(SCH, cur_I, ctrl, aux_reg_q, message,
                                                          1'b0, prev_I_q, hash_reg_q));

    // sub_I is indexed at layer_q+1 (identity of the tree below)
    wire [Q_SUB_W-1:0] q_sub_data = Q_SUB_W'(msg_hash_msg(SCH, cur_I, ctrl, aux_reg_q, message,
                                                          1'b1, prev_I_q, hash_reg_q));

    localparam int unsigned Q_MSG_BLOCKS    = calc_sha_blocks($bits(q_msg_data));
    localparam int unsigned Q_MSG_PAD_ZEROS = calc_sha_pad_zeros($bits(q_msg_data));
    localparam int unsigned Q_SUB_BLOCKS    = calc_sha_blocks($bits(q_sub_data));
    localparam int unsigned Q_SUB_PAD_ZEROS = calc_sha_pad_zeros($bits(q_sub_data));

    wire [Q_MSG_BLOCKS*512-1:0] q_msg_padded =
            {q_msg_data, 1'b1, {Q_MSG_PAD_ZEROS{1'b0}}, 64'($bits(q_msg_data))};
    wire [Q_SUB_BLOCKS*512-1:0] q_sub_padded =
            {q_sub_data, 1'b1, {Q_SUB_PAD_ZEROS{1'b0}}, 64'($bits(q_sub_data))};

    // -------------------------------------------------------------------------
    // WOTS chain: H(I || q || i || j || tmp)
    // -------------------------------------------------------------------------

    localparam int unsigned WOTS_MSG_W = chain_msg_bits(SCH);

    wire [WOTS_MSG_W-1:0] wots_data = WOTS_MSG_W'(ots_chain_msg(SCH, cur_I, ctrl, hash_reg_q));

    // WOTS is designed to fit in a single block, assume BLOCKS=1
    //localparam int unsigned WOTS_BLOCKS    = calc_sha_blocks($bits(wots_data));
    localparam int unsigned WOTS_PAD_ZEROS = calc_sha_pad_zeros($bits(wots_data));

    wire [512-1:0] wots_padded =
            {wots_data, 1'b1, {WOTS_PAD_ZEROS{1'b0}}, 64'($bits(wots_data))};

    // -------------------------------------------------------------------------
    // Kc: H(I || q || D_PBLC || pk0..pk33), accumulated incrementally
    //
    // WOTS-phase absorbs (StWotsAccum, odd chains) assemble a data block from
    // the carry (the prefix itself for the first block), the banked even
    // endpoint and the head of the odd endpoint still sitting in hash_reg;
    // the tail of the odd endpoint becomes the next carry. StKcFinal then only
    // absorbs the final block: the last carry plus padding.
    // -------------------------------------------------------------------------

    localparam int unsigned KC_DATA_BITS = KC_PREFIX_W + OTS_LEN*DW;
    localparam int unsigned KC_PAD_ZEROS = calc_sha_pad_zeros(KC_DATA_BITS);

    wire kc_odd       = wots_chain_q[0];
    wire kc_first_blk = (wots_chain_q == CHAIN_W'(1));
    wire kc_final     = (seq_q == StKcFinal);
    wire kc_accum     = (seq_q == StWots) && (wots_q == StWotsAccum);
    wire kc_absorbing = (kc_accum && kc_odd) || kc_final;

    // Strobes of the accumulation registers below: every endpoint lands in
    // hash_reg as usual and is copied from there during StWotsAccum, and
    // the suspended state is latched back at each save's ready pulse.
    wire kc_saved = sha_save && sha_ready;

    wire [KC_PREFIX_W-1:0] kc_prefix = ots_pk_prefix(SCH, cur_I, ctrl);

    wire [KC_CARRY_W-1:0] kc_carry = kc_first_blk ? kc_prefix : kc_hi_q;

    wire [511:0] kc_absorb_block = {kc_carry, kc_lo_q,
                                    hash_reg_q[DW-1 -: KC_TOP_W]};

    // Final padding block: the carry alone (even OTS_LEN), or the carry plus
    // the unpaired banked endpoint (odd OTS_LEN).
    logic [511:0] kc_final_block;
    if (KC_TAIL == 1) begin : gen_kc_final_odd
        assign kc_final_block = {kc_hi_q, kc_lo_q, 1'b1, {KC_PAD_ZEROS{1'b0}},
                                 64'(KC_DATA_BITS)};
    end else begin : gen_kc_final_even
        assign kc_final_block = {kc_hi_q, 1'b1, {KC_PAD_ZEROS{1'b0}},
                                 64'(KC_DATA_BITS)};
    end

    // Every Kc block except the final one suspends the Kc hash; every one
    // after the first resumes it from the saved state.
    assign sha_save    = kc_absorbing && !kc_final;
    assign sha_restore = kc_absorbing && !kc_first_blk;

    // -------------------------------------------------------------------------
    // Leaf: H(I || q || D_LEAF || Kc)
    // -------------------------------------------------------------------------

    localparam int unsigned LEAF_MSG_W = leaf_msg_bits(SCH);

    wire [LEAF_MSG_W-1:0] leaf_data = LEAF_MSG_W'(leaf_msg(cur_I, ctrl, hash_reg_q));

    localparam int unsigned LEAF_BLOCKS    = calc_sha_blocks($bits(leaf_data));
    localparam int unsigned LEAF_PAD_ZEROS = calc_sha_pad_zeros($bits(leaf_data));

    wire [LEAF_BLOCKS*512-1:0] leaf_padded =
            {leaf_data, 1'b1, {LEAF_PAD_ZEROS{1'b0}}, 64'($bits(leaf_data))};

    // -------------------------------------------------------------------------
    // Merkle helpers
    // -------------------------------------------------------------------------

    wire mrkl_wants = (seq_q == StMerkle) && (mrkl_q == StMrklLoad);
    wire mrkl_loading = mrkl_wants && valid;

    // Nodes are indexed as 2n (left) and 2n+1 (right) from their parent
    wire [31:0]      parent_num = node_index_q >> 1; // node / 2
    wire             is_right   = node_index_q[0];

    // aux_reg holds the auth path sibling
    logic [DW-1:0] left_node;
    logic [DW-1:0] right_node;

    assign {left_node, right_node} = is_right ? {aux_reg_q,  hash_reg_q}
                                              : {hash_reg_q, aux_reg_q};

    // -------------------------------------------------------------------------
    // Merkle: H(I || parent || D_INTR || left || right)
    // -------------------------------------------------------------------------

    localparam int unsigned MRKL_MSG_W = tree_msg_bits(SCH);

    wire [MRKL_MSG_W-1:0] mrkl_data = MRKL_MSG_W'(ots_tree_join_msg(SCH, cur_I, ctrl,
                                                                    left_node, right_node));

    localparam int unsigned MRKL_BLOCKS    = calc_sha_blocks($bits(mrkl_data));
    localparam int unsigned MRKL_PAD_ZEROS = calc_sha_pad_zeros($bits(mrkl_data));

    wire [MRKL_BLOCKS*512-1:0] mrkl_padded =
            {mrkl_data, 1'b1, {MRKL_PAD_ZEROS{1'b0}}, 64'($bits(mrkl_data))};


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
            StWots: begin
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
    wire wots_wants = (seq_q == StWots) && (wots_q == StWotsLoad);
    wire wots_loading = wots_wants && valid;
    wire wants_data = hdr_wants || wots_wants || mrkl_wants;

    // Qualified by valid so ready is never asserted on its own.
    assign ready = valid && wants_data;
    wire hash_reg_en  = wots_loading | hash_complete;

    assign hash_reg_d = (!wots_loading) ? sha_digest : data;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            hash_reg_q <= '0;
        end else if (hash_reg_en) begin
            hash_reg_q <= hash_reg_d;
        end
    end

    // -------------------------------------------------------------------------
    // aux_reg — stores Q hash throughout WOTS, and auth siblings during Merkle
    // -------------------------------------------------------------------------

    wire wots_init  = (seq_q == StWots) && (wots_q == StWotsInit);

    // Scratch register, three time-disjoint producers: the randomizer while Q
    // is being set up, the Q digest through WOTS, and each auth sibling during
    // Merkle. Reusing it keeps the randomizer out of storage entirely -- it is
    // only ever read by the Q hash.
    wire rand_loading = (seq_q == StQ) && (hdr_cnt_q == HDR_CNT_W'(1)) && valid;

    assign aux_reg_d = (mrkl_loading || rand_loading) ? data : hash_reg_q;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aux_reg_q <= '0;
        end else if (wots_init || mrkl_loading || rand_loading) begin
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
            kc_lo_q   <= '0;
            kc_tail_q <= '0;
        end else if (kc_accum && (!kc_odd || !sha_ready)) begin
            if (kc_odd) kc_tail_q <= hash_reg_q[KC_CARRY_W-1:0];
            else        kc_lo_q   <= hash_reg_q;
        end
    end

    // At the absorb's ready pulse sha_digest holds the resumable state, and
    // the staged tail becomes the carry of the next Kc block.
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            kc_state_q <= '0;
            kc_hi_q    <= '0;
        end else if (kc_saved) begin
            kc_state_q <= sha_digest;
            kc_hi_q    <= kc_tail_q;
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
                        wots_step_d = cur_digit; // load step counter from the signed digit
                        // hash_reg captures the chain signature this cycle too
                        // (outside this always_comb since hash_reg is shared)

                        // hash unless the digit is already the maximum value
                        wots_d = (cur_digit != DIGIT_MAX) ? StWotsHash : StWotsAccum;
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
                        wots_d = (wots_step_q != DIGIT_MAX-1) ? StWotsHash : StWotsAccum;
                    end
                end

                StWotsAccum: begin
                    // Even chain: bank the endpoint (kc_lo copies it from
                    // hash_reg this cycle) and advance — on the last chain
                    // (odd OTS_LEN) it rides in the final padding block.
                    // Odd chain: absorb the assembled block into the
                    // suspended Kc hash and advance on its ready;
                    // kc_state/kc_hi latch there too.
                    if (kc_odd) begin
                        wots_sha_valid = 1'b1;
                    end
                    if (!kc_odd || sha_ready) begin
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
        node_index_d    = node_index_q;

        mrkl_sha_valid  = 1'b0;
        mrkl_complete   = 1'b0;

        // Only activate when main FSM is in Merkle state
        if (seq_q == StMerkle) begin

            unique case (mrkl_q)
                StMrklInit: begin
                    // initialize node_index from license
                    // set bit h to convert leaf index to node index
                    // (nodes above might use leaf_index but with bit[h]=0)
                    node_index_d = (32'd1 << TREE_HT) | leaf_index_q;

                    mrkl_d = StMrklLoad;
                end

                StMrklLoad: begin
                    // The sibling must stay stable for the whole two-block
                    // hash, so it is latched into aux_reg rather than used
                    // straight off the bus.
                    if (valid) begin
                        mrkl_d = StMrklHash;
                    end
                end

                StMrklHash: begin
                    // Start the hash and wait to complete
                    mrkl_sha_valid = 1'b1;
                    if (hash_complete) begin
                        // Increment level count and set node index to parent
                        // or clear counter and node index
                        mrkl_level_d = ~last_level ? mrkl_level_q+1 : '0;
                        node_index_d = ~last_level ? parent_num     : '0;
                        mrkl_d = ~last_level ? StMrklLoad : StMrklInit;

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
                    layer_d   = LAYER_W'(LAYERS - 1);
                    hdr_cnt_d = '0;
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
                            leaf_index_d = data[DW-1 -: 32];
                            // Keep the identifier this layer used: the layer
                            // above signs our public key and needs it.
                            prev_I_d     = cur_I_q;
                            cur_I_d      = data[DW-33 -: KCTX_W];
                        end
                        hdr_cnt_d = hdr_cnt_q + 1'b1;
                    end
                end else begin
                    // hdr_cnt_q == HDR_DONE: both header beats have been captured.
                    // Start Q hash and wait to complete
                    sha_valid = 1'b1;
                    if (hash_complete) begin
                        hdr_cnt_d = '0;
                        seq_d     = StWots;
                    end
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
                // Start Kc hash and wait to complete
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StLeaf;
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
                    seq_d   = (~is_pk_layer) ? StQ : StDone;
                    layer_d = (~is_pk_layer) ? layer_q - 1'b1 : '0;
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
            cur_I_q      <= '0;
            prev_I_q     <= '0;
            hdr_cnt_q    <= '0;
            seq_q         <= StIdle;
            wots_q        <= StWotsInit;
            wots_chain_q  <= '0;
            wots_step_q   <= '0;
            mrkl_q        <= StMrklInit;
            mrkl_level_q  <= '0;
            node_index_q  <= '0;
            layer_q       <= '0;
        end else begin
            leaf_index_q <= leaf_index_d;
            cur_I_q      <= cur_I_d;
            prev_I_q     <= prev_I_d;
            hdr_cnt_q    <= hdr_cnt_d;
            seq_q         <= seq_d;
            wots_q        <= wots_d;
            wots_chain_q  <= wots_chain_d;
            wots_step_q   <= wots_step_d;
            mrkl_q        <= mrkl_d;
            mrkl_level_q  <= mrkl_level_d;
            node_index_q  <= node_index_d;
            layer_q       <= layer_d;
        end
    end

endmodule
