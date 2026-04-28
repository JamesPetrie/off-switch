// Hash-based Signature Verification (HSS-LMS or SPHINCS+ FORS)
//
// Single-module implementation. One SHA-256 core shared by all phases,
// sequenced by a main FSM. SCHEME (0 = LMS, 1 = SPHINCS) selects the path
// at elaboration; only one branch survives synthesis.
//
//   LMS (RFC 8554 HSS/LMS):
//     Idle → Q0 → Wots → WotsKc → Leaf → Merkle → Done
//   SPHINCS+ FORS (this iteration; hypertree TODO):
//     Idle → Q0 → Q1 → Fors → ForsKc → Wots → … → Done   (Wots not yet SPHINCS-aware)
//
// Q phase: produces enough message-hash material for the chosen scheme.
//   - LMS uses 256 bits (fits one SHA-256 digest, captured at end of StQ0).
//   - SPHINCS+ needs K*A + tree_idx + leaf_idx ≈ 379 bits, supplied by two
//     MGF1-SHA256 blocks (StQ0 + StQ1) into a 2*WIDTH-wide aux register.
//     Block index q_hash_cntr is appended to the SPHINCS Q payload so the
//     same Q_SPHINCS_DATA macro covers both blocks.
//
// LMS deviation from the standard (preserved):
// Verification runs bottom-up: start at layer HSS_LEVELS-1 (leaf tree that
// signs the user message), and on each mrkl_complete either move up one layer
// (restart Q→...→Merkle with hash_reg_q carrying the just-computed root as
// the next layer's signed-message input) or, at layer 0, compare the result
// against ROOT_PUB_KEY. Intermediate root consistency is verified implicitly
// by each upper layer's WOTS+Merkle succeeding with that root as its Q input.
// This is the opposite direction of the standard but allows area saving.
//
// Protocol:
//   1. Assert valid and hold message, license stable until ready pulses
//   2. ready pulses high for one cycle when verification completes
//   3. When ready, check verif_passed: 1 = valid, 0 = invalid

module hash_verify
    import arith_pkg::*;
    import hss_pkg::*;
    import sphincs_pkg::*;
#(
    parameter bit SCHEME = 1'b0,    // 0 = LMS, 1 = SPHINCS

    // License width depends on the selected scheme
    localparam int unsigned LICENSE_W = SCHEME ? $bits(sphincs_lic_t)
                                               : $bits(hss_lic_t)
) (
    input  logic                  clk,
    input  logic                  rst_n,
    input  logic                  valid,
    input  logic [WIDTH-1:0]      message,
    input  logic [LICENSE_W-1:0]  license,

    output logic                  ready,
    output logic                  verif_passed
);

    // -------------------------------------------------------------------------
    // FSM state types — FORS states placed before WOTS to mirror dataflow:
    // SPHINCS goes Q* → Fors → ForsKc → Wots; LMS goes Q0 → Wots directly.
    // -------------------------------------------------------------------------

    typedef enum logic [3:0] {
        StIdle,
        StQ0, StQ1,                                // Q phase (Q1 SPHINCS-only)
        StFors, StForsKc,                          // SPHINCS FORS path
        StWots, StWotsKc, StLeaf, StMerkle,        // LMS / SPHINCS hypertree
        StDone
    } seq_state_e;

    typedef enum logic [2:0] {
        StForsInit, StForsLoad, StForsLeaf, StForsHash, StForsPkStore
    } fors_state_e;

    typedef enum logic [1:0] {
        StWotsInit, StWotsLoad, StWotsHash, StWotsPkStore
    } wots_state_e;

    typedef enum logic [0:0] {
        StMrklInit, StMrklHash
    } mrkl_state_e;

    // -------------------------------------------------------------------------
    // Registers
    // -------------------------------------------------------------------------

    seq_state_e   seq_q,   seq_d;
    fors_state_e  fors_q,  fors_d;
    wots_state_e  wots_q,  wots_d;
    mrkl_state_e  mrkl_q,  mrkl_d;

    // Hash register — working hash output across all phases
    logic [WIDTH-1:0] hash_reg_q,    hash_reg_d;

    // Auxiliary register — holds the message-hash material derived during Q.
    // Width fits two SHA-256 digests so SPHINCS+ can take both MGF1 blocks
    // without wraparound; LMS uses only the upper half.
    localparam int unsigned AUX_W = 2 * WIDTH;
    logic [AUX_W-1:0] aux_reg_q;

    // Shared block counter — indexes SHA-256 blocks within a multi-block hash
    logic [4:0]       blk_idx_q,     blk_idx_d;

    // FORS counters (driven by FORS sub-FSM)
    logic [FORS_TREE_CNT_W-1:0] fors_tree_q,  fors_tree_d;
    logic [FORS_LVL_W-1:0]      fors_level_q, fors_level_d;
    logic [FORS_NODE_W-1:0]     fors_node_q,  fors_node_d;

    // WOTS counters (driven by WOTS sub-FSM)
    logic [5:0]       wots_chain_q,  wots_chain_d;  // chain index 0-33
    logic [7:0]       wots_step_q,   wots_step_d;   // step within chain

    // Merkle tree level (driven by Merkle sub-FSM)
    logic [5:0]       mrkl_level_q,  mrkl_level_d;

    // Shared pk storage — fed by FORS in SPHINCS mode (FORS_K entries) and by
    // WOTS in LMS mode (WOTS_P entries). Sized to the larger; once SPHINCS+
    // hypertree lands, depth must pick max across both schemes' WOTS_P + FORS_K.
    localparam int unsigned PK_STORE_DEPTH = (FORS_K > WOTS_P) ? FORS_K : WOTS_P;
    logic [WIDTH-1:0] pk_store_q [PK_STORE_DEPTH];
    logic [WIDTH-1:0] pk_store_d [PK_STORE_DEPTH];

    // Merkle node index
    logic [31:0] node_index_q, node_index_d;

    // Hypertree layer counter
    logic [LAYER_CNT_W-1:0] layer_q, layer_d;

    // -------------------------------------------------------------------------
    // SHA-256 wrapper instance
    // -------------------------------------------------------------------------

    logic         sha_valid;
    logic [511:0] sha_block;
    logic         sha_last;
    wire          sha_ready;
    wire  [255:0] sha_digest;

    sha256_wrap u_sha256 (
        .clk    (clk),
        .rst_n  (rst_n),
        .valid  (sha_valid),
        .block  (sha_block),
        .last   (sha_last),
        .ready  (sha_ready),
        .digest (sha_digest)
    );

    wire hash_complete = sha_last && sha_ready;

    // -------------------------------------------------------------------------
    // Per-layer selectors — driven inside the SCHEME generate so only one
    // typed license view exists in elaboration.
    // -------------------------------------------------------------------------

    // Hypertree layer signing the message (bottom)
    wire is_msg_layer = (int'(layer_q) == HSS_LEVELS - 1);
    // Hypertree layer corresponding to the public key (top)
    wire is_pk_layer  = (layer_q == '0);

    // Module-level shared wires (driven inside generate)
    wire [127:0]     cur_I;
    wire [31:0]      cur_leaf_index;
    wire [WIDTH-1:0] cur_randomizer;
    wire [127:0]     cur_sub_I_next;     // identity of layer below (HSS Q_sub)
    wire [WIDTH-1:0] cur_sub_root;       // T[1] from layer below (HSS Q_sub)
    wire [WIDTH-1:0] cur_sig_chain;
    wire             last_chain;
    wire [WIDTH-1:0] cur_auth_node;
    wire             last_level;
    wire [WIDTH-1:0] cur_fors_sk;
    wire [WIDTH-1:0] cur_fors_auth;

    generate
        if (SCHEME == 1'b0) begin : g_lms
            hss_lic_t hss_lic;
            assign hss_lic = license;

            assign cur_I           = is_pk_layer ? TOP_IDENTIFIER
                                                 : hss_lic.sub_I[layer_q];
            assign cur_leaf_index  = hss_lic.leaf_index[layer_q];
            assign cur_randomizer  = hss_lic.randomizer[layer_q];
            assign cur_sub_I_next  = hss_lic.sub_I[layer_q + 1'b1];
            assign cur_sub_root    = hash_reg_q;   // T[1] just computed below
            assign cur_sig_chain   = hss_lic.sig_chains[layer_q][wots_chain_q];
            assign last_chain      = (int'(wots_chain_q) == WOTS_P - 1);
            assign cur_auth_node   = hss_lic.auth_path[layer_q][mrkl_level_q];
            assign last_level      = (int'(mrkl_level_q) == TREE_H - 1);

            // FORS-side wires unused in LMS; drive to constants.
            assign cur_fors_sk     = '0;
            assign cur_fors_auth   = '0;
        end else begin : g_sphincs
            sphincs_lic_t sphincs_lic;
            assign sphincs_lic = license;

            // SPHINCS hypertree (WOTS path) is not yet wired up — placeholders.
            // TODO(SPHINCS+): drive these from the SPHINCS+ hypertree license
            // fields once the hypertree iteration lands.
            assign cur_I           = TOP_IDENTIFIER;
            assign cur_leaf_index  = '0;
            assign cur_randomizer  = '0;
            assign cur_sub_I_next  = TOP_IDENTIFIER;
            assign cur_sub_root    = hash_reg_q;
            assign cur_sig_chain   = '0;
            assign last_chain      = (int'(wots_chain_q) == WOTS_P - 1);
            assign cur_auth_node   = '0;
            assign last_level      = (int'(mrkl_level_q) == TREE_H - 1);

            // FORS license drives the per-tree sk and per-(tree,level) sibling.
            assign cur_fors_sk     = sphincs_lic.sk[fors_tree_q];
            assign cur_fors_auth   = sphincs_lic.auth[fors_tree_q][fors_level_q];
        end
    endgenerate

    // -------------------------------------------------------------------------
    // q_digits (LMS WOTS) — derived from the upper half of aux_reg, which
    // captured the StQ0 digest. SPHINCS leaves the lower half holding the
    // StQ1 digest; LMS skips StQ1 and the lower half stays at reset.
    // -------------------------------------------------------------------------

    logic [7:0] q_digits[WOTS_P];

    always_comb begin
        logic [WIDTH-1:0] hash;
        logic [15:0]      csum;

        hash = aux_reg_q[AUX_W-1:WIDTH];
        csum = '0;

        // Load the digits from q_hash and calculate the checksum
        for (int i = 0; i < WOTS_P1; i++) begin

            // load the digit
            // shift hash left 8 bits, shift out to q_digits and shift in zeros
            {q_digits[i], hash} = {hash, 8'b0};

            // add the digit's contribution to the checksum
            csum += 16'(WOTS_MAX_COEF) - 16'(q_digits[i]);
        end

        // Load the checksum digits
        for (int i = WOTS_P1; i < WOTS_P; i++) begin
            // shift csum left 8 bits, shift out to q_digits and shift in zeros
            {q_digits[i], csum} = {csum, 8'b0};
        end
    end

    wire [7:0] cur_digit = q_digits[wots_chain_q];

    // -------------------------------------------------------------------------
    // fors_q_idx (SPHINCS FORS) — K leaf indices into the K FORS trees,
    // pulled from the high bits of aux_reg without wraparound. K*A = 315
    // bits sit comfortably in 2*WIDTH = 512.
    // -------------------------------------------------------------------------

    logic [FORS_A-1:0] fors_q_idx [FORS_K];

    always_comb begin
        for (int i = 0; i < FORS_K; i++) begin
            fors_q_idx[i] = aux_reg_q[(AUX_W - 1) - (i * FORS_A) -: FORS_A];
        end
    end

    // -------------------------------------------------------------------------
    // pk_store concatenations — separate vectors for WOTS (LMS) and FORS
    // (SPHINCS) since their widths differ. pk_store itself is shared.
    // -------------------------------------------------------------------------

    logic [WOTS_P*WIDTH-1:0] pk_wots_concat;
    always_comb begin
        logic [WIDTH-1:0] dis;
        pk_wots_concat = 0;
        for (int i = 0; i < WOTS_P; i++) begin
            {dis, pk_wots_concat} = {pk_wots_concat, pk_store_q[i]};
        end
    end

    logic [FORS_K*WIDTH-1:0] pk_fors_concat;
    always_comb begin
        logic [WIDTH-1:0] dis;
        pk_fors_concat = 0;
        for (int i = 0; i < FORS_K; i++) begin
            {dis, pk_fors_concat} = {pk_fors_concat, pk_store_q[i]};
        end
    end

    // -------------------------------------------------------------------------
    // SHA-256 hash inputs — continuous padded bitvectors
    //
    // Using macros to avoid repeating construction for size and value
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

`define Q_PREFIX {cur_I, cur_leaf_index, D_MESG, cur_randomizer}

`define Q_MSG_DATA {`Q_PREFIX, message}
    wire [$bits(`Q_MSG_DATA)-1 : 0] q_msg_data = `Q_MSG_DATA;
`undef Q_MSG_DATA

`define Q_SUB_DATA {`Q_PREFIX, LMS_TYPE, LMOTS_TYPE, cur_sub_I_next, cur_sub_root}
    wire [$bits(`Q_SUB_DATA)-1 : 0] q_sub_data = `Q_SUB_DATA;
`undef Q_SUB_DATA

    localparam int unsigned Q_MSG_BLOCKS    = calc_sha_blocks($bits(q_msg_data));
    localparam int unsigned Q_MSG_PAD_ZEROS = calc_sha_pad_zeros($bits(q_msg_data));
    localparam int unsigned Q_SUB_BLOCKS    = calc_sha_blocks($bits(q_sub_data));
    localparam int unsigned Q_SUB_PAD_ZEROS = calc_sha_pad_zeros($bits(q_sub_data));

    wire [Q_MSG_BLOCKS*512-1:0] q_msg_padded =
            {q_msg_data, 1'b1, {Q_MSG_PAD_ZEROS{1'b0}}, 64'($bits(q_msg_data))};
    wire [Q_SUB_BLOCKS*512-1:0] q_sub_padded =
            {q_sub_data, 1'b1, {Q_SUB_PAD_ZEROS{1'b0}}, 64'($bits(q_sub_data))};

    // -------------------------------------------------------------------------
    // SPHINCS+ Q payload — single macro covering both MGF1-SHA256 blocks of
    // H_msg. The block index `q_hash_cntr` (0 in StQ0, 1 in StQ1) is appended
    // to the payload so the same data wire feeds both hashes.
    //
    //   block_i = SHA-256(SPHINCS_R || PUB_SEED || PUB_ROOT || message || u32str(i))
    // for i = 0, 1.
    //
    // TODO(SPHINCS+): SPHINCS_R is per-signature randomness from the license
    // (not yet present in sphincs_lic_t); PUB_ROOT is the SPHINCS+ public-key
    // root (not yet in sphincs_pkg). Placeholders are used for now — the
    // structural skeleton is what this iteration is for.
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] SPHINCS_R = '0;   // TODO: per-signature randomness
    localparam logic [WIDTH-1:0] PUB_ROOT  = '0;   // TODO: SPHINCS+ public-key root

    // Block index for the SPHINCS Q payload. 0 in StQ0, 1 in StQ1.
    wire q_hash_cntr = (seq_q != StQ1) ? 1'b0 : 1'b1;

`define Q_SPHINCS_DATA {SPHINCS_R, PUB_SEED, PUB_ROOT, message, 32'(q_hash_cntr)}
    wire [$bits(`Q_SPHINCS_DATA)-1 : 0] q_sphincs_data = `Q_SPHINCS_DATA;
`undef Q_SPHINCS_DATA

    localparam int unsigned Q_SPHINCS_BLOCKS    = calc_sha_blocks  ($bits(q_sphincs_data));
    localparam int unsigned Q_SPHINCS_PAD_ZEROS = calc_sha_pad_zeros($bits(q_sphincs_data));

    wire [Q_SPHINCS_BLOCKS*512-1:0] q_sphincs_padded =
            {q_sphincs_data, 1'b1, {Q_SPHINCS_PAD_ZEROS{1'b0}}, 64'($bits(q_sphincs_data))};

    // -------------------------------------------------------------------------
    // WOTS chain: H(I || q || i || j || tmp)
    // -------------------------------------------------------------------------

`define WOTS_DATA {cur_I, cur_leaf_index, 16'(wots_chain_q), 8'(wots_step_q), hash_reg_q}
    wire [$bits(`WOTS_DATA)-1 : 0] wots_data = `WOTS_DATA;
`undef WOTS_DATA

    // WOTS is designed to fit in a single block, assume BLOCKS=1
    //localparam int unsigned WOTS_BLOCKS    = calc_sha_blocks($bits(wots_data));
    localparam int unsigned WOTS_PAD_ZEROS = calc_sha_pad_zeros($bits(wots_data));

    wire [512-1:0] wots_padded =
            {wots_data, 1'b1, {WOTS_PAD_ZEROS{1'b0}}, 64'($bits(wots_data))};

    // -------------------------------------------------------------------------
    // KC_WOTS (LMS WOTS pks aggregation): H(I || q || D_PBLC || pk0..pk33)
    // -------------------------------------------------------------------------

`define KC_WOTS_DATA {cur_I, cur_leaf_index, D_PBLC, pk_wots_concat}
    wire [$bits(`KC_WOTS_DATA)-1 : 0] kc_wots_data = `KC_WOTS_DATA;
`undef KC_WOTS_DATA

    localparam int unsigned KC_WOTS_BLOCKS    = calc_sha_blocks($bits(kc_wots_data));
    localparam int unsigned KC_WOTS_PAD_ZEROS = calc_sha_pad_zeros($bits(kc_wots_data));

    wire [KC_WOTS_BLOCKS*512-1:0] kc_wots_padded =
            {kc_wots_data, 1'b1, {KC_WOTS_PAD_ZEROS{1'b0}}, 64'($bits(kc_wots_data))};

    // -------------------------------------------------------------------------
    // Leaf: H(I || q || D_LEAF || Kc)
    // -------------------------------------------------------------------------

`define LEAF_DATA {cur_I, cur_leaf_index, D_LEAF, hash_reg_q}
    wire [$bits(`LEAF_DATA)-1 : 0] leaf_data = `LEAF_DATA;
`undef LEAF_DATA

    localparam int unsigned LEAF_BLOCKS    = calc_sha_blocks($bits(leaf_data));
    localparam int unsigned LEAF_PAD_ZEROS = calc_sha_pad_zeros($bits(leaf_data));

    wire [LEAF_BLOCKS*512-1:0] leaf_padded =
            {leaf_data, 1'b1, {LEAF_PAD_ZEROS{1'b0}}, 64'($bits(leaf_data))};

    // -------------------------------------------------------------------------
    // Merkle helpers
    // -------------------------------------------------------------------------

    // Nodes are indexed as 2n (left) and 2n+1 (right) from their parent
    wire [31:0]      parent_num = node_index_q >> 1; // node / 2
    wire             is_right   = node_index_q[0];

    // aux_reg holds the auth path sibling
    logic [WIDTH-1:0] left_node;
    logic [WIDTH-1:0] right_node;

    assign {left_node, right_node} = is_right ? {cur_auth_node, hash_reg_q}
                                              : {hash_reg_q,    cur_auth_node};

    // -------------------------------------------------------------------------
    // Merkle: H(I || parent || D_INTR || left || right)
    // -------------------------------------------------------------------------

`define MRKL_DATA {cur_I, parent_num, D_INTR, left_node, right_node}
    wire [$bits(`MRKL_DATA)-1 : 0] mrkl_data = `MRKL_DATA;
`undef MRKL_DATA

    localparam int unsigned MRKL_BLOCKS    = calc_sha_blocks($bits(mrkl_data));
    localparam int unsigned MRKL_PAD_ZEROS = calc_sha_pad_zeros($bits(mrkl_data));

    wire [MRKL_BLOCKS*512-1:0] mrkl_padded =
            {mrkl_data, 1'b1, {MRKL_PAD_ZEROS{1'b0}}, 64'($bits(mrkl_data))};

    // -------------------------------------------------------------------------
    // FORS auth-path helpers (peer to the LMS Merkle helpers above)
    // -------------------------------------------------------------------------

    wire [FORS_NODE_W-2:0] fors_parent   = fors_node_q[FORS_NODE_W-1:1];
    wire                   fors_is_right = fors_node_q[0];

    logic [WIDTH-1:0] fors_l;
    logic [WIDTH-1:0] fors_r;
    assign {fors_l, fors_r} = fors_is_right ? {cur_fors_auth, hash_reg_q}
                                            : {hash_reg_q,    cur_fors_auth};

    // -------------------------------------------------------------------------
    // FORS leaf:        H(PUB_SEED || ADRS_FORS_TREE  || tree_idx || fors_q_idx[tree] || sk[tree])
    // FORS internal:    H(PUB_SEED || ADRS_FORS_TREE  || tree_idx || parent_idx       || L || R)
    // FORS aggregation: H(PUB_SEED || ADRS_FORS_ROOTS || pk_store[0] || ... || pk_store[K-1])
    // -------------------------------------------------------------------------

`define FORS_LEAF_DATA {PUB_SEED, ADRS_FORS_TREE, 32'(fors_tree_q),    \
                        32'(fors_q_idx[fors_tree_q]), cur_fors_sk}
    wire [$bits(`FORS_LEAF_DATA)-1 : 0] fors_leaf_data = `FORS_LEAF_DATA;
`undef FORS_LEAF_DATA

`define FORS_NODE_DATA {PUB_SEED, ADRS_FORS_TREE, 32'(fors_tree_q),    \
                        32'(fors_parent), fors_l, fors_r}
    wire [$bits(`FORS_NODE_DATA)-1 : 0] fors_node_data = `FORS_NODE_DATA;
`undef FORS_NODE_DATA

`define KC_FORS_DATA {PUB_SEED, ADRS_FORS_ROOTS, pk_fors_concat}
    wire [$bits(`KC_FORS_DATA)-1 : 0] kc_fors_data = `KC_FORS_DATA;
`undef KC_FORS_DATA

    localparam int unsigned FORS_LEAF_BLOCKS  = calc_sha_blocks  ($bits(fors_leaf_data));
    localparam int unsigned FORS_LEAF_PAD_Z   = calc_sha_pad_zeros($bits(fors_leaf_data));
    localparam int unsigned FORS_NODE_BLOCKS  = calc_sha_blocks  ($bits(fors_node_data));
    localparam int unsigned FORS_NODE_PAD_Z   = calc_sha_pad_zeros($bits(fors_node_data));
    localparam int unsigned KC_FORS_BLOCKS    = calc_sha_blocks  ($bits(kc_fors_data));
    localparam int unsigned KC_FORS_PAD_Z     = calc_sha_pad_zeros($bits(kc_fors_data));

    wire [FORS_LEAF_BLOCKS*512-1:0] fors_leaf_padded =
            {fors_leaf_data, 1'b1, {FORS_LEAF_PAD_Z{1'b0}}, 64'($bits(fors_leaf_data))};
    wire [FORS_NODE_BLOCKS*512-1:0] fors_node_padded =
            {fors_node_data, 1'b1, {FORS_NODE_PAD_Z{1'b0}}, 64'($bits(fors_node_data))};
    wire [KC_FORS_BLOCKS*512-1:0]   kc_fors_padded   =
            {kc_fors_data,   1'b1, {KC_FORS_PAD_Z{1'b0}},   64'($bits(kc_fors_data))};


    // -------------------------------------------------------------------------
    // SHA block counter, last block flag and block selection
    // -------------------------------------------------------------------------

    // Helper variable
    int unsigned num_blocks;
    int unsigned blk_shift;

    logic [$bits(q_msg_padded)-1:0]            q_msg_discard;
    logic [$bits(q_sub_padded)-1:0]            q_sub_discard;
    logic [$bits(q_sphincs_padded)-1:0]        q_sphincs_discard;
    logic [$bits(kc_wots_padded)-1:0]          kc_wots_discard;
    logic [$bits(leaf_padded)-1:0]             leaf_discard;
    logic [$bits(mrkl_padded)-1:0]             mrkl_discard;
    logic [$bits(fors_leaf_padded)-1:0]        fors_leaf_discard;
    logic [$bits(fors_node_padded)-1:0]        fors_node_discard;
    logic [$bits(kc_fors_padded)-1:0]          kc_fors_discard;

    // Block counter
    always_comb begin
        blk_idx_d = blk_idx_q;

        if (sha_ready) begin
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

    // Last block flag
    assign sha_last = (int'(blk_idx_q) == num_blocks-1) ? 1'b1 : 1'b0;

    always_comb begin
        blk_shift = int'(blk_idx_q) * 512;

        num_blocks =  0;
        sha_block  = '0;

        q_msg_discard         = 0;
        q_sub_discard         = 0;
        q_sphincs_discard     = 0;
        kc_wots_discard       = 0;
        leaf_discard          = 0;
        mrkl_discard          = 0;
        fors_leaf_discard     = 0;
        fors_node_discard     = 0;
        kc_fors_discard       = 0;

        unique case (seq_q)
            StQ0: begin
                if (SCHEME == 1'b1) begin
                    num_blocks = Q_SPHINCS_BLOCKS;
                    {sha_block, q_sphincs_discard} =
                        {q_sphincs_padded, 512'b0} << blk_shift;
                end else if (is_msg_layer) begin
                    num_blocks = Q_MSG_BLOCKS;
                    {sha_block, q_msg_discard} = {q_msg_padded, 512'b0} << blk_shift;
                end else begin
                    num_blocks = Q_SUB_BLOCKS;
                    {sha_block, q_sub_discard} = {q_sub_padded, 512'b0} << blk_shift;
                end
            end
            StQ1: begin
                if (SCHEME == 1'b1) begin
                    num_blocks = Q_SPHINCS_BLOCKS;
                    {sha_block, q_sphincs_discard} =
                        {q_sphincs_padded, 512'b0} << blk_shift;
                end
            end
            StFors: begin
                if (SCHEME == 1'b1) begin
                    unique case (fors_q)
                        StForsLeaf: begin
                            num_blocks = FORS_LEAF_BLOCKS;
                            {sha_block, fors_leaf_discard} =
                                {fors_leaf_padded, 512'b0} << blk_shift;
                        end
                        StForsHash: begin
                            num_blocks = FORS_NODE_BLOCKS;
                            {sha_block, fors_node_discard} =
                                {fors_node_padded, 512'b0} << blk_shift;
                        end
                        default: ;
                    endcase
                end
            end
            StForsKc: begin
                if (SCHEME == 1'b1) begin
                    num_blocks = KC_FORS_BLOCKS;
                    {sha_block, kc_fors_discard} = {kc_fors_padded, 512'b0} << blk_shift;
                end
            end
            StWots: begin
                num_blocks = 1;
                sha_block  = wots_padded;
            end
            StWotsKc: begin
                num_blocks = KC_WOTS_BLOCKS;
                {sha_block, kc_wots_discard} = {kc_wots_padded, 512'b0} << blk_shift;
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

    wire wots_loading = (seq_q == StWots) && (wots_q == StWotsLoad);
    wire hash_reg_en  = wots_loading | hash_complete;

    assign hash_reg_d = (!wots_loading) ? sha_digest : cur_sig_chain;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            hash_reg_q <= '0;
        end else if (hash_reg_en) begin
            hash_reg_q <= hash_reg_d;
        end
    end

    // -------------------------------------------------------------------------
    // aux_reg — holds Q hash material across the rest of the run.
    // StQ0 → upper half (LMS uses this for q_digits)
    // StQ1 → lower half (SPHINCS only; together with upper, feeds fors_q_idx)
    // -------------------------------------------------------------------------

    wire q0_capture = (seq_q == StQ0) && hash_complete;
    wire q1_capture = (seq_q == StQ1) && hash_complete;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aux_reg_q <= '0;
        end else begin
            if (q0_capture) aux_reg_q[AUX_W-1:WIDTH] <= sha_digest;
            if (q1_capture) aux_reg_q[WIDTH-1:0]    <= sha_digest;
        end
    end

    // -------------------------------------------------------------------------
    // Sub-FSM output signals
    // -------------------------------------------------------------------------

    logic fors_sha_valid;
    logic fors_complete;

    logic wots_sha_valid;
    logic wots_complete;

    logic mrkl_sha_valid;
    logic mrkl_complete;

    // -------------------------------------------------------------------------
    // pk_store_d — shared between LMS WOTS and SPHINCS FORS sub-FSMs.
    // Single combinational driver: default-hold, then write the active slot
    // when the active sub-FSM is in its PkStore phase.
    // -------------------------------------------------------------------------

    always_comb begin
        for (int i = 0; i < PK_STORE_DEPTH; i++)
            pk_store_d[i] = pk_store_q[i];

        if (seq_q == StWots && wots_q == StWotsPkStore) begin
            pk_store_d[wots_chain_q] = hash_reg_q;
        end
        if (seq_q == StFors && fors_q == StForsPkStore) begin
            pk_store_d[fors_tree_q]  = hash_reg_q;
        end
    end

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
                    wots_step_d = cur_digit; // load step counter from the signed digit
                    // hash_reg captures chain signature this cycle also
                    // (outside this always_comb since hash_reg is shared)

                    // start hashing if the digit is not the maximum value,
                    // otherwise move to store right away
                    wots_d = (cur_digit != WOTS_MAX_COEF) ? StWotsHash : StWotsPkStore;
                end

                StWotsHash: begin
                    // Start the hash and wait to complete
                    wots_sha_valid = 1'b1;
                    if (sha_ready) begin
                        // increment step counter
                        wots_step_d = wots_step_q + 1;

                        // continue hashing if this was not the last hash,
                        // otherwise move to store
                        wots_d = (wots_step_q != WOTS_MAX_COEF-1) ? StWotsHash : StWotsPkStore;
                    end
                end

                StWotsPkStore: begin
                    // store the chain's public key (handled by the shared
                    // pk_store_d always_comb below)

                    // Increment chain count and move to load the next chain
                    // or clear counter and return to Init on the last chain
                    wots_chain_d  = ~last_chain ? wots_chain_q+1 : '0;
                    wots_d        = ~last_chain ? StWotsLoad     : StWotsInit;

                    // signal completion to main FSM on last chain
                    wots_complete = last_chain;
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
                    node_index_d = (32'd1 << TREE_H) | cur_leaf_index;

                    mrkl_d = StMrklHash;
                end

                StMrklHash: begin
                    // Start the hash and wait to complete
                    mrkl_sha_valid = 1'b1;
                    if (hash_complete) begin
                        // Increment level count and set node index to parent
                        // or clear counter and node index
                        mrkl_level_d = ~last_level ? mrkl_level_q+1 : '0;
                        node_index_d = ~last_level ? parent_num     : '0;
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
    // FORS sub-FSM — for each of K trees: hash leaf, climb auth path, store
    // root in pk_store. Mirrors the WOTS chain loop.
    // -------------------------------------------------------------------------

    wire fors_last_tree  = (int'(fors_tree_q)  == FORS_K - 1);
    wire fors_last_level = (int'(fors_level_q) == FORS_A - 1);

    always_comb begin
        fors_d         = fors_q;

        fors_tree_d    = fors_tree_q;
        fors_level_d   = fors_level_q;
        fors_node_d    = fors_node_q;

        fors_sha_valid = 1'b0;
        fors_complete  = 1'b0;

        // Only activate when main FSM is in StFors
        if (seq_q == StFors) begin

            unique case (fors_q)
                StForsInit: begin
                    // Q-hash capture into aux_reg happens once per outer FORS
                    // run via the dedicated capture path above; here we just
                    // reset the tree counter and step into Load.
                    fors_tree_d = '0;
                    fors_d      = StForsLoad;
                end

                StForsLoad: begin
                    // Seed level=0, node = (1<<A) | q_idx[tree] for the
                    // current tree. Once per tree.
                    fors_level_d = '0;
                    fors_node_d  = FORS_NODE_W'((1 << FORS_A) | fors_q_idx[fors_tree_q]);
                    fors_d       = StForsLeaf;
                end

                StForsLeaf: begin
                    // Hash the FORS leaf for the current tree.
                    fors_sha_valid = 1'b1;
                    if (hash_complete) begin
                        fors_d = StForsHash;
                    end
                end

                StForsHash: begin
                    // Hash internal node, climb up one level, or move on to
                    // PkStore on the last level.
                    fors_sha_valid = 1'b1;
                    if (hash_complete) begin
                        fors_d       = ~fors_last_level ? StForsHash      : StForsPkStore;
                        fors_level_d = ~fors_last_level ? fors_level_q+1  : '0;
                        fors_node_d  = ~fors_last_level ? fors_node_q>>1  : '0;
                    end
                end

                StForsPkStore: begin
                    // Store happens via the shared pk_store_d driver above.
                    // Advance to the next tree, or complete on the last.
                    fors_tree_d   = ~fors_last_tree ? fors_tree_q + 1'b1 : '0;
                    fors_d        = ~fors_last_tree ? StForsLoad         : StForsInit;
                    fors_complete = fors_last_tree;
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
        sha_valid     = 1'b0;
        ready         = 1'b0;
        verif_passed  = 1'b0;

        unique case (seq_q)

            StIdle: begin
                if (valid) begin
                    layer_d = LAYER_CNT_W'(HSS_LEVELS - 1);
                    seq_d   = StQ0;
                end
            end

            StQ0: begin
                // Hash first message-digest block; LMS finishes Q here, SPHINCS
                // continues into StQ1 for the second MGF1 block.
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = (SCHEME == 1'b1) ? StQ1 : StWots;
                end
            end

            StQ1: begin
                // SPHINCS-only: second MGF1 block.
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StFors;
                end
            end

            StFors: begin
                // The FORS step has multiple iterations, delegate hash control to FORS sub-FSM
                sha_valid = fors_sha_valid;
                if (fors_complete) begin
                    seq_d = StForsKc;
                end
            end

            StForsKc: begin
                // Aggregate FORS roots into the FORS public key, then fall
                // through into the SPHINCS hypertree slot.
                sha_valid = 1'b1;
                if (hash_complete) begin
                    // TODO(SPHINCS+): the WOTS / WotsKc / Leaf / Merkle states
                    //   still reference HSS-LMS license fields and LMS-specific
                    //   addressing. They will produce meaningless results in
                    //   SPHINCS mode until adapted in a follow-up plan that
                    //   introduces the SPHINCS+ hypertree signature inputs.
                    seq_d = StWots;
                end
            end

            StWots: begin
                sha_valid = wots_sha_valid;
                if (wots_complete) begin
                    seq_d = StWotsKc;
                end
            end

            StWotsKc: begin
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StLeaf;
                end
            end

            StLeaf: begin
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StMerkle;
                end
            end

            StMerkle: begin
                // The Merkle step has multiple iterations, delegate hash control to Merkle sub-FSM
                sha_valid = mrkl_sha_valid;
                if (mrkl_complete) begin
                    seq_d   = (~is_pk_layer) ? StQ0            : StDone;
                    layer_d = (~is_pk_layer) ? layer_q - 1'b1  : '0;
                end
            end

            StDone: begin
                ready        = 1'b1;
                verif_passed = (hash_reg_q == ROOT_PUB_KEY);
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
            seq_q         <= StIdle;
            fors_q        <= StForsInit;
            fors_tree_q   <= '0;
            fors_level_q  <= '0;
            fors_node_q   <= '0;
            wots_q        <= StWotsInit;
            wots_chain_q  <= '0;
            wots_step_q   <= '0;
            mrkl_q        <= StMrklInit;
            mrkl_level_q  <= '0;
            node_index_q  <= '0;
            layer_q       <= '0;
            for (int i = 0; i < PK_STORE_DEPTH; i++)
                pk_store_q[i] <= '0;
        end else begin
            seq_q         <= seq_d;
            fors_q        <= fors_d;
            fors_tree_q   <= fors_tree_d;
            fors_level_q  <= fors_level_d;
            fors_node_q   <= fors_node_d;
            wots_q        <= wots_d;
            wots_chain_q  <= wots_chain_d;
            wots_step_q   <= wots_step_d;
            mrkl_q        <= mrkl_d;
            mrkl_level_q  <= mrkl_level_d;
            node_index_q  <= node_index_d;
            layer_q       <= layer_d;
            for (int i = 0; i < PK_STORE_DEPTH; i++)
                pk_store_q[i] <= pk_store_d[i];
        end
    end

endmodule
