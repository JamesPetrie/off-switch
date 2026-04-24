// HSS-LMS Signature Verification
//
// Single-module implementation of RFC 8554 HSS/LMS verification.
// One SHA-256 core shared by all phases, sequenced by a main FSM:
//
//   Sequencer  — phases: Idle → Q → Wots → Kc → Leaf → Merkle → Done
//   Q          — hash for message digest Q
//   WOTS       — hash WOTS chains forward to their public keys (sub-FSM)
//   Kc         — hash all 34 pks (counter-based)
//   Leaf       — hash for leaf node
//   Merkle     — walk auth path from leaf to root (sub-FSM)
//
// Note: Deviation from the standard!
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

module hss_verify
    import arith_pkg::*;
    import hss_pkg::*;
(
    input  logic             clk,
    input  logic             rst_n,
    input  logic             valid,
    input  logic [WIDTH-1:0] message,
    input  license_t         license,

    output logic             ready,
    output logic             verif_passed
);

    // -------------------------------------------------------------------------
    // FSM state types
    // -------------------------------------------------------------------------

    typedef enum logic [3:0] {
        StIdle, StQ, StWots, StKc, StLeaf, StMerkle, StDone
    } seq_state_e;

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
    wots_state_e  wots_q,  wots_d;
    mrkl_state_e  mrkl_q,  mrkl_d;

    // Hash register — working hash output across all phases
    logic [WIDTH-1:0] hash_reg_q,    hash_reg_d;

    // Auxiliary register — companion value alongside hash_reg
    // WOTS: holds Q hash
    logic [WIDTH-1:0] aux_reg_q,     aux_reg_d;

    // Shared block counter — indexes SHA-256 blocks within a multi-block hash
    // REVISIT hardcoded widhts
    logic [4:0]       blk_idx_q,     blk_idx_d;

    // WOTS counters (driven by WOTS sub-FSM)
    logic [5:0]       wots_chain_q,  wots_chain_d;  // chain index 0-33
    logic [7:0]       wots_step_q,   wots_step_d;   // step within chain

    // Merkle tree level (driven by Merkle sub-FSM)
    logic [5:0]       mrkl_level_q,  mrkl_level_d;

    // pk storage (34 x 256 bits) — filled by WOTS, read by Kc
    logic [WIDTH-1:0] pk_store_q [WOTS_P];
    logic [WIDTH-1:0] pk_store_d [WOTS_P];

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
    // Per-layer selectors
    // -------------------------------------------------------------------------

    // Hypertree layer signing the message (bottom)
    wire is_msg_layer = (int'(layer_q) == HSS_LEVELS - 1);
    // Hypetree layer corresponing to the Public Key (top)
    wire is_pk_layer  = (layer_q == '0);

    // Top-tree identifier is the package constant; lower trees carry theirs
    // in the license as sub_I[lv] (≥1). sub_I[0] is unused for the top layer.
    wire [127:0] cur_I = is_pk_layer  ? TOP_IDENTIFIER
                                      : license.sub_I[layer_q];

    // -------------------------------------------------------------------------
    // Data indexed by WOTS chain / Merkle level
    // -------------------------------------------------------------------------

    wire [WIDTH-1:0] cur_sig_chain = license.sig_chains[layer_q][wots_chain_q];
    wire             last_chain    = (int'(wots_chain_q) == WOTS_P-1) ? 1'b1 : 1'b0;

    wire [WIDTH-1:0] cur_auth_node = license.auth_path[layer_q][mrkl_level_q];
    wire             last_level    = (int'(mrkl_level_q) == TREE_H-1) ? 1'b1 : 1'b0;

    // -------------------------------------------------------------------------
    // Q hash split into digits + checksum — computed combinationally
    // -------------------------------------------------------------------------

    logic [7:0] q_digits[WOTS_P];

    // Using byte-wise shift left to avoid indexing issues
    always_comb begin
        logic [WIDTH-1:0] hash;     // hash working variable
        logic [15:0]      csum;     // checksum working variable

        hash  = aux_reg_q;
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
    // digit wise pk hashes combined into a single bitvector for hashing
    // -------------------------------------------------------------------------

    logic [WOTS_P*WIDTH-1:0] pk_concat;
    always_comb begin
        logic [WIDTH-1:0] pk_discard;
        pk_concat = 0; // '0 triggers verilator WIDTHCONCAT on wide vectors
        for (int i = 0; i < WOTS_P; i++) begin
            // shift pk_concat left WIDTH bits, shift in from pk_store_q[i], discard shift out
            {pk_discard, pk_concat} = {pk_concat, pk_store_q[i]};
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

`define Q_PREFIX {cur_I, license.leaf_index[layer_q], D_MESG, license.randomizer[layer_q]}

`define Q_MSG_DATA {`Q_PREFIX, message}
    wire [$bits(`Q_MSG_DATA)-1 : 0] q_msg_data = `Q_MSG_DATA;
`undef Q_MSG_DATA

    // sub_I is indexed at layer_q+1 (identity of the tree below)
`define Q_SUB_DATA {`Q_PREFIX, LMS_TYPE, LMOTS_TYPE, license.sub_I[layer_q + 1'b1], hash_reg_q}
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
    // WOTS chain: H(I || q || i || j || tmp)
    // -------------------------------------------------------------------------

`define WOTS_DATA {cur_I, license.leaf_index[layer_q], 16'(wots_chain_q), \
                   8'(wots_step_q), hash_reg_q}
    wire [$bits(`WOTS_DATA)-1 : 0] wots_data = `WOTS_DATA;
`undef WOTS_DATA

    // WOTS is designed to fit in a single block, assume BLOCKS=1
    //localparam int unsigned WOTS_BLOCKS    = calc_sha_blocks($bits(wots_data));
    localparam int unsigned WOTS_PAD_ZEROS = calc_sha_pad_zeros($bits(wots_data));

    wire [512-1:0] wots_padded =
            {wots_data, 1'b1, {WOTS_PAD_ZEROS{1'b0}}, 64'($bits(wots_data))};

    // -------------------------------------------------------------------------
    // Kc: H(I || q || D_PBLC || pk0..pk33)
    // -------------------------------------------------------------------------

`define KC_DATA {cur_I, license.leaf_index[layer_q], D_PBLC, pk_concat}
    wire [$bits(`KC_DATA)-1 : 0] kc_data = `KC_DATA;
`undef KC_DATA

    localparam int unsigned KC_BLOCKS    = calc_sha_blocks($bits(kc_data));
    localparam int unsigned KC_PAD_ZEROS = calc_sha_pad_zeros($bits(kc_data));

    wire [KC_BLOCKS*512-1:0] kc_padded =
            {kc_data, 1'b1, {KC_PAD_ZEROS{1'b0}}, 64'($bits(kc_data))};

    // -------------------------------------------------------------------------
    // Leaf: H(I || q || D_LEAF || Kc)
    // -------------------------------------------------------------------------

`define LEAF_DATA {cur_I, license.leaf_index[layer_q], D_LEAF, hash_reg_q}
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
    // SHA block counter, last block flag and block selection
    // -------------------------------------------------------------------------

    // Helper variable
    int unsigned num_blocks;
    int unsigned blk_shift;

    // Unused bits from shift output
    logic [$bits(q_msg_padded)-1:0] q_msg_discard;
    logic [$bits(q_sub_padded)-1:0] q_sub_discard;
    logic [$bits(kc_padded)-1:0]    kc_discard;
    logic [$bits(leaf_padded)-1:0]  leaf_discard;
    logic [$bits(mrkl_padded)-1:0]  mrkl_discard;

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

    // Input vector and block selection
    always_comb begin
        blk_shift = int'(blk_idx_q) * 512;

        num_blocks =  0;
        sha_block  = '0;

        // Note: '0 causes too large concatentation lint warning on kc, using 0 instead
        q_msg_discard = 0;
        q_sub_discard = 0;
        kc_discard    = 0;
        leaf_discard  = 0;
        mrkl_discard  = 0;

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
                num_blocks = 1;
                sha_block = wots_padded;
            end
            StKc: begin
                num_blocks = KC_BLOCKS;
                {sha_block, kc_discard}   = {kc_padded,   512'b0} << blk_shift;
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
    // aux_reg — stores Q hash throughout WOTS, and auth siblings during Merkle
    // -------------------------------------------------------------------------

    wire wots_init  = (seq_q == StWots) && (wots_q == StWotsInit);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aux_reg_q <= '0;
        end else if (wots_init) begin
            aux_reg_q <= hash_reg_q;
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

        for (int i = 0; i < WOTS_P; i++)
            pk_store_d[i] = pk_store_q[i];

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
                    // store the chain's public key
                    pk_store_d[wots_chain_q] = hash_reg_q;

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
                    node_index_d = (32'd1 << TREE_H) | license.leaf_index[layer_q];

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
                    // Start at the bottom layer (signs the user message)
                    layer_d = LAYER_CNT_W'(HSS_LEVELS - 1);
                    seq_d   = StQ;
                end
            end

            // The states below are responsible to start the hashing and process the completion
            // The rest (feeding the appropriate inputs to the SHA block) is taken care outisde this
            // always_comb block based on the FSM state and sub-FSM states

            StQ: begin
                // Start Q hash and wait to complete
                sha_valid = 1'b1;
                if (hash_complete) begin
                    seq_d = StWots;
                end
            end

            StWots: begin
                // The WOTS step has multiple iterations, delegate hash control to WOTS sub-FSM
                sha_valid = wots_sha_valid;
                if (wots_complete) begin
                    seq_d = StKc;
                end
            end

            StKc: begin
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
            wots_q        <= StWotsInit;
            wots_chain_q  <= '0;
            wots_step_q   <= '0;
            mrkl_q        <= StMrklInit;
            mrkl_level_q  <= '0;
            node_index_q  <= '0;
            layer_q       <= '0;
            for (int i = 0; i < WOTS_P; i++)
                pk_store_q[i] <= '0;
        end else begin
            seq_q         <= seq_d;
            wots_q        <= wots_d;
            wots_chain_q  <= wots_chain_d;
            wots_step_q   <= wots_step_d;
            mrkl_q        <= mrkl_d;
            mrkl_level_q  <= mrkl_level_d;
            node_index_q  <= node_index_d;
            layer_q       <= layer_d;
            for (int i = 0; i < WOTS_P; i++)
                pk_store_q[i] <= pk_store_d[i];
        end
    end

endmodule
