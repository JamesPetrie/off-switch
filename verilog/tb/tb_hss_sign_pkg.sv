// HSS-LMS signing functions for testbench use.
// Uses DPI-C SHA-256 to compute WOTS+ signatures at runtime across
// HSS_LEVELS stacked trees (bottom-up verify order mirrors hss_verify.sv).

`ifndef TB_HSS_SIGN_PKG_SV
`define TB_HSS_SIGN_PKG_SV

package tb_hss_sign_pkg;

    import arith_pkg::*;
    import hss_pkg::*;
    import tb_hss_tree_pkg::*;

    // DPI import — implemented in dpi_sha256.cpp
    // Buffer sized to fit the largest single-hash input we assemble here.
    import "DPI-C" function void dpi_sha256(
        input byte unsigned data[128], input int byte_len,
        output bit [255:0] digest
    );

    // -------------------------------------------------------------------------
    // Buffer accumulator for SHA-256 input assembly
    // -------------------------------------------------------------------------

    // Large enough for the biggest single-block consumer: inter-layer Q input
    // (128+32+16+256+32+32+128+256 bits = 110 bytes). Sized generously.
    byte unsigned sha_buf [128];
    int           sha_idx;

    function automatic void sha_clear();
        sha_idx = 0;
    endfunction

    function automatic void sha_pack256(input logic [255:0] val);
        for (int i = 0; i < 32; i++)
            sha_buf[sha_idx + i] = val[(255 - i*8) -: 8];
        sha_idx += 32;
    endfunction

    function automatic void sha_pack128(input logic [127:0] val);
        for (int i = 0; i < 16; i++)
            sha_buf[sha_idx + i] = val[(127 - i*8) -: 8];
        sha_idx += 16;
    endfunction

    function automatic void sha_pack32(input logic [31:0] val);
        for (int i = 0; i < 4; i++)
            sha_buf[sha_idx + i] = val[(31 - i*8) -: 8];
        sha_idx += 4;
    endfunction

    function automatic void sha_pack16(input logic [15:0] val);
        sha_buf[sha_idx]     = val[15:8];
        sha_buf[sha_idx + 1] = val[7:0];
        sha_idx += 2;
    endfunction

    function automatic void sha_pack8(input logic [7:0] val);
        sha_buf[sha_idx] = val;
        sha_idx += 1;
    endfunction

    function automatic logic [255:0] sha_finish();
        logic [255:0] digest;
        dpi_sha256(sha_buf, sha_idx, digest);
        return digest;
    endfunction

    // -------------------------------------------------------------------------
    // Derive WOTS+ secret key per RFC 8554 Appendix A:
    //   x_q[i] = H(I || u32str(q) || u16str(i) || u8str(0xff) || SEED)
    // -------------------------------------------------------------------------

    function automatic logic [255:0] wots_secret_key(
        input logic [127:0] ident,
        input logic [255:0] seed,
        input logic [31:0]  leaf_q,
        input int           chain_i
    );
        sha_clear();
        sha_pack128(ident);
        sha_pack32 (leaf_q);
        sha_pack16 (16'(chain_i));
        sha_pack8  (8'hff);
        sha_pack256(seed);
        return sha_finish();
    endfunction

    // -------------------------------------------------------------------------
    // WOTS+ chain hash: H(I || u32str(q) || u16str(i) || u8str(j) || tmp)
    // -------------------------------------------------------------------------

    function automatic logic [255:0] wots_chain_hash(
        input logic [127:0] ident,
        input logic [31:0]  leaf_q,
        input int           chain_i,
        input int           step_j,
        input logic [255:0] tmp
    );
        sha_clear();
        sha_pack128(ident);
        sha_pack32 (leaf_q);
        sha_pack16 (16'(chain_i));
        sha_pack8  (8'(step_j));
        sha_pack256(tmp);
        return sha_finish();
    endfunction

    // -------------------------------------------------------------------------
    // Q hash for a 256-bit message input:
    //   H(I || u32str(q) || D_MESG || C || message)
    // -------------------------------------------------------------------------

    function automatic logic [255:0] compute_q_hash_msg(
        input logic [127:0] ident,
        input logic [31:0]  leaf_q,
        input logic [255:0] randomizer,
        input logic [255:0] message
    );
        sha_clear();
        sha_pack128(ident);
        sha_pack32 (leaf_q);
        sha_pack16 (D_MESG);
        sha_pack256(randomizer);
        sha_pack256(message);
        return sha_finish();
    endfunction

    // -------------------------------------------------------------------------
    // Q hash for a serialised-pub inter-layer input:
    //   H(I || u32str(q) || D_MESG || C || LMS_TYPE || LMOTS_TYPE || sub_I || sub_root)
    // -------------------------------------------------------------------------

    function automatic logic [255:0] compute_q_hash_sub(
        input logic [127:0] ident,
        input logic [31:0]  leaf_q,
        input logic [255:0] randomizer,
        input logic [127:0] sub_ident,
        input logic [255:0] sub_root
    );
        sha_clear();
        sha_pack128(ident);
        sha_pack32 (leaf_q);
        sha_pack16 (D_MESG);
        sha_pack256(randomizer);
        sha_pack32 (LMS_TYPE);
        sha_pack32 (LMOTS_TYPE);
        sha_pack128(sub_ident);
        sha_pack256(sub_root);
        return sha_finish();
    endfunction

    // -------------------------------------------------------------------------
    // Extract auth path from pre-built tree (per layer)
    // -------------------------------------------------------------------------

    function automatic void get_auth_path(
        input  int lv,
        input  int leaf_q,
        output logic [TREE_H_MAX-1:0][WIDTH-1:0] path
    );
        int node_idx;
        node_idx = NUM_LEAVES + leaf_q;
        for (int level = 0; level < TREE_H; level++) begin
            path[level] = TREE[lv][node_idx ^ 1];
            node_idx = node_idx >> 1;
        end
        for (int level = TREE_H; level < TREE_H_MAX; level++)
            path[level] = '0;
    endfunction

    // -------------------------------------------------------------------------
    // Per-layer leaf state — only the bottom layer advances on each sign call;
    // upper-layer leaves stay sticky (set from SUB_LEAF_INDEX) throughout the
    // simulation.
    // -------------------------------------------------------------------------

    int cur_leaf [HSS_LEVELS];

    function automatic void init_leaves();
        // Upper layers (if any): use pre-chosen sub leaves, stable across
        // signings. For L=1 the loop is empty.
        for (int lv = 0; lv < int'(HSS_LEVELS) - 1; lv++)
            cur_leaf[lv] = SUB_LEAF_INDEX[lv];
        // Bottom layer: starts at 5, advances on each hss_sign
        cur_leaf[HSS_LEVELS - 1] = 5;
    endfunction

    function automatic logic [255:0] make_randomizer(input int leaf);
        return {32{leaf[7:0]}};
    endfunction

    function automatic void advance_leaf();
        cur_leaf[HSS_LEVELS - 1] = cur_leaf[HSS_LEVELS - 1] + 1;
        if (cur_leaf[HSS_LEVELS - 1] >= NUM_LEAVES)
            $fatal("Exhausted all %0d leaves in the bottom tree", NUM_LEAVES);
    endfunction

    // -------------------------------------------------------------------------
    // Sign a single LMS layer (one tree's worth of WOTS+ + auth path) given a
    // pre-computed Q digest. Writes into the license struct at index `lv`.
    // -------------------------------------------------------------------------

    function automatic void sign_layer_with_q(
        input int                                    lv,
        input logic [255:0]                          q_hash,
        ref   license_t                              lic
    );
        logic [15:0]  csum;
        int digits [WOTS_P];
        logic [127:0] ident;
        logic [255:0] seed;
        int q;

        ident = IDENTIFIER[lv];
        seed  = MASTER_SEED[lv];
        q     = cur_leaf[lv];

        // Extract digits
        for (int i = 0; i < WOTS_P1; i++)
            digits[i] = int'(q_hash[(255 - i*8) -: 8]);

        // Checksum
        csum = '0;
        for (int i = 0; i < WOTS_P1; i++)
            csum = csum + 16'(WOTS_MAX_COEF) - 16'(digits[i]);
        digits[WOTS_P1]     = int'(csum[15:8]);
        digits[WOTS_P1 + 1] = int'(csum[7:0]);

        // Sign each chain: hash secret key forward digit[i] steps
        for (int i = 0; i < WOTS_P; i++) begin
            logic [255:0] val;
            val = wots_secret_key(ident, seed, q, i);
            for (int j = 0; j < digits[i]; j++)
                val = wots_chain_hash(ident, q, i, j, val);
            lic.sig_chains[lv][i] = val;
        end

        lic.leaf_index[lv] = q;
        lic.randomizer[lv] = make_randomizer(q);

        // Auth path
        get_auth_path(lv, q, lic.auth_path[lv]);
    endfunction

    // -------------------------------------------------------------------------
    // HSS sign — populate a license_t across all HSS_LEVELS layers.
    //
    // Layer HSS_LEVELS-1 (bottom) signs the user message.
    // Layer lv < L-1 signs pub[lv+1] = serialised LMS public key of layer lv+1.
    // Upper-layer signatures are recomputed each call (cheap for small h),
    // but only the bottom-layer leaf advances afterwards.
    // -------------------------------------------------------------------------

    function automatic license_t hss_sign(
        input logic [255:0] message
    );
        license_t lic;
        logic [255:0] q_hash;
        int q;

        lic = 0;

        // Subtree identifiers go into the license so the verifier can
        // reconstruct pub[lv] during upper-layer Q hashing. sub_I[0] is unused
        // (layer 0 is the top tree and uses TOP_IDENTIFIER).
        for (int lv = 1; lv < HSS_LEVELS; lv++)
            lic.sub_I[lv] = IDENTIFIER[lv];

        // Sign each layer lv from top (0) to bottom (L-1). Payload at lv:
        //   - lv == L-1 : user message
        //   - lv <  L-1 : pub[lv+1] = {LMS_TYPE, LMOTS_TYPE, I_{lv+1}, root_{lv+1}}
        for (int lv = 0; lv < HSS_LEVELS; lv++) begin
            q = cur_leaf[lv];
            if (lv == HSS_LEVELS - 1) begin
                q_hash = compute_q_hash_msg(IDENTIFIER[lv], q,
                                            make_randomizer(q), message);
            end else begin
                q_hash = compute_q_hash_sub(IDENTIFIER[lv], q,
                                            make_randomizer(q),
                                            IDENTIFIER[lv + 1],
                                            TREE[lv + 1][1]);  // T_{lv+1}[1] = root
            end
            sign_layer_with_q(lv, q_hash, lic);
        end

        // Advance only the bottom-layer leaf for the next call.
        advance_leaf();

        return lic;
    endfunction

endpackage

`endif
