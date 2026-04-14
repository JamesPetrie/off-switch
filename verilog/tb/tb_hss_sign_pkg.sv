// HSS-LMS signing functions for testbench use.
// Uses DPI-C SHA-256 to compute WOTS+ signatures at runtime.

`ifndef TB_HSS_SIGN_PKG_SV
`define TB_HSS_SIGN_PKG_SV

package tb_hss_sign_pkg;

    import arith_pkg::*;
    import hss_pkg::*;
    import tb_hss_tree_pkg::*;

    // DPI import — implemented in dpi_sha256.cpp
    import "DPI-C" function void dpi_sha256(
        input byte unsigned data[], input int byte_len,
        output bit [255:0] digest
    );

    // -------------------------------------------------------------------------
    // Buffer accumulator for SHA-256 input assembly
    // -------------------------------------------------------------------------

    byte unsigned sha_buf [96];
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
        input logic [31:0] leaf_q,
        input int          chain_i
    );
        sha_clear();
        sha_pack128(IDENTIFIER);
        sha_pack32 (leaf_q);
        sha_pack16 (16'(chain_i));
        sha_pack8  (8'hff);
        sha_pack256(MASTER_SEED);
        return sha_finish();
    endfunction

    // -------------------------------------------------------------------------
    // WOTS+ chain hash: H(I || u32str(q) || u16str(i) || u8str(j) || tmp)
    // -------------------------------------------------------------------------

    function automatic logic [255:0] wots_chain_hash(
        input logic [31:0]  leaf_q,
        input int           chain_i,
        input int           step_j,
        input logic [255:0] tmp
    );
        sha_clear();
        sha_pack128(IDENTIFIER);
        sha_pack32 (leaf_q);
        sha_pack16 (16'(chain_i));
        sha_pack8  (8'(step_j));
        sha_pack256(tmp);
        return sha_finish();
    endfunction

    // -------------------------------------------------------------------------
    // Q hash: H(I || u32str(q) || D_MESG || C || message)
    // -------------------------------------------------------------------------

    function automatic logic [255:0] compute_q_hash(
        input logic [31:0]  leaf_q,
        input logic [255:0] randomizer,
        input logic [255:0] message
    );
        sha_clear();
        sha_pack128(IDENTIFIER);
        sha_pack32 (leaf_q);
        sha_pack16 (D_MESG);
        sha_pack256(randomizer);
        sha_pack256(message);
        return sha_finish();
    endfunction

    // -------------------------------------------------------------------------
    // Extract auth path from pre-built tree
    // -------------------------------------------------------------------------

    function automatic void get_auth_path(
        input  int leaf_q,
        output logic [MAX_HEIGHT-1:0][WIDTH-1:0] path
    );
        int node_idx;
        node_idx = NUM_LEAVES + leaf_q;
        for (int lv = 0; lv < TREE_HEIGHT; lv++) begin
            path[lv] = TREE[node_idx ^ 1];
            node_idx = node_idx >> 1;
        end
        for (int lv = TREE_HEIGHT; lv < MAX_HEIGHT; lv++)
            path[lv] = '0;
    endfunction

    // -------------------------------------------------------------------------
    // Leaf and randomizer state — auto-advances after each signing
    // -------------------------------------------------------------------------

    int cur_leaf = 0;

    function automatic logic [255:0] make_randomizer(input int leaf);
        return {32{leaf[7:0]}};
    endfunction

    function automatic void advance_leaf();
        cur_leaf = cur_leaf + 1;
        if (cur_leaf >= NUM_LEAVES)
            $fatal("Exhausted all %0d leaves in the Merkle tree", NUM_LEAVES);
    endfunction

    // -------------------------------------------------------------------------
    // Sign a message → license_t
    // Uses cur_leaf and auto-derived randomizer, then advances the leaf.
    // -------------------------------------------------------------------------

    function automatic license_t hss_sign(
        input logic [255:0] message
    );
        license_t lic;
        logic [255:0] q_hash;
        logic [15:0]  csum;
        int digits [WOTS_P];

        lic.leaf_index = cur_leaf;
        lic.randomizer = make_randomizer(lic.leaf_index);

        // Q hash
        q_hash = compute_q_hash(lic.leaf_index, lic.randomizer, message);

        // Extract digits
        for (int i = 0; i < WOTS_P1; i++)
            digits[i] = int'(q_hash[(255 - i*8) -: 8]);

        // Checksum
        csum = '0;
        for (int i = 0; i < WOTS_P1; i++)
            csum = csum + 16'(MAX_COEF) - 16'(digits[i]);
        digits[WOTS_P1]     = int'(csum[15:8]);
        digits[WOTS_P1 + 1] = int'(csum[7:0]);

        // Sign each chain: hash secret key forward digit[i] steps
        for (int i = 0; i < WOTS_P; i++) begin
            logic [255:0] val;
            val = wots_secret_key(lic.leaf_index, i);
            for (int j = 0; j < digits[i]; j++)
                val = wots_chain_hash(lic.leaf_index, i, j, val);
            lic.sig_chains[i] = val;
        end

        // Auth path
        get_auth_path(lic.leaf_index, lic.auth_path);

        // Advance to next leaf
        advance_leaf();

        return lic;
    endfunction

endpackage

`endif
