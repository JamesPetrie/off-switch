// HSS-LMS signing for testbench use.
//
// Signatures come from the Cisco hash-sigs reference implementation of
// RFC 8554 (tb/hash-sigs, git submodule) through DPI-C (tb/dpi_hss.c,
// tb/hss_signer.c). Each test signer is a two-level hypertree with keys
// derived from a fixed seed; the public keys are in rtl/hss_keys.svh
// (`make hss-keys`) and are checked against the signer on first use.
//
// hss_sign() returns the standard HSS signature re-laid as the verifier's
// licence beat stream; the bytes themselves are not reinterpreted.

`ifndef TB_HSS_SIGN_PKG_SV
`define TB_HSS_SIGN_PKG_SV

package tb_hss_sign_pkg;

    import arith_pkg::*;
    import hss_pkg::*;

    // -------------------------------------------------------------------------
    // RFC 8554 encodings, in bytes
    // -------------------------------------------------------------------------

    localparam int unsigned LMS_PUB_LEN = 4 + 4 + 16 + 32;              // type || otstype || I || T
    localparam int unsigned OTS_SIG_LEN = 4 + 32 + 32 * WOTS_P;              // otstype || C || y[p]
    localparam int unsigned LMS_SIG_LEN = 4 + OTS_SIG_LEN + 4 + 32 * TREE_H; // q || ots sig || type || path
    localparam int unsigned HSS_SIG_LEN = 4 + (HSS_LEVELS - 1) * (LMS_SIG_LEN + LMS_PUB_LEN)
                                            + LMS_SIG_LEN;

    import "DPI-C" function int dpi_hss_init(input int signer);
    import "DPI-C" function int dpi_hss_pubkey(input  int           signer,
                                               output int           levels,
                                               output int           lms_type,
                                               output int           ots_type,
                                               output bit [127:0]   identifier,
                                               output bit [255:0]   root);
    import "DPI-C" function int dpi_hss_sign(input  int           signer,
                                             input  bit [255:0]   message,
                                             output byte unsigned sig [HSS_SIG_LEN]);

    // -------------------------------------------------------------------------
    // License format
    //
    // Kept in the shape of the standard signature format. The RTL no longer
    // takes it as a packed word: license_beat() below serialises it onto the
    // verifier's beat stream.
    // -------------------------------------------------------------------------

    typedef struct packed {
        // Per-layer OTS + Merkle material. lv=0 is the top tree; lv=HSS_LEVELS-1
        // is the leaf tree that signs the user message.
        //    Dimension 3     Dimension 2     Dimension 1
        logic [HSS_LEVELS-1:0]                [31:0]       leaf_index;
        logic [HSS_LEVELS-1:0]                [WIDTH-1:0]  randomizer;
        logic [HSS_LEVELS-1:0]                [127:0]      sub_I;
        logic [HSS_LEVELS-1:0][WOTS_P-1:0]    [WIDTH-1:0]  sig_chains;
        logic [HSS_LEVELS-1:0][TREE_H-1:0]    [WIDTH-1:0]  auth_path;
    } license_t;

    // -------------------------------------------------------------------------
    // Beat stream layout (must match hss_verify / hss_pkg):
    //   per layer, from HSS_LEVELS-1 down to 0:
    //     beat 0        {leaf_index[32], sub_I[128], padding}
    //     beat 1        randomizer
    //     beats 2..     WOTS_P chain signatures
    //     last TREE_H   auth path siblings
    // -------------------------------------------------------------------------

    localparam int unsigned BEATS_PER_LAYER = LAYER_HDR_BEATS + WOTS_P + TREE_H;
    localparam int unsigned TOTAL_BEATS     = HSS_LEVELS * BEATS_PER_LAYER;

    function automatic logic [WIDTH-1:0] license_beat(input license_t lic,
                                                     input int       idx);
        int lv, off;
        lv  = int'(HSS_LEVELS) - 1 - (idx / int'(BEATS_PER_LAYER));
        off = idx % int'(BEATS_PER_LAYER);
        if (lv < 0 || lv >= int'(HSS_LEVELS)) return '0;
        case (off)
            0:       return {lic.leaf_index[lv], lic.sub_I[lv],
                             {(WIDTH - 32 - 128){1'b0}}};
            1:       return lic.randomizer[lv];
            default: begin
                off = off - int'(LAYER_HDR_BEATS);
                return (off < int'(WOTS_P)) ? lic.sig_chains[lv][off]
                                            : lic.auth_path[lv][off - int'(WOTS_P)];
            end
        endcase
    endfunction

    // -------------------------------------------------------------------------
    // Signature bytes -> license_t
    // -------------------------------------------------------------------------

    byte unsigned sig_buf [HSS_SIG_LEN];

    function automatic logic [31:0] be32(input int off);
        return {sig_buf[off], sig_buf[off+1], sig_buf[off+2], sig_buf[off+3]};
    endfunction

    function automatic logic [127:0] be128(input int off);
        logic [127:0] v;
        for (int i = 0; i < 16; i++) v[127 - 8*i -: 8] = sig_buf[off + i];
        return v;
    endfunction

    function automatic logic [255:0] be256(input int off);
        logic [255:0] v;
        for (int i = 0; i < 32; i++) v[255 - 8*i -: 8] = sig_buf[off + i];
        return v;
    endfunction

    // One LMS signature at sig_buf[off]: u32 q || ots sig || u32 type || path.
    // Fills layer lv of lic; returns the offset past it.
    function automatic int unpack_lms_sig(input int off, input int lv, ref license_t lic);
        lic.leaf_index[lv] = be32(off);                              off += 4;
        if (be32(off) != 32'(LMOTS_TYPE))
            $fatal(1, "hss_sign: layer %0d OTS type 0x%08x, expected 0x%08x", lv, be32(off), 32'(LMOTS_TYPE));
        off += 4;
        lic.randomizer[lv] = be256(off);                             off += 32;
        for (int i = 0; i < int'(WOTS_P); i++) begin
            lic.sig_chains[lv][i] = be256(off);                      off += 32;
        end
        if (be32(off) != 32'(LMS_TYPE))
            $fatal(1, "hss_sign: layer %0d LMS type 0x%08x, expected 0x%08x", lv, be32(off), 32'(LMS_TYPE));
        off += 4;
        for (int j = 0; j < int'(TREE_H); j++) begin
            lic.auth_path[lv][j] = be256(off);                       off += 32;
        end
        return off;
    endfunction

    // The signer's public keys must be the ones hss_pkg was elaborated with.
    function automatic void check_keys();
        int         levels, lms_type, ots_type;
        bit [127:0] identifier;
        bit [255:0] root;
        for (int s = 0; s < $size(PUBKEYS); s++) begin
            if (dpi_hss_init(s) != 0 ||
                dpi_hss_pubkey(s, levels, lms_type, ots_type, identifier, root) != 0)
                $fatal(1, "hss_sign: hash-sigs could not set up signer %0d", s);
            if (levels != int'(HSS_LEVELS) || lms_type != int'(LMS_TYPE) || ots_type != int'(LMOTS_TYPE))
                $fatal(1, "hss_sign: signer %0d parameter set (L=%0d, lms=%0d, ots=%0d) is not hss_pkg's",
                       s, levels, lms_type, ots_type);
            if (identifier != PUBKEYS[s].identifier || root != PUBKEYS[s].root_pub_key)
                $fatal(1, "hss_sign: signer %0d public key is not the one in rtl/hss_keys.svh; run `make hss-keys`", s);
        end
    endfunction

    // -------------------------------------------------------------------------
    // HSS sign: the next bottom-tree leaf of signer_id signs the 256-bit
    // message; upper layers come along inside the signature as RFC 8554
    // lays them out. The signer advances its leaf on every call.
    // -------------------------------------------------------------------------

    function automatic license_t hss_sign(
        input logic [255:0] message,
        input int           signer_id
    );
        static bit keys_checked = 1'b0;
        license_t lic;
        int off;

        if (!keys_checked) begin
            check_keys();
            keys_checked = 1'b1;
        end

        if (dpi_hss_sign(signer_id, message, sig_buf) != 0)
            $fatal(1, "hss_sign: hash-sigs could not sign for signer %0d", signer_id);

        lic = 0;
        // u32 Nspk, the number of signed public keys: all layers but the bottom
        if (be32(0) != 32'(HSS_LEVELS - 1))
            $fatal(1, "hss_sign: Nspk %0d, expected %0d", be32(0), HSS_LEVELS - 1);
        off = 4;
        for (int lv = 0; lv < int'(HSS_LEVELS); lv++) begin
            off = unpack_lms_sig(off, lv, lic);
            if (lv < int'(HSS_LEVELS) - 1) begin
                // pub[lv+1] = u32 type || u32 otstype || I || T: the verifier
                // rebuilds it from sub_I and its own Merkle root, so only I
                // rides in the licence. sub_I[0] stays 0: the top tree's
                // identity is the fused public key's.
                if (be32(off) != 32'(LMS_TYPE) || be32(off + 4) != 32'(LMOTS_TYPE))
                    $fatal(1, "hss_sign: pub[%0d] types 0x%08x/0x%08x are not hss_pkg's",
                           lv + 1, be32(off), be32(off + 4));
                lic.sub_I[lv + 1] = be128(off + 8);
                off += int'(LMS_PUB_LEN);
            end
        end
        if (off != int'(HSS_SIG_LEN))
            $fatal(1, "hss_sign: consumed %0d of %0d signature bytes", off, HSS_SIG_LEN);
        return lic;
    endfunction

endpackage

`endif
