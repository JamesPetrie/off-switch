// SLH-DSA signing for testbench use.
//
// Signatures come from slhdsa-c, the PQ Code Package implementation of
// FIPS 205 (tb/slhdsa-c, git submodule), through DPI-C (tb/dpi_slh.c,
// tb/slh_signer.c). Each test signer is an SLH-DSA-SHA2-128s key pair derived
// from a fixed seed; the public keys are in rtl/slh_keys.svh
// (`make slh-keys`) and are checked against the signer on first use.
//
// slh_sign() returns the standard signature cut into the verifier's licence
// beats, one n-byte element per beat; the bytes themselves are not
// reinterpreted.

`ifndef TB_SLH_SIGN_PKG_SV
`define TB_SLH_SIGN_PKG_SV

package tb_slh_sign_pkg;

    import arith_pkg::*;
    import slh_pkg::*;

    // The FIPS 205 signature, in bytes
    localparam int unsigned SLH_SIG_LEN = SLH_SIG_ELEMS * SLH_N;

    import "DPI-C" function int dpi_slh_init(input int signer);
    import "DPI-C" function int dpi_slh_sig_len();
    import "DPI-C" function int dpi_slh_pubkey(input  int              signer,
                                               output bit [SLH_NW-1:0] seed,
                                               output bit [SLH_NW-1:0] root);
    import "DPI-C" function int dpi_slh_sign(input  int           signer,
                                             input  bit [255:0]   message,
                                             output byte unsigned sig [SLH_SIG_LEN]);

    // -------------------------------------------------------------------------
    // License format: the signature as its n-byte elements, element 0 the
    // first one of the signature
    // -------------------------------------------------------------------------

    typedef logic [SLH_SIG_ELEMS-1:0][SLH_BEAT_W-1:0] license_t;

    // -------------------------------------------------------------------------
    // Beat stream layout (FIPS 205 Fig 17; must match hss_verify / slh_pkg):
    //   beat 0            the randomizer R
    //   k x (1 + a)       per FORS tree, a secret value and its auth path
    //   d x (len + h')    per layer, from the bottom of the hypertree, the
    //                     chain signatures and the auth path
    // -------------------------------------------------------------------------

    localparam int unsigned TOTAL_BEATS     = SLH_SIG_ELEMS;
    localparam int unsigned FORS_TREE_BEATS = 1 + SLH_A;
    localparam int unsigned LAYER_BEATS     = SLH_LEN + SLH_HP;

    localparam int unsigned R_BEAT    = 0;
    localparam int unsigned FORS_BEAT = 1;                                     // first FORS tree
    localparam int unsigned HT_BEAT   = FORS_BEAT + SLH_K * FORS_TREE_BEATS;   // bottom layer
    localparam int unsigned LAST_BEAT = TOTAL_BEATS - 1;

    function automatic logic [SLH_BEAT_W-1:0] license_beat(input license_t lic,
                                                          input int       idx);
        if (idx < 0 || idx >= int'(TOTAL_BEATS)) return '0;
        return lic[idx];
    endfunction

    // -------------------------------------------------------------------------
    // The signer must sign for the parameter set of slh_pkg, with the public
    // keys slh_pkg was elaborated with.
    // -------------------------------------------------------------------------

    function automatic void check_keys();
        bit [SLH_NW-1:0] seed, root;
        if (dpi_slh_sig_len() != int'(SLH_SIG_LEN))
            $fatal(1, "slh_sign: the signer's signatures are %0d bytes, slh_pkg's %0d",
                   dpi_slh_sig_len(), SLH_SIG_LEN);
        for (int s = 0; s < int'(SLH_NUM_KEYS); s++) begin
            if (dpi_slh_init(s) != 0 || dpi_slh_pubkey(s, seed, root) != 0)
                $fatal(1, "slh_sign: slhdsa-c could not set up signer %0d", s);
            if (seed != SLH_KEYS[s].seed || root != SLH_KEYS[s].root)
                $fatal(1, "slh_sign: signer %0d public key is not the one in rtl/slh_keys.svh; run `make slh-keys`", s);
        end
    endfunction

    // -------------------------------------------------------------------------
    // SLH-DSA sign: signer_id signs the 256-bit message (pure mode, empty
    // context string). The signature is the deterministic variant, so the
    // same message gives the same licence.
    // -------------------------------------------------------------------------

    byte unsigned sig_buf [SLH_SIG_LEN];

    function automatic license_t slh_sign(
        input logic [255:0] message,
        input int           signer_id
    );
        static bit keys_checked = 1'b0;
        license_t lic;

        if (!keys_checked) begin
            check_keys();
            keys_checked = 1'b1;
        end

        if (dpi_slh_sign(signer_id, message, sig_buf) != 0)
            $fatal(1, "slh_sign: slhdsa-c could not sign for signer %0d", signer_id);

        // Element e is bytes [e*n, (e+1)*n) of the signature, first byte in
        // the MSBs
        for (int e = 0; e < int'(SLH_SIG_ELEMS); e++)
            for (int i = 0; i < int'(SLH_N); i++)
                lic[e][int'(SLH_BEAT_W) - 1 - 8*i -: 8] = sig_buf[e * int'(SLH_N) + i];
        return lic;
    endfunction

endpackage

`endif
