// base_pkg — crypto-agnostic parameters for the security_block IP.
// Anything that varies per crypto backend belongs in a crypto-specific
// package (e.g. ecdsa_pkg / hss_pkg).
package base_pkg;

    // Number of signers whose licenses must all be verified per nonce
    // before the allowance counter is incremented and a fresh nonce is
    // published (multi-sig enforcement — fixed submission order).
    localparam int unsigned NUM_SIGNERS = 2;

endpackage
