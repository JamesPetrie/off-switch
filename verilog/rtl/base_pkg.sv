// base_pkg — crypto-agnostic parameters for the security_block IP.
// Anything that varies per crypto backend belongs in a crypto-specific
// package (e.g. ecdsa_pkg / hss_pkg).
package base_pkg;

    localparam int unsigned LICENSE_STREAM_W = 256;

endpackage
