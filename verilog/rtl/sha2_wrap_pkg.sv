// SHA-2 wrapper — widths of the block and digest ports of sha2_wrap.

package sha2_wrap_pkg;

    localparam int unsigned SHA256_BLOCK_W  = 512;   // SHA-256 message block
    localparam int unsigned SHA256_DIGEST_W = 256;   // SHA-256 digest / running state

endpackage
