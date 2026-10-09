// Test signers for the HSS-LMS testbenches, backed by the Cisco hash-sigs
// reference implementation of RFC 8554 (tb/hash-sigs, git submodule).
//
// Each signer is a two-level hypertree, LMS_SHA256_N32_H5 / LMOTS_SHA256_N32_W8
// at both levels, the parameter set hss_pkg.sv is elaborated for. Keys are
// derived from a fixed per-signer seed, so key generation and every signature
// are reproducible from run to run: `make hss-keys` writes the public keys
// into rtl/hss_keys.svh, and the testbench signs with the matching private
// keys at run time.

#ifndef HSS_SIGNER_H
#define HSS_SIGNER_H

#include <stddef.h>

#define HSS_SIGNER_MAX        4
#define HSS_SIGNER_LEVELS     2
#define HSS_SIGNER_TREE_H     5
#define HSS_SIGNER_WOTS_P     34

// Byte lengths of the RFC 8554 encodings for this parameter set
#define HSS_SIGNER_LMS_PUB_LEN 56    // u32 lms_type || u32 ots_type || I[16] || T[32]
#define HSS_SIGNER_PUB_LEN     60    // u32 L || LMS public key
#define HSS_SIGNER_OTS_SIG_LEN (4 + 32 + 32 * HSS_SIGNER_WOTS_P)                     // u32 ots_type || C || y[p]
#define HSS_SIGNER_LMS_SIG_LEN (4 + HSS_SIGNER_OTS_SIG_LEN + 4 + 32 * HSS_SIGNER_TREE_H) // u32 q || ots sig || u32 lms_type || path[h]
#define HSS_SIGNER_SIG_LEN     (4 + (HSS_SIGNER_LEVELS - 1) * (HSS_SIGNER_LMS_SIG_LEN + HSS_SIGNER_LMS_PUB_LEN) \
                                  + HSS_SIGNER_LMS_SIG_LEN)

// All return 0 on success, non-zero on failure (details on stderr).
int hss_signer_init(int id);
int hss_signer_pubkey(int id, unsigned char out[HSS_SIGNER_PUB_LEN]);
int hss_signer_sign(int id, const unsigned char msg[32], unsigned char sig[HSS_SIGNER_SIG_LEN]);
// Verify with the reference implementation; 0 if the signature is valid.
int hss_signer_verify(const unsigned char pub[HSS_SIGNER_PUB_LEN], const unsigned char msg[32],
                      const unsigned char sig[HSS_SIGNER_SIG_LEN]);

#endif
