// Test signers for the SLH-DSA testbenches, backed by slhdsa-c, the PQ Code
// Package implementation of FIPS 205 (tb/slhdsa-c, git submodule).
//
// Each signer is an SLH-DSA-SHA2-128s key pair, the parameter set slh_pkg.sv
// is elaborated for. Keys are derived from a fixed per-signer seed and the
// signatures are the deterministic variant, so key generation and every
// signature are reproducible from run to run: `make slh-keys` writes the
// public keys into rtl/slh_keys.svh, and the testbench signs with the
// matching private keys at run time.

#ifndef SLH_SIGNER_H
#define SLH_SIGNER_H

#include <stddef.h>

#define SLH_SIGNER_MAX     4
#define SLH_SIGNER_N       16    // bytes per node

// Byte lengths of the FIPS 205 encodings for this parameter set
#define SLH_SIGNER_PUB_LEN (2 * SLH_SIGNER_N)                                   // PK.seed || PK.root
#define SLH_SIGNER_SIG_LEN ((1 + 14 * (1 + 12) + 7 * (35 + 9)) * SLH_SIGNER_N)  // R || FORS[k(1+a)] || HT[d(len+h')]

// All return 0 on success, non-zero on failure (details on stderr).
int slh_signer_init(int id);
int slh_signer_pubkey(int id, unsigned char out[SLH_SIGNER_PUB_LEN]);
// Pure SLH-DSA with an empty context string (FIPS 205 Algorithm 22).
int slh_signer_sign(int id, const unsigned char msg[32], unsigned char sig[SLH_SIGNER_SIG_LEN]);
// Verify with the reference implementation; 0 if the signature is valid.
int slh_signer_verify(const unsigned char pub[SLH_SIGNER_PUB_LEN], const unsigned char msg[32],
                      const unsigned char sig[SLH_SIGNER_SIG_LEN]);

#endif
