// Test signers over the slhdsa-c implementation of FIPS 205. See slh_signer.h.

#include "slh_signer.h"

#include <stdio.h>
#include <string.h>

#include "slh_dsa.h"
#include "sha2_api.h"

#define PRM (&slh_dsa_sha2_128s)

static struct {
    int           ready;
    unsigned char priv[4 * SLH_SIGNER_N];   // SK.seed || SK.prf || PK.seed || PK.root
    unsigned char pub[SLH_SIGNER_PUB_LEN];
} signers[SLH_SIGNER_MAX];

int slh_signer_init(int id)
{
    if (id < 0 || id >= SLH_SIGNER_MAX) {
        fprintf(stderr, "slh_signer: signer %d out of range (max %d)\n", id, SLH_SIGNER_MAX - 1);
        return 1;
    }
    if (signers[id].ready) return 0;   // already initialised

    if (slh_sig_sz(PRM) != SLH_SIGNER_SIG_LEN || slh_pk_sz(PRM) != SLH_SIGNER_PUB_LEN ||
        slh_sk_sz(PRM) != sizeof signers[id].priv) {
        fprintf(stderr, "slh_signer: encoding lengths in slh_signer.h do not match slhdsa-c\n");
        return 1;
    }

    // Deterministic key material: SK.seed || SK.prf || PK.seed are the first
    // 3n bytes of SHA-512 of a label fixed per signer. Signatures need no
    // randomness (the deterministic variant derives the randomizer R from
    // SK.prf).
    char          label[64];
    unsigned char seed[64];
    int n = snprintf(label, sizeof label, "off-switch SLH-DSA test signer %d", id);
    sha2_512(seed, label, (size_t)n);

    if (slh_keygen_internal(signers[id].priv, signers[id].pub, seed, seed + SLH_SIGNER_N,
                            seed + 2 * SLH_SIGNER_N, PRM) != 0) {
        fprintf(stderr, "slh_signer: key generation failed for signer %d\n", id);
        return 1;
    }
    signers[id].ready = 1;
    return 0;
}

int slh_signer_pubkey(int id, unsigned char out[SLH_SIGNER_PUB_LEN])
{
    if (slh_signer_init(id)) return 1;
    memcpy(out, signers[id].pub, SLH_SIGNER_PUB_LEN);
    return 0;
}

int slh_signer_sign(int id, const unsigned char msg[32], unsigned char sig[SLH_SIGNER_SIG_LEN])
{
    if (slh_signer_init(id)) return 1;
    // No context string, no added randomness
    if (slh_sign(sig, msg, 32, NULL, 0, signers[id].priv, NULL, PRM) != SLH_SIGNER_SIG_LEN) {
        fprintf(stderr, "slh_signer: signing failed for signer %d\n", id);
        return 1;
    }
    return 0;
}

int slh_signer_verify(const unsigned char pub[SLH_SIGNER_PUB_LEN], const unsigned char msg[32],
                      const unsigned char sig[SLH_SIGNER_SIG_LEN])
{
    return slh_verify(msg, 32, sig, SLH_SIGNER_SIG_LEN, NULL, 0, pub, PRM) ? 0 : 1;
}
