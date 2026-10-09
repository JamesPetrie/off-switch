// Test signers over the Cisco hash-sigs reference implementation. See hss_signer.h.

#include "hss_signer.h"

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <openssl/sha.h>

#include "hss.h"
#include "hss_verify.h"

static const param_set_t LM_TYPE[HSS_SIGNER_LEVELS]  = { LMS_SHA256_N32_H5,   LMS_SHA256_N32_H5 };
static const param_set_t OTS_TYPE[HSS_SIGNER_LEVELS] = { LMOTS_SHA256_N32_W8, LMOTS_SHA256_N32_W8 };

static struct {
    unsigned char           priv[HSS_MAX_PRIVATE_KEY_LEN];
    unsigned char           pub[HSS_SIGNER_PUB_LEN];
    struct hss_working_key *wk;
} signers[HSS_SIGNER_MAX];

// -----------------------------------------------------------------------------
// Deterministic randomness for key generation: SHA-256(seed || counter), with
// the seed fixed per signer. hash-sigs draws the key seed and I from this once
// at key generation; signatures need no randomness (the randomizer C is
// derived from the key seed).
// -----------------------------------------------------------------------------

static unsigned char drbg_seed[32];
static uint64_t      drbg_ctr;

static bool drbg(void *out, size_t len)
{
    unsigned char *p = out;
    while (len) {
        unsigned char blk[32], in[40];
        memcpy(in, drbg_seed, 32);
        for (int i = 0; i < 8; i++) in[32 + i] = (unsigned char)(drbg_ctr >> (56 - 8 * i));
        SHA256(in, sizeof in, blk);
        drbg_ctr++;
        size_t n = len < 32 ? len : 32;
        memcpy(p, blk, n);
        p += n; len -= n;
    }
    return true;
}

static void drbg_seed_for(int id)
{
    char label[64];
    int n = snprintf(label, sizeof label, "off-switch HSS test signer %d", id);
    SHA256((const unsigned char *)label, (size_t)n, drbg_seed);
    drbg_ctr = 0;
}

// -----------------------------------------------------------------------------

int hss_signer_init(int id)
{
    if (id < 0 || id >= HSS_SIGNER_MAX) {
        fprintf(stderr, "hss_signer: signer %d out of range (max %d)\n", id, HSS_SIGNER_MAX - 1);
        return 1;
    }
    if (signers[id].wk) return 0;   // already initialised

    if (hss_get_signature_len(HSS_SIGNER_LEVELS, LM_TYPE, OTS_TYPE) != HSS_SIGNER_SIG_LEN ||
        hss_get_public_key_len(HSS_SIGNER_LEVELS, LM_TYPE, OTS_TYPE) != HSS_SIGNER_PUB_LEN) {
        fprintf(stderr, "hss_signer: encoding lengths in hss_signer.h do not match hash-sigs\n");
        return 1;
    }

    struct hss_extra_info info;
    hss_init_extra_info(&info);

    drbg_seed_for(id);
    // No update callback: the private key lives in signers[id].priv
    if (!hss_generate_private_key(drbg, HSS_SIGNER_LEVELS, LM_TYPE, OTS_TYPE,
                                  NULL, signers[id].priv,
                                  signers[id].pub, sizeof signers[id].pub,
                                  NULL, 0, &info)) {
        fprintf(stderr, "hss_signer: key generation failed for signer %d (error %d)\n",
                id, (int)hss_extra_info_test_error_code(&info));
        return 1;
    }
    signers[id].wk = hss_load_private_key(NULL, signers[id].priv, 0, NULL, 0, &info);
    if (!signers[id].wk) {
        fprintf(stderr, "hss_signer: loading the private key failed for signer %d (error %d)\n",
                id, (int)hss_extra_info_test_error_code(&info));
        return 1;
    }
    return 0;
}

int hss_signer_pubkey(int id, unsigned char out[HSS_SIGNER_PUB_LEN])
{
    if (hss_signer_init(id)) return 1;
    memcpy(out, signers[id].pub, HSS_SIGNER_PUB_LEN);
    return 0;
}

int hss_signer_sign(int id, const unsigned char msg[32], unsigned char sig[HSS_SIGNER_SIG_LEN])
{
    if (hss_signer_init(id)) return 1;
    struct hss_extra_info info;
    hss_init_extra_info(&info);
    if (!hss_generate_signature(signers[id].wk, NULL, signers[id].priv,
                                msg, 32, sig, HSS_SIGNER_SIG_LEN, &info)) {
        fprintf(stderr, "hss_signer: signing failed for signer %d (error %d; out of leaves?)\n",
                id, (int)hss_extra_info_test_error_code(&info));
        return 1;
    }
    return 0;
}

int hss_signer_verify(const unsigned char pub[HSS_SIGNER_PUB_LEN], const unsigned char msg[32],
                      const unsigned char sig[HSS_SIGNER_SIG_LEN])
{
    return hss_validate_signature(pub, msg, 32, sig, HSS_SIGNER_SIG_LEN, NULL) ? 0 : 1;
}
