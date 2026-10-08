// DPI-C glue between tb_hss_sign_pkg.sv and the hash-sigs test signers.
//
// Bit vectors cross DPI as svBitVecVal words, word 0 holding bits [31:0];
// the RFC 8554 byte strings are big-endian, so byte 0 is the top of the vector.

#include "svdpi.h"
#include "hss_signer.h"

static void vec_to_bytes(const svBitVecVal *w, int nbits, unsigned char *out)
{
    for (int i = 0; i < nbits / 8; i++) {
        int lsb = nbits - 8 - 8 * i;
        out[i] = (unsigned char)(w[lsb / 32] >> (lsb % 32));
    }
}

static void bytes_to_vec(const unsigned char *in, int nbits, svBitVecVal *w)
{
    for (int i = 0; i < nbits / 32; i++) w[i] = 0;
    for (int i = 0; i < nbits / 8; i++) {
        int lsb = nbits - 8 - 8 * i;
        w[lsb / 32] |= (svBitVecVal)in[i] << (lsb % 32);
    }
}

int dpi_hss_init(int signer)
{
    return hss_signer_init(signer);
}

// identifier: bit [127:0], root: bit [255:0], from the signer's public key
// (u32 L || u32 lms_type || u32 ots_type || I || T); the type words are
// returned so the testbench can check them against hss_pkg.
int dpi_hss_pubkey(int signer, int *levels, int *lms_type, int *ots_type,
                   svBitVecVal *identifier, svBitVecVal *root)
{
    unsigned char pub[HSS_SIGNER_PUB_LEN];
    if (hss_signer_pubkey(signer, pub)) return 1;
    *levels   = (pub[0]  << 24) | (pub[1]  << 16) | (pub[2]  << 8) | pub[3];
    *lms_type = (pub[4]  << 24) | (pub[5]  << 16) | (pub[6]  << 8) | pub[7];
    *ots_type = (pub[8]  << 24) | (pub[9]  << 16) | (pub[10] << 8) | pub[11];
    bytes_to_vec(pub + 12, 128, identifier);
    bytes_to_vec(pub + 28, 256, root);
    return 0;
}

// message: bit [255:0]; sig: byte unsigned [HSS_SIGNER_SIG_LEN], the RFC 8554
// HSS signature as hash-sigs emits it. Each call consumes the signer's next
// bottom-tree leaf.
int dpi_hss_sign(int signer, const svBitVecVal *message, unsigned char *sig)
{
    unsigned char msg[32];
    vec_to_bytes(message, 256, msg);
    return hss_signer_sign(signer, msg, sig);
}
