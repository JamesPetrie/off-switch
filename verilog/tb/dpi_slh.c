// DPI-C glue between tb_slh_sign_pkg.sv and the slhdsa-c test signers.

#include "svdpi.h"
#include "dpi_vec.h"
#include "slh_signer.h"

int dpi_slh_init(int signer)
{
    return slh_signer_init(signer);
}

// Bytes dpi_slh_sign() writes, for the testbench to check its buffer against
int dpi_slh_sig_len(void)
{
    return SLH_SIGNER_SIG_LEN;
}

// seed, root: bit [127:0], the two halves of the signer's public key
// (PK.seed || PK.root).
int dpi_slh_pubkey(int signer, svBitVecVal *seed, svBitVecVal *root)
{
    unsigned char pub[SLH_SIGNER_PUB_LEN];
    if (slh_signer_pubkey(signer, pub)) return 1;
    bytes_to_vec(pub,                8 * SLH_SIGNER_N, seed);
    bytes_to_vec(pub + SLH_SIGNER_N, 8 * SLH_SIGNER_N, root);
    return 0;
}

// message: bit [255:0]; sig: byte unsigned [SLH_SIGNER_SIG_LEN], the FIPS 205
// signature as slhdsa-c emits it. The same message gives the same signature.
int dpi_slh_sign(int signer, const svBitVecVal *message, unsigned char *sig)
{
    unsigned char msg[32];
    vec_to_bytes(message, 256, msg);
    return slh_signer_sign(signer, msg, sig);
}
