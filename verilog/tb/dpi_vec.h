// Bit vectors across DPI-C, for the test signers' glue (dpi_hss.c, dpi_slh.c).
//
// Bit vectors cross DPI as svBitVecVal words, word 0 holding bits [31:0];
// the byte strings of the signature standards are big-endian, so byte 0 is
// the top of the vector.

#ifndef DPI_VEC_H
#define DPI_VEC_H

#include "svdpi.h"

static inline void vec_to_bytes(const svBitVecVal *w, int nbits, unsigned char *out)
{
    for (int i = 0; i < nbits / 8; i++) {
        int lsb = nbits - 8 - 8 * i;
        out[i] = (unsigned char)(w[lsb / 32] >> (lsb % 32));
    }
}

static inline void bytes_to_vec(const unsigned char *in, int nbits, svBitVecVal *w)
{
    for (int i = 0; i < nbits / 32; i++) w[i] = 0;
    for (int i = 0; i < nbits / 8; i++) {
        int lsb = nbits - 8 - 8 * i;
        w[lsb / 32] |= (svBitVecVal)in[i] << (lsb % 32);
    }
}

#endif
