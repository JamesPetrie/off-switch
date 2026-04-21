// Minimal SHA-256 for DPI. Based on public domain code by Brad Conte.
// DPI wrapper at the bottom exposes sha256 to SystemVerilog.

#include "dpi_sha256.h"
#include <string.h>
#include "svdpi.h"

// --------------------------------------------------------------------------
// SHA-256 core
// --------------------------------------------------------------------------

#define ROTRIGHT(a,b) (((a) >> (b)) | ((a) << (32-(b))))
#define CH(x,y,z)  (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x,y,z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x)  (ROTRIGHT(x, 2) ^ ROTRIGHT(x,13) ^ ROTRIGHT(x,22))
#define EP1(x)  (ROTRIGHT(x, 6) ^ ROTRIGHT(x,11) ^ ROTRIGHT(x,25))
#define SIG0(x) (ROTRIGHT(x, 7) ^ ROTRIGHT(x,18) ^ ((x) >> 3))
#define SIG1(x) (ROTRIGHT(x,17) ^ ROTRIGHT(x,19) ^ ((x) >> 10))

static const uint32_t k[64] = {
    0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,
    0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
    0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,
    0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
    0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,
    0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,
    0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
    0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,
    0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
    0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,
    0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,
    0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
    0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,
    0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
};

static void sha256_transform(sha256_ctx *ctx, const uint8_t *data)
{
    uint32_t a, b, c, d, e, f, g, h, t1, t2, w[64];
    int i;

    for (i = 0; i < 16; i++)
        w[i] = ((uint32_t)data[4*i] << 24) | ((uint32_t)data[4*i+1] << 16)
              | ((uint32_t)data[4*i+2] << 8) | (uint32_t)data[4*i+3];
    for (i = 16; i < 64; i++)
        w[i] = SIG1(w[i-2]) + w[i-7] + SIG0(w[i-15]) + w[i-16];

    a = ctx->state[0]; b = ctx->state[1]; c = ctx->state[2]; d = ctx->state[3];
    e = ctx->state[4]; f = ctx->state[5]; g = ctx->state[6]; h = ctx->state[7];

    for (i = 0; i < 64; i++) {
        t1 = h + EP1(e) + CH(e,f,g) + k[i] + w[i];
        t2 = EP0(a) + MAJ(a,b,c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    ctx->state[0] += a; ctx->state[1] += b;
    ctx->state[2] += c; ctx->state[3] += d;
    ctx->state[4] += e; ctx->state[5] += f;
    ctx->state[6] += g; ctx->state[7] += h;
}

void sha256_init(sha256_ctx *ctx)
{
    ctx->datalen = 0;
    ctx->bitlen  = 0;
    ctx->state[0] = 0x6a09e667; ctx->state[1] = 0xbb67ae85;
    ctx->state[2] = 0x3c6ef372; ctx->state[3] = 0xa54ff53a;
    ctx->state[4] = 0x510e527f; ctx->state[5] = 0x9b05688c;
    ctx->state[6] = 0x1f83d9ab; ctx->state[7] = 0x5be0cd19;
}

void sha256_update(sha256_ctx *ctx, const uint8_t *data, size_t len)
{
    for (size_t i = 0; i < len; i++) {
        ctx->data[ctx->datalen++] = data[i];
        if (ctx->datalen == 64) {
            sha256_transform(ctx, ctx->data);
            ctx->bitlen += 512;
            ctx->datalen = 0;
        }
    }
}

void sha256_final(sha256_ctx *ctx, uint8_t *hash)
{
    uint32_t i = ctx->datalen;

    ctx->data[i++] = 0x80;
    if (i > 56) {
        while (i < 64) ctx->data[i++] = 0;
        sha256_transform(ctx, ctx->data);
        i = 0;
    }
    while (i < 56) ctx->data[i++] = 0;

    ctx->bitlen += ctx->datalen * 8;
    ctx->data[56] = (ctx->bitlen >> 56) & 0xff;
    ctx->data[57] = (ctx->bitlen >> 48) & 0xff;
    ctx->data[58] = (ctx->bitlen >> 40) & 0xff;
    ctx->data[59] = (ctx->bitlen >> 32) & 0xff;
    ctx->data[60] = (ctx->bitlen >> 24) & 0xff;
    ctx->data[61] = (ctx->bitlen >> 16) & 0xff;
    ctx->data[62] = (ctx->bitlen >>  8) & 0xff;
    ctx->data[63] = (ctx->bitlen      ) & 0xff;
    sha256_transform(ctx, ctx->data);

    for (i = 0; i < 8; i++) {
        hash[i*4    ] = (ctx->state[i] >> 24) & 0xff;
        hash[i*4 + 1] = (ctx->state[i] >> 16) & 0xff;
        hash[i*4 + 2] = (ctx->state[i] >>  8) & 0xff;
        hash[i*4 + 3] = (ctx->state[i]      ) & 0xff;
    }
}

void sha256(const uint8_t *data, size_t len, uint8_t *hash)
{
    sha256_ctx ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, data, len);
    sha256_final(&ctx, hash);
}

// --------------------------------------------------------------------------
// DPI wrapper
//
// SV declaration:
//   import "DPI-C" function void dpi_sha256(
//       input byte unsigned data[96], input int byte_len,
//       output bit [255:0] digest);
//
// Verilator packs bit[255:0] as an array of svBitVecVal (uint32_t words),
// word 0 = bits[31:0], word 7 = bits[255:224].
// SHA-256 output is big-endian bytes, so we need to reverse into the
// little-endian word array that verilator expects.
// --------------------------------------------------------------------------

extern "C" void dpi_sha256(const unsigned char *data, int byte_len,
                           svBitVecVal *digest)
{
    uint8_t hash[32];
    sha256((const uint8_t *)data, (size_t)byte_len, hash);

    // Pack big-endian hash bytes into svBitVecVal words (little-endian word order)
    // digest word 0 = bits[31:0] = hash[28..31]
    // digest word 7 = bits[255:224] = hash[0..3]
    for (int w = 0; w < 8; w++) {
        int byte_off = (7 - w) * 4;
        digest[w] = ((uint32_t)hash[byte_off]     << 24)
                  | ((uint32_t)hash[byte_off + 1] << 16)
                  | ((uint32_t)hash[byte_off + 2] <<  8)
                  | ((uint32_t)hash[byte_off + 3]);
    }
}
