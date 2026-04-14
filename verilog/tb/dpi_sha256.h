// Minimal SHA-256 implementation for DPI use in testbenches.
// Based on public domain code by Brad Conte (brad AT bradconte.com).

#ifndef DPI_SHA256_H
#define DPI_SHA256_H

#include <stdint.h>
#include <stddef.h>

typedef struct {
    uint8_t  data[64];
    uint32_t datalen;
    uint64_t bitlen;
    uint32_t state[8];
} sha256_ctx;

void sha256_init(sha256_ctx *ctx);
void sha256_update(sha256_ctx *ctx, const uint8_t *data, size_t len);
void sha256_final(sha256_ctx *ctx, uint8_t *hash);

// One-shot convenience
void sha256(const uint8_t *data, size_t len, uint8_t *hash);

#endif
