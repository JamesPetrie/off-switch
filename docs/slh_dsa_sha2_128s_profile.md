# SLH-DSA-SHA2-128s RTL Profile

## Scope

This document freezes the first stateless post-quantum profile for the
Off-Switch RTL. The implementation target is the verification side of
`SLH-DSA-SHA2-128s` from NIST FIPS 205. Signing and secret-key storage remain
outside the chip and are exercised through liboqs during prototyping.

The first implementation uses SHA-256 only. The interfaces and storage widths
reserve room for a later `SLH-DSA-SHA2-192s` profile, where SHA-256 and SHA-512
are both required by FIPS 205.

## Fixed parameter set

| Name | Value |
| --- | ---: |
| `n` | 16 bytes |
| `h` | 63 |
| `d` | 7 |
| `h'` | 9 |
| `a` | 12 |
| `k` | 14 |
| `lg(w)` | 4 |
| `w` | 16 |
| `m` | 30 bytes |
| WOTS+ `len1` | 32 |
| WOTS+ `len2` | 3 |
| WOTS+ `len` | 35 |
| Public-key length | 32 bytes |
| Signature length | 7,856 bytes |

The public key is the FIPS 205 byte string:

```text
PK.seed (16 bytes) || PK.root (16 bytes)
```

Only public keys are stored in the RTL. No SLH-DSA secret seed, PRF key, WOTS+
secret value, or FORS secret value is stored on the verifier.

## Canonical Off-Switch message

The authority signs one fixed-format application message `M`:

| Offset | Size | Field | Encoding |
| ---: | ---: | --- | --- |
| 0 | 16 | Domain | ASCII `OFFSWITCH-SLH-V1` |
| 16 | 16 | Device identifier | Opaque bytes |
| 32 | 32 | Nonce | Opaque bytes from the security block |
| 64 | 8 | Policy epoch | Unsigned big-endian integer |

The total application-message length is 72 bytes. The Pure SLH-DSA API is used
with an empty context. The external FIPS 205 operation therefore verifies the
corresponding internal message encoding; host software must pass exactly the
72-byte application message above to the Pure API.

The allowance increment remains a trusted RTL parameter in the first version.
An untrusted host cannot request an arbitrary allowance value.

## Signature layout

The signature byte string is parsed in FIPS 205 order:

| Start | End (exclusive) | Size | Region |
| ---: | ---: | ---: | --- |
| 0 | 16 | 16 | Randomizer `R` |
| 16 | 2,928 | 2,912 | FORS signature |
| 2,928 | 7,856 | 4,928 | Hypertree signature |

The derived sizes are:

```text
FORS = k * (a + 1) * n
     = 14 * 13 * 16
     = 2,912 bytes

XMSS layer = (WOTS_LEN + h') * n
           = (35 + 9) * 16
           = 704 bytes

Hypertree = d * XMSS layer
          = 7 * 704
          = 4,928 bytes
```

All signature regions consist of 16-byte elements. The complete signature has
491 elements: one `R` element, 182 FORS elements, and 308 hypertree elements.

## H_msg split

`H_msg` produces 30 bytes. The result is split as specified by FIPS 205:

```text
FORS message: k * a = 168 bits = 21 bytes
Tree index:   h - h' = 54 bits, carried in 7 bytes and masked to 54 bits
Leaf index:   h' = 9 bits, carried in 2 bytes and masked to 9 bits
```

The masking and big-endian conversion are explicit verification test points.

## Signature stream

The SLH-DSA verifier uses a 64-bit valid-ready input rather than a 62,848-bit
packed license port. `security_block` supports both integration styles:

- `SLH_STREAM_INPUT=1` exposes the stream directly and reduces the otherwise
  unused packed license input to one bit. This is the PYNQ-Z2 hardware path.
- `SLH_STREAM_INPUT=0` retains the packed compatibility adapter for legacy
  simulations.

```systemverilog
sig_valid
sig_ready
sig_data[63:0]
sig_keep[7:0]
sig_last
```

Byte lane zero is `sig_data[7:0]` and is the earliest byte in the signature
stream. `sig_keep[0]` qualifies byte lane zero. For this profile the signature
length is a multiple of eight, so all accepted words require
`sig_keep == 8'hff`; `sig_last` is asserted only on word 981.

The parser outputs one top-aligned 256-bit element at a time. For the 128s
profile, the first signature byte is placed in `element_data[255:248]`, the
sixteenth byte is placed in `element_data[135:128]`, and the unused low 128
bits are zero. Top alignment keeps the interface unchanged when `n` grows to
24 or 32 bytes.

## Verification and policy boundaries

The standalone verifier is responsible for:

1. exact signature length and framing;
2. `H_msg` and digest/index parsing;
3. FORS public-key recovery;
4. seven WOTS+/XMSS verification layers;
5. comparison with `PK.root`; and
6. a final `verify_done` / `verify_passed` result.

The Off-Switch policy logic remains responsible for:

1. nonce generation and publication;
2. selecting the expected signer's public key;
3. requiring every configured signer for the same nonce;
4. incrementing allowance only after the last signer passes; and
5. rotating the nonce only after complete authorization.

The first `CRYPTO_TYPE=2` path supports one configured signer. ECDSA and HSS/LMS
keep their existing multi-signer behavior; an SLH-DSA public-key array and
signer selector are a documented follow-up rather than an implicit claim.

A reset, malformed stream, truncated signature, extra signature byte, invalid
signature, or wrong signer must fail closed and must not increase allowance.

## Upgrade constraints

To avoid rewriting the verifier for SHA2-192s:

- algorithm controllers request abstract `F`, `H`, `T_l`, and `H_msg`
  operations instead of instantiating SHA-256 directly;
- internal node ports reserve 256 bits;
- all loop bounds and signature offsets are derived from profile constants;
- the signature input remains byte-stream based; and
- the future hash service selects SHA-256 or SHA-512 per FIPS 205 function.

## Implemented RTL blocks

The local SHA2-128s path now contains:

1. exact 64-bit signature framing and 16-byte element parsing;
2. SHA-256 `H_msg`, compressed ADRS, `F`, `H`, and variable-length `T_l`;
3. FORS public-key recovery over 14 trees of height 12;
4. WOTS+ chain completion and checksum handling;
5. XMSS authentication-path recovery and seven-layer hypertree recovery;
6. end-to-end root comparison with distinct invalid/error results;
7. direct streaming and a legacy packed-license adapter;
8. `security_block` selection through `CRYPTO_TYPE=2`; and
9. a PYNQ-Z2 AXI-Lite stream bridge with sticky completion/error status.

Signing, secret-key generation/storage, multi-signer SLH-DSA selection,
SHA2-192s/SHA-512, throughput optimization, and production entropy remain out
of scope for this first verifier.

## Local verification

The stored vectors contain public keys, messages, signatures, and public
intermediate traces only. Recheck them with the isolated liboqs environment:

```text
/home/chenhao/toy-sphincs-off-switch/.venv/bin/python \
  verilog/tools/check_slh_dsa_vectors.py
```

Run the principal RTL gates from `verilog/`:

```text
make sim TB=slh_dsa_verify
make sim TB=top_slh
make sim TB=top_slh_stream
make sim TB=pynq_axi_slh
make pynq-client-test
make lint CRYPTO_TYPE=0
make lint CRYPTO_TYPE=1
make lint CRYPTO_TYPE=2
make lint CRYPTO_TYPE=2 SLH_STREAM_INPUT=1
make pynq-lint
```

The end-to-end test covers a valid liboqs signature, changed signature, changed
message, wrong public key, malformed byte keep, input gaps/backpressure, and
reset during a partial transaction. The packed and streamed security-block
tests prove that a tampered signature cannot increase allowance. The AXI test
checks register semantics, 64-bit byte order, FIFO overflow handling, sticky
completion, result status, and allowance readback.

## Acceptance gates

This first SHA2-128s verification milestone requires:

- liboqs accepts a signature that the RTL accepts;
- changing the message, public key, or any signature region is rejected;
- random input backpressure does not change the result;
- reset during a transaction clears all partial state;
- ECDSA and HSS/LMS regression tests remain passing; and
- allowance increases only after complete SLH-DSA verification.

Normative algorithm reference: NIST FIPS 205,
<https://doi.org/10.6028/NIST.FIPS.205>.
