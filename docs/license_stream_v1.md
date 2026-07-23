# Conservative License Stream V1

**Status:** Draft for review
**Target repository:** `JamesPetrie/off-switch`

## 1. Changes

### 1.1 License Stream Interface

The license input is changed from a crypto-dependent packed value to a fixed
256-bit ready/valid stream:

```systemverilog
input  logic         license_valid;
output logic         license_ready;
input  logic [255:0] license;
```

A word transfers on a rising clock edge when both `license_valid` and
`license_ready` are high. The source may insert gaps between words. While
`license_valid` is high and `license_ready` is low, the source must keep the
current word stable.

The selected crypto backend determines the number of words in a transaction, so
the interface does not use `last` or `keep`.

### 1.2 Stream Wrapper

A stream wrapper receives the license words and reconstructs the packed license
before starting verification:

```text
256-bit ready/valid stream
            |
            v
  license stream wrapper
  (receive and reconstruct)
            |
            v
 packed-license security_block
```

The wrapper has two operating phases:

```text
Receive -> Verify -> Receive
```

During `Receive`, `license_ready` indicates that the wrapper can accept the
current word. After the final word is accepted, the wrapper deasserts
`license_ready`, holds the reconstructed license stable, and starts
verification. It reasserts `license_ready` when another license transaction can
be accepted.

Reset discards a partially received transaction. Verification starts only
after every expected word has transferred.

### 1.3 Serialization

Words are transmitted most-significant word first. The transport image is:

```text
{packed_license, zero_padding}
```

Padding extends the packed license to a whole number of 256-bit words and is
ignored by the receiver.

#### ECDSA

The 512-bit ECDSA license requires two words:

| Word | Value |
| ---: | --- |
| 0 | `r[255:0]` |
| 1 | `s[255:0]` |

#### HSS-LMS

The current 31,040-bit HSS-LMS `license_t` requires 122 words:

```text
ceil(31,040 / 256) = 122
```

The first 121 words contain 256 license bits each. The final word contains the
last 64 license bits in its most-significant positions and 192 zero-padding
bits in its least-significant positions.

## 2. Results

### 2.1 Functional Verification

The local full-buffer stream implementation produced the following results:

| Check | Result |
| --- | ---: |
| Verilator lint, ECDSA configuration | Pass |
| Verilator lint, HSS-LMS configuration | Pass |
| Verible lint | Pass |
| ECDSA security-block regression | 16/16 pass |
| HSS-LMS security-block regression | 17/17 pass |
| Demo regression | 3/3 pass |
| Dedicated stream protocol checks | 3/3 pass |

The stream protocol tests covered:

- gaps between input words;
- backpressure during verification;
- reset during a partial transaction; and
- starting verification only after the final word.

### 2.2 Area Results

Local Yosys mapping used the Nangate45 typical standard-cell library.

#### ECDSA

| Design | Mapped cell area | Change |
| --- | ---: | ---: |
| Packed-input baseline | 82,367.964 | - |
| Stream V1 | 85,793.246 | +4.16% |

The ECDSA stream receiver adds a 512-bit license buffer.

#### HSS-LMS

| Design | Mapped cell area | Change |
| --- | ---: | ---: |
| Packed-input baseline | 152,856.102 | - |
| Stream V1 | 354,397.386 | +131.85% |

The HSS-LMS stream receiver stores 122 complete 256-bit words:

```text
122 * 256 = 31,232 bits
```

The mapped design therefore contains 31,232 additional `DFF_X1` cells. This
complete-license buffer is the main source of the HSS-LMS area increase.

These figures are relative standard-cell mapping results. They are not FPGA
utilization results or sign-off timing results.
