# PYNQ-Z2 SLH-DSA Off-Switch Prototype

This directory builds the Off-Switch prototype for the Digilent PYNQ-Z2. The
target is the Zynq-7000 `xc7z020clg400-1`; the processing system sends a
7,856-byte `SLH-DSA-SHA2-128s` signature to the programmable logic over
AXI-Lite. The RTL verifies the signature and lights LD0 while the allowance is
non-zero.

The default bitstream uses `CRYPTO_TYPE=2` and SHA-256. ECDSA remains available
as `CRYPTO_TYPE=0` for regression and comparison, but it is not the default
PYNQ build.

## Security boundary

The FPGA contains only the 32-byte SLH-DSA public key (`PK.seed || PK.root`).
Key generation and signing stay in an external liboqs authority. Never copy the
64-byte secret-key file to the board image or commit it to this repository.

The checked-in wrapper currently contains this development public key:

```text
PK.seed = 60a3dbd593dcc6e3bffd214c7603259b
PK.root = 835c3ada1c98b54a217b46638bf956eb
```

Replace those parameters before using a different authority. The prototype
device ID is `00112233445566778899aabbccddeeff`, and the policy epoch is `1`.

## Architecture

```text
PYNQ Python client
    |
    | AXI-Lite: start + 982 x 64-bit signature words
    v
one-entry stream FIFO
    |
    | valid / ready / data / keep / last
    v
security_block -> SLH-DSA-SHA2-128s verifier -> allowance -> LD0
```

The one-entry FIFO provides backpressure without storing the whole signature in
registers or block RAM. Byte lane zero (`sig_data[7:0]`) is the earliest byte
in the signature. Every word has `keep=0xff`; `last` is asserted on word 981.

## AXI-Lite register map

| Address | Access | Description |
| ---: | :---: | --- |
| `0x00`-`0x1c` | R | 256-bit nonce, eight little-word-order 32-bit registers |
| `0x20`-`0x3c` | RW | ECDSA `s` compatibility registers |
| `0x40`-`0x5c` | RW | ECDSA `r` compatibility registers |
| `0x60` | W | CONTROL: bit 0 start, bit 1 clear sticky state |
| `0x64` | R | STATUS, described below |
| `0x68` | RW | First four bytes of the staged signature word |
| `0x6c` | RW | Next four bytes of the staged signature word |
| `0x70` | W | SIG_PUSH: bit 0 push, bit 1 final word |
| `0x74` | R | Number of accepted 64-bit signature words |
| `0x78`-`0x7c` | R | 64-bit allowance, low word first |

STATUS bits:

| Bit(s) | Meaning |
| ---: | --- |
| 0 | Nonce ready |
| 1 | License transaction in flight |
| 2 | Signature FIFO can accept a word |
| 3 | Verification completed (sticky) |
| 4 | Signature passed (sticky) |
| 5 | FIFO overflow attempt (sticky) |
| 6 | Workload enabled / LED asserted |
| 7 | Raw verifier stream-ready signal |
| 9:8 | `CRYPTO_TYPE` (`0` ECDSA, `2` SLH-DSA) |

Writing CONTROL bit 0 clears the previous completion/result/overflow state,
clears the accepted-word counter, and starts one license transaction. Writing
bit 1 clears sticky status and the word counter without starting verification.

## Host-side validation

Run these checks in WSL before opening Vivado:

```bash
cd /home/chenhao/off-switch/verilog
make sim TB=pynq_axi_slh
make pynq-client-test
make pynq-lint
```

The AXI test checks word ordering, FIFO behavior, sticky status, start/done/pass
handshake, and allowance readback. The Python test checks the canonical
72-byte message and proves that the client preserves all 7,856 signature bytes.

## Build the overlay

Vivado is required; it is not installed by this repository. From a shell where
the Vivado environment is sourced:

```bash
cd /home/chenhao/off-switch/pynq-z2
vivado -mode batch -source build.tcl
```

The script targets PYNQ-Z2, configures FCLK0 to a conservative 50 MHz, packages
the local RTL as custom AXI IP, and produces matching files:

```text
pynq-z2/build/off_switch.bit
pynq-z2/build/off_switch.hwh
```

Inspect the synthesis and implementation timing/utilization reports before
changing the clock or treating the design as more than a prototype.

## Provision the external authority

Use the isolated Python environment containing liboqs-python. This example
creates the key outside the repository and refuses to overwrite an existing
secret file:

```bash
cd /home/chenhao/off-switch
/home/chenhao/toy-sphincs-off-switch/.venv/bin/python \
  verilog/tools/slh_dsa_pynq_authority.py keygen \
  --secret-key ~/.config/off-switch/slh_dsa_sha2_128s_secret.bin \
  --public-key ~/.config/off-switch/slh_dsa_sha2_128s_public.bin
```

Update `SLH_PK_SEED` and `SLH_PK_ROOT` in
`ip_repo/off_switch_axi_1_0/hdl/off_switch_axi.v` if the printed public key does
not match the development key above, then rebuild the overlay.

## Run on the board

Copy the matching `.bit`, `.hwh`, and client to the board. The client detects
the crypto mode, prints the nonce and exact message to sign, then waits for a
signature file:

```bash
python3 off_switch_client.py off_switch.bit
```

On the authority machine, sign the printed 32-byte nonce:

```bash
cd /home/chenhao/off-switch
/home/chenhao/toy-sphincs-off-switch/.venv/bin/python \
  verilog/tools/slh_dsa_pynq_authority.py sign \
  --secret-key ~/.config/off-switch/slh_dsa_sha2_128s_secret.bin \
  --nonce <64-hex-character-nonce> \
  --signature /tmp/off_switch_license.bin
```

Transfer only the generated signature to the board and enter its path at the
client prompt. The client streams exactly 982 words and reports whether the
hardware accepted or rejected the signature. A one-shot invocation is also
available:

```bash
python3 off_switch_client.py off_switch.bit \
  --signature /tmp/off_switch_license.bin
```

The one-shot form is valid only when the signature was generated for the nonce
currently published by that bitstream instance.

## Prototype limitations

- The nonce source is the repository's deterministic LFSR prototype, not a
  production entropy source.
- The SLH-DSA path supports one public key/signer.
- The AXI bridge has one 64-bit FIFO entry and is intentionally throughput
  conservative.
- SHA-512 and SLH-DSA-SHA2-192s are not implemented.
- Physical attack resistance, fault injection countermeasures, and side-channel
  hardening remain future work.
