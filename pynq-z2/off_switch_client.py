#!/usr/bin/env python3
"""
Off-switch client for PYNQ-Z2.

Reads the 256-bit nonce from the security_block AXI IP and automatically
selects the ECDSA or streamed SLH-DSA protocol reported by the bitstream.

Register map:
    0x00..0x1C   nonce      (R, 8 words, word 0 = LSBs)
    0x20..0x3C   license s  (W, 8 words, word 0 = LSBs)
    0x40..0x5C   license r  (W, 8 words, word 0 = LSBs)
    0x60         control    (W, bit 0 start, bit 1 clear sticky status)
    0x64         status     (R, mode/stream/result flags)
    0x68         sig low    (W, first four bytes of an SLH-DSA stream word)
    0x6C         sig high   (W, next four bytes of an SLH-DSA stream word)
    0x70         sig push   (W, bit 0 push, bit 1 final word)
    0x74         sig count  (R, accepted 64-bit words)
    0x78..0x7C   allowance  (R, 64 bits)

Usage (on the board):
    ./off_switch_client.py [bitstream.bit] [--signature license.bin]

Run it directly as the normal user from any directory — if not already root
the script re-execs itself under `sudo -i` so the pynq-venv and XRT
environment are sourced (plain `sudo` fails with "No Devices Found").

If no bitstream argument is given, the script loads off_switch.bit from the
same directory as itself.
"""

import argparse
import os
import sys
import time
from pathlib import Path

REG_NONCE = 0x00
REG_LIC_S = 0x20
REG_LIC_R = 0x40
REG_CONTROL = 0x60
REG_STATUS = 0x64
REG_SIG_LO = 0x68
REG_SIG_HI = 0x6C
REG_SIG_PUSH = 0x70
REG_SIG_COUNT = 0x74

STATUS_SLOT_READY = 1 << 2
STATUS_DONE = 1 << 3
STATUS_PASSED = 1 << 4
STATUS_OVERFLOW = 1 << 5
STATUS_MODE_SHIFT = 8

CRYPTO_ECDSA = 0
CRYPTO_SLH_DSA = 2
SLH_SIGNATURE_BYTES = 7856
SLH_SIGNATURE_WORDS = SLH_SIGNATURE_BYTES // 8
OFFSWITCH_DOMAIN = b"OFFSWITCH-SLH-V1"
OFFSWITCH_DEVICE_ID = bytes.fromhex("00112233445566778899aabbccddeeff")
OFFSWITCH_POLICY_EPOCH = 1

DEFAULT_BITSTREAM = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "off_switch.bit")
IP_NAME           = "off_switch_axi_0"


def elevate_if_needed():
    """Re-exec under the board's root login environment when necessary."""
    if os.geteuid() == 0:
        return
    arguments = sys.argv[1:]
    if arguments and not arguments[0].startswith("-"):
        arguments[0] = os.path.abspath(arguments[0])
    for index, argument in enumerate(arguments[:-1]):
        if argument == "--signature":
            arguments[index + 1] = os.path.abspath(arguments[index + 1])
    os.execvp(
        "sudo",
        ["sudo", "-i", os.path.abspath(__file__), *arguments],
    )


def read_u256(ip, base):
    n = 0
    for i in range(8):
        n |= ip.read(base + 4 * i) << (32 * i)
    return n


def write_u256(ip, base, value):
    for i in range(8):
        ip.write(base + 4 * i, (value >> (32 * i)) & 0xFFFFFFFF)


def parse_hex(s, label):
    s = s.strip().lower().removeprefix("0x")
    if not s:
        raise ValueError(f"{label} is empty")
    if len(s) > 64:
        raise ValueError(f"{label} is {len(s)} hex chars (max 64)")
    return int(s, 16)


def wait_for_nonce(ip, prev, timeout_s=10.0, poll_s=0.05):
    """Poll until the nonce register is non-zero and differs from prev.
    Returns the latest read on timeout (may equal prev or be zero)."""
    deadline = time.monotonic() + timeout_s
    while time.monotonic() < deadline:
        n = read_u256(ip, REG_NONCE)
        if n != 0 and n != prev:
            return n
        time.sleep(poll_s)
    return read_u256(ip, REG_NONCE)


def canonical_slh_message(nonce):
    return (
        OFFSWITCH_DOMAIN
        + OFFSWITCH_DEVICE_ID
        + nonce.to_bytes(32, "big")
        + OFFSWITCH_POLICY_EPOCH.to_bytes(8, "big")
    )


def wait_for_status(ip, mask, timeout_s=300.0, poll_s=0.001):
    deadline = time.monotonic() + timeout_s
    while time.monotonic() < deadline:
        status = ip.read(REG_STATUS)
        if status & STATUS_OVERFLOW:
            raise RuntimeError("SLH-DSA stream FIFO overflowed")
        if status & mask:
            return status
        time.sleep(poll_s)
    raise TimeoutError(f"timed out waiting for status mask 0x{mask:08x}")


def stream_slh_signature(ip, signature):
    if len(signature) != SLH_SIGNATURE_BYTES:
        raise ValueError(
            f"signature has {len(signature)} bytes; expected {SLH_SIGNATURE_BYTES}"
        )

    ip.write(REG_CONTROL, 1)
    for word_index in range(SLH_SIGNATURE_WORDS):
        status = wait_for_status(ip, STATUS_SLOT_READY)
        if status & STATUS_DONE:
            raise RuntimeError("verification completed before the signature ended")
        chunk = signature[8 * word_index : 8 * (word_index + 1)]
        ip.write(REG_SIG_LO, int.from_bytes(chunk[:4], "little"))
        ip.write(REG_SIG_HI, int.from_bytes(chunk[4:], "little"))
        push = 1 | (2 if word_index == SLH_SIGNATURE_WORDS - 1 else 0)
        ip.write(REG_SIG_PUSH, push)

    status = wait_for_status(ip, STATUS_DONE)
    count = ip.read(REG_SIG_COUNT) & 0xFFFF
    if count != SLH_SIGNATURE_WORDS:
        raise RuntimeError(
            f"FPGA accepted {count} signature words; expected {SLH_SIGNATURE_WORDS}"
        )
    return bool(status & STATUS_PASSED)


def run_ecdsa(ip):
    prev_nonce = 0
    while True:
        nonce = wait_for_nonce(ip, prev_nonce)
        if nonce == 0:
            print("ERROR: timed out waiting for a non-zero nonce", file=sys.stderr)
            return 1
        if nonce == prev_nonce:
            print("(nonce unchanged; the previous license may have been rejected)")

        print(f"\nNonce (hex, big-endian): {nonce:064x}\n")
        try:
            r_str = input("License r (hex): ")
            s_str = input("License s (hex): ")
        except (EOFError, KeyboardInterrupt):
            print()
            return 0

        try:
            r = parse_hex(r_str, "r")
            s = parse_hex(s_str, "s")
        except ValueError as error:
            print(f"ERROR: {error}", file=sys.stderr)
            continue

        write_u256(ip, REG_LIC_R, r)
        write_u256(ip, REG_LIC_S, s)
        ip.write(REG_CONTROL, 1)
        print("ECDSA license submitted.")
        prev_nonce = nonce


def run_slh_dsa(ip, signature_path=None):
    prev_nonce = 0
    while True:
        nonce = wait_for_nonce(ip, prev_nonce)
        if nonce == 0:
            print("ERROR: timed out waiting for a non-zero nonce", file=sys.stderr)
            return 1
        message = canonical_slh_message(nonce)
        print(f"\nNonce (hex, big-endian): {nonce:064x}")
        print(f"Canonical message (hex): {message.hex()}\n")

        try:
            selected_path = signature_path
            if selected_path is None:
                selected_path = Path(input("SLH-DSA signature file: ").strip())
            signature = selected_path.expanduser().read_bytes()
            passed = stream_slh_signature(ip, signature)
        except (EOFError, KeyboardInterrupt):
            print()
            return 0
        except (OSError, ValueError, RuntimeError, TimeoutError) as error:
            print(f"ERROR: {error}", file=sys.stderr)
            if signature_path is not None:
                return 1
            continue

        print("SLH-DSA license accepted." if passed else "SLH-DSA license rejected.")
        if signature_path is not None:
            return 0 if passed else 1
        prev_nonce = nonce if passed else 0


def main():
    elevate_if_needed()
    from pynq import Overlay

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bitstream", nargs="?", default=DEFAULT_BITSTREAM)
    parser.add_argument(
        "--signature",
        type=Path,
        help="submit one SLH-DSA signature file and exit",
    )
    args = parser.parse_args()
    bit_file = args.bitstream

    print(f"Loading overlay {bit_file} ...")
    overlay = Overlay(bit_file)
    ip = getattr(overlay, IP_NAME)
    mode = (ip.read(REG_STATUS) >> STATUS_MODE_SHIFT) & 0x3
    print(f"Detected crypto mode {mode}.")
    if mode == CRYPTO_ECDSA:
        return run_ecdsa(ip)
    if mode == CRYPTO_SLH_DSA:
        return run_slh_dsa(ip, args.signature)
    print(f"ERROR: unsupported crypto mode {mode}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
