#!/usr/bin/env python3
"""
Off-switch client for PYNQ-Z2.

Reads the 256-bit nonce from the security_block AXI IP, prints it as hex,
prompts the user for the ECDSA license components (r, s), writes them back,
and pulses the submit register.

Register map:
    0x00..0x1C   nonce      (R, 8 words, word 0 = LSBs)
    0x20..0x3C   license s  (W, 8 words, word 0 = LSBs)
    0x40..0x5C   license r  (W, 8 words, word 0 = LSBs)
    0x60         submit     (W, write 1 to assert license_valid)

Usage (on the board):
    ./off_switch_client.py [bitstream.bit]

Run it directly as the normal user from any directory — if not already root
the script re-execs itself under `sudo -i` so the pynq-venv and XRT
environment are sourced (plain `sudo` fails with "No Devices Found").

If no bitstream argument is given, the script loads off_switch.bit from the
same directory as itself.
"""

import os
import sys
import time

# Re-exec under `sudo -i` when not root, so the pynq-venv + XRT environment is
# sourced. This lets the script be launched directly (e.g. ./off_switch_client.py)
# from any directory. `sudo -i` resets cwd to root's home, so we always pass our
# own absolute path; resolve a relative bitstream arg before cwd changes too.
if os.geteuid() != 0:
    _args = sys.argv[1:]
    if _args:
        _args[0] = os.path.abspath(_args[0])
    os.execvp("sudo", ["sudo", "-i", os.path.abspath(__file__), *_args])

from pynq import Overlay

REG_NONCE = 0x00
REG_LIC_S = 0x20
REG_LIC_R = 0x40
REG_SUBMIT = 0x60

DEFAULT_BITSTREAM = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "off_switch.bit")
IP_NAME           = "off_switch_axi_0"


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


def main():
    bit_file = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_BITSTREAM

    print(f"Loading overlay {bit_file} ...")
    overlay = Overlay(bit_file)
    ip = getattr(overlay, IP_NAME)

    prev_nonce = 0
    while True:
        nonce = wait_for_nonce(ip, prev_nonce)
        if nonce == 0:
            print("ERROR: timed out waiting for a non-zero nonce", file=sys.stderr)
            return 1
        if nonce == prev_nonce:
            print("(nonce unchanged after timeout — more signatures might be needed or license was rejected)")

        print()
        print(f"Nonce (hex, big-endian): {nonce:064x}")
        print()

        try:
            r_str = input("License r (hex): ")
            s_str = input("License s (hex): ")
        except (EOFError, KeyboardInterrupt):
            print()
            return 0

        try:
            r = parse_hex(r_str, "r")
            s = parse_hex(s_str, "s")
        except ValueError as e:
            print(f"ERROR: {e}", file=sys.stderr)
            continue

        write_u256(ip, REG_LIC_R, r)
        write_u256(ip, REG_LIC_S, s)
        ip.write(REG_SUBMIT, 1)
        print("License submitted.")
        prev_nonce = nonce


if __name__ == "__main__":
    sys.exit(main())
