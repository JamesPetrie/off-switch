#!/usr/bin/env python3
"""
Off-switch license signer (host-side).

Prompts for a 256-bit nonce in hex, signs it with the secp256k1 private key
hard-wired into the testbench (d=2), and prints the resulting ECDSA license
components r and s as 64-char hex strings ready to paste into
off_switch_client.py.

The math mirrors test/test_ecdsa.ml: the nonce is treated as the raw message
hash z (no SHA-256 applied). The signing nonce k is freshly randomised per
signature.
"""

import secrets

# secp256k1 parameters
P = (1 << 256) - (1 << 32) - 977
N = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
GX = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
GY = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8

# Testbench fixture
PRIV_KEY = 2


def point_add(p1, p2):
    if p1 is None: return p2
    if p2 is None: return p1
    x1, y1 = p1
    x2, y2 = p2
    if x1 == x2:
        if (y1 + y2) % P == 0:
            return None
        lam = (3 * x1 * x1) * pow(2 * y1, -1, P) % P
    else:
        lam = (y2 - y1) * pow(x2 - x1, -1, P) % P
    x3 = (lam * lam - x1 - x2) % P
    y3 = (lam * (x1 - x3) - y1) % P
    return (x3, y3)


def scalar_mult(k, point):
    result = None
    addend = point
    while k:
        if k & 1:
            result = point_add(result, addend)
        addend = point_add(addend, addend)
        k >>= 1
    return result


def sign(z, d):
    while True:
        k = secrets.randbelow(N - 1) + 1
        rx, _ry = scalar_mult(k, (GX, GY))
        r = rx % N
        if r == 0:
            continue
        s = pow(k, -1, N) * (z + r * d) % N
        if s == 0:
            continue
        return r, s


def parse_hex(s):
    s = s.strip().lower().removeprefix("0x")
    if not s:
        raise ValueError("empty input")
    if len(s) > 64:
        raise ValueError(f"input is {len(s)} hex chars (max 64)")
    return int(s, 16)


def main():
    nonce = parse_hex(input("Nonce (hex): "))
    r, s = sign(nonce, PRIV_KEY)
    print()
    print(f"r = {r:064x}")
    print(f"s = {s:064x}")


if __name__ == "__main__":
    main()
