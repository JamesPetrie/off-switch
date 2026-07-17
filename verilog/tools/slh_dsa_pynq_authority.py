#!/usr/bin/env python3
"""Provision and use an external SLH-DSA authority for the PYNQ prototype."""

from __future__ import annotations

import argparse
import hashlib
import os
from pathlib import Path

from generate_slh_dsa_vectors import ALGORITHM, configure_oqs


DOMAIN = b"OFFSWITCH-SLH-V1"
DEFAULT_DEVICE_ID = bytes.fromhex("00112233445566778899aabbccddeeff")
PUBLIC_KEY_BYTES = 32
SECRET_KEY_BYTES = 64
SIGNATURE_BYTES = 7856
NONCE_BYTES = 32


def parse_fixed_hex(value: str, byte_length: int, label: str) -> bytes:
    try:
        decoded = bytes.fromhex(value.removeprefix("0x"))
    except ValueError as error:
        raise argparse.ArgumentTypeError(f"{label} is not valid hex") from error
    if len(decoded) != byte_length:
        raise argparse.ArgumentTypeError(
            f"{label} must contain exactly {byte_length} bytes"
        )
    return decoded


def write_private_key(path: Path, secret_key: bytes) -> None:
    path = path.expanduser().resolve()
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(descriptor, "wb") as output:
        output.write(secret_key)


def command_keygen(args: argparse.Namespace, oqs_module: object) -> None:
    with oqs_module.Signature(ALGORITHM) as signer:
        public_key = signer.generate_keypair()
        secret_key = signer.export_secret_key()

    if len(public_key) != PUBLIC_KEY_BYTES:
        raise RuntimeError(f"unexpected public-key size: {len(public_key)}")
    if len(secret_key) != SECRET_KEY_BYTES:
        raise RuntimeError(f"unexpected secret-key size: {len(secret_key)}")

    write_private_key(args.secret_key, secret_key)
    if args.public_key:
        public_path = args.public_key.expanduser().resolve()
        public_path.parent.mkdir(parents=True, exist_ok=True)
        public_path.write_bytes(public_key)

    print("SLH-DSA authority created. Keep the secret-key file off the FPGA.")
    print(f"secret_key={args.secret_key.expanduser().resolve()}")
    if args.public_key:
        print(f"public_key={args.public_key.expanduser().resolve()}")
    print(f"SLH_PK_SEED={public_key[:16].hex()}")
    print(f"SLH_PK_ROOT={public_key[16:].hex()}")


def build_message(nonce: bytes, device_id: bytes, epoch: int) -> bytes:
    if not 0 <= epoch < (1 << 64):
        raise ValueError("policy epoch must fit in 64 bits")
    return DOMAIN + device_id + nonce + epoch.to_bytes(8, "big")


def command_sign(args: argparse.Namespace, oqs_module: object) -> None:
    secret_path = args.secret_key.expanduser().resolve()
    secret_key = secret_path.read_bytes()
    if len(secret_key) != SECRET_KEY_BYTES:
        raise RuntimeError(
            f"{secret_path} has {len(secret_key)} bytes; expected {SECRET_KEY_BYTES}"
        )

    nonce = parse_fixed_hex(args.nonce, NONCE_BYTES, "nonce")
    device_id = parse_fixed_hex(args.device_id, 16, "device ID")
    message = build_message(nonce, device_id, args.epoch)
    with oqs_module.Signature(ALGORITHM, secret_key) as signer:
        signature = signer.sign(message)
    if len(signature) != SIGNATURE_BYTES:
        raise RuntimeError(f"unexpected signature size: {len(signature)}")
    public_key = secret_key[-PUBLIC_KEY_BYTES:]
    with oqs_module.Signature(ALGORITHM) as verifier:
        if not verifier.verify(message, signature, public_key):
            raise RuntimeError("liboqs rejected the newly generated signature")

    output_path = args.signature.expanduser().resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_bytes(signature)
    print(f"message_hex={message.hex()}")
    print(f"signature={output_path}")
    print(f"signature_sha256={hashlib.sha256(signature).hexdigest()}")


def make_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--oqs-install-path",
        type=Path,
        help="liboqs installation prefix",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    keygen = subparsers.add_parser(
        "keygen",
        help="create a persistent authority key outside the repository",
    )
    keygen.add_argument("--secret-key", type=Path, required=True)
    keygen.add_argument("--public-key", type=Path)

    sign = subparsers.add_parser(
        "sign",
        help="sign the canonical 72-byte Off-Switch message for a board nonce",
    )
    sign.add_argument("--secret-key", type=Path, required=True)
    sign.add_argument("--nonce", required=True, help="32-byte board nonce in hex")
    sign.add_argument("--signature", type=Path, required=True)
    sign.add_argument(
        "--device-id",
        default=DEFAULT_DEVICE_ID.hex(),
        help="16-byte device ID in hex",
    )
    sign.add_argument("--epoch", type=int, default=1)
    return parser


def main() -> int:
    parser = make_parser()
    args = parser.parse_args()
    oqs_module = configure_oqs(args.oqs_install_path)
    if ALGORITHM not in oqs_module.get_enabled_sig_mechanisms():
        raise SystemExit(f"{ALGORITHM} is not enabled in this liboqs build")

    if args.command == "keygen":
        command_keygen(args, oqs_module)
    elif args.command == "sign":
        command_sign(args, oqs_module)
    else:
        parser.error(f"unsupported command: {args.command}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
