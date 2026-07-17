#!/usr/bin/env python3
"""Recheck stored public SLH-DSA vectors with liboqs and the Python model."""

from __future__ import annotations

import argparse
from pathlib import Path

from generate_slh_dsa_vectors import (
    ALGORITHM,
    configure_oqs,
    flip_first_bit,
)
from slh_dsa_sha2_128s_ref import verify_signature


DEFAULT_VECTORS = (
    Path("verilog/vectors/slh_dsa_sha2_128s_smoke"),
    Path("verilog/vectors/slh_dsa_sha2_128s_offswitch_zero_nonce"),
)


def check_vector(vector_dir: Path, oqs_module: object) -> None:
    message = (vector_dir / "message.bin").read_bytes()
    public_key = (vector_dir / "public_key.bin").read_bytes()
    signature = (vector_dir / "signature.bin").read_bytes()

    reference_valid, trace = verify_signature(message, public_key, signature)
    reference_message_rejected = not verify_signature(
        flip_first_bit(message), public_key, signature
    )[0]
    reference_signature_rejected = not verify_signature(
        message, public_key, flip_first_bit(signature)
    )[0]

    with oqs_module.Signature(ALGORITHM) as verifier:
        oqs_valid = bool(verifier.verify(message, signature, public_key))
        oqs_message_rejected = not verifier.verify(
            flip_first_bit(message), signature, public_key
        )
        oqs_signature_rejected = not verifier.verify(
            message, flip_first_bit(signature), public_key
        )

    stored_hmsg = (vector_dir / "hmsg_digest.bin").read_bytes().hex()
    stored_root = (vector_dir / "computed_root.bin").read_bytes().hex()
    expected_root = public_key[16:32].hex()
    checks = {
        "Python valid": reference_valid,
        "Python changed message rejected": reference_message_rejected,
        "Python changed signature rejected": reference_signature_rejected,
        "liboqs valid": oqs_valid,
        "liboqs changed message rejected": oqs_message_rejected,
        "liboqs changed signature rejected": oqs_signature_rejected,
        "H_msg artifact matches": trace.get("hmsg_hex") == stored_hmsg,
        "computed root artifact matches": (
            trace.get("computed_root_hex") == stored_root == expected_root
        ),
        "no secret key artifact": not (vector_dir / "secret_key.bin").exists(),
    }
    failures = [name for name, passed in checks.items() if not passed]
    if failures:
        raise RuntimeError(
            f"{vector_dir}: failed checks: {', '.join(failures)}"
        )
    print(
        f"PASS {vector_dir}: liboqs + Python, positive/negative checks, "
        f"root={expected_root}"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "vectors",
        type=Path,
        nargs="*",
        default=list(DEFAULT_VECTORS),
        help="stored public vector directories",
    )
    parser.add_argument(
        "--oqs-install-path",
        type=Path,
        help="liboqs installation prefix",
    )
    args = parser.parse_args()

    oqs_module = configure_oqs(args.oqs_install_path)
    if ALGORITHM not in oqs_module.get_enabled_sig_mechanisms():
        raise SystemExit(f"{ALGORITHM} is not enabled in this liboqs build")
    for vector_dir in args.vectors:
        check_vector(vector_dir.resolve(), oqs_module)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
