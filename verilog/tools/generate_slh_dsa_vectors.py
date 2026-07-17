#!/usr/bin/env python3
"""Generate public SLH-DSA-SHA2-128s vectors for RTL integration tests.

The output intentionally excludes the secret key. liboqs supplies the
standards-compatible end-to-end oracle; intermediate FIPS 205 traces are added
separately when the hash/FORS/WOTS controllers are implemented.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
from typing import Any

from slh_dsa_sha2_128s_ref import verify_signature


ALGORITHM = "SLH_DSA_PURE_SHA2_128S"
DOMAIN = b"OFFSWITCH-SLH-V1"
MESSAGE_BYTES = 72
PUBLIC_KEY_BYTES = 32
SIGNATURE_BYTES = 7_856
STREAM_BYTES = 8

DEFAULT_DEVICE_ID = bytes.fromhex("00112233445566778899aabbccddeeff")
DEFAULT_NONCE = bytes.fromhex(
    "54ff53a510e527f9b05688c1f83d9ab5"
    "be0cd19a48b2de3a4b6d0c7fbc69a79a"
)


def parse_hex(value: str, expected_bytes: int, label: str) -> bytes:
    try:
        decoded = bytes.fromhex(value)
    except ValueError as error:
        raise argparse.ArgumentTypeError(f"{label} is not valid hexadecimal") from error
    if len(decoded) != expected_bytes:
        raise argparse.ArgumentTypeError(
            f"{label} must contain {expected_bytes} bytes, got {len(decoded)}"
        )
    return decoded


def sha256_hex(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def flip_first_bit(value: bytes) -> bytes:
    return bytes([value[0] ^ 1]) + value[1:]


def build_message(device_id: bytes, nonce: bytes, policy_epoch: int) -> bytes:
    if not 0 <= policy_epoch < (1 << 64):
        raise ValueError("policy epoch must fit in an unsigned 64-bit integer")
    message = DOMAIN + device_id + nonce + policy_epoch.to_bytes(8, "big")
    if len(message) != MESSAGE_BYTES:
        raise AssertionError(f"unexpected canonical message length: {len(message)}")
    return message


def write_stream_words(path: Path, signature: bytes) -> None:
    if len(signature) % STREAM_BYTES:
        raise ValueError("signature is not an exact number of 64-bit words")
    with path.open("w", encoding="ascii", newline="\n") as output:
        for offset in range(0, len(signature), STREAM_BYTES):
            chunk = signature[offset : offset + STREAM_BYTES]
            # Lane zero (data[7:0]) is the earliest byte on the RTL stream.
            output.write(f"{int.from_bytes(chunk, 'little'):016x}\n")


def configure_oqs(install_path: Path | None) -> Any:
    if install_path is not None:
        os.environ["OQS_INSTALL_PATH"] = str(install_path)
    else:
        candidate = Path(
            "/home/chenhao/toy-sphincs-off-switch/.venv/liboqs"
        )
        if candidate.is_dir():
            os.environ.setdefault("OQS_INSTALL_PATH", str(candidate))

    os.environ.setdefault("PYOQS_VERSION", "latest")
    try:
        import oqs  # pylint: disable=import-outside-toplevel
    except (ImportError, RuntimeError) as error:
        raise SystemExit(
            "Unable to load liboqs-python. Run this tool with the isolated "
            "Python environment used by toy-sphincs-off-switch and set "
            f"OQS_INSTALL_PATH if needed. Original error: {error}"
        ) from error
    return oqs


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("verilog/vectors/slh_dsa_sha2_128s_smoke"),
        help="directory for public RTL vector files",
    )
    parser.add_argument(
        "--oqs-install-path",
        type=Path,
        help="liboqs installation prefix (defaults to OQS_INSTALL_PATH)",
    )
    parser.add_argument(
        "--device-id",
        default=DEFAULT_DEVICE_ID.hex(),
        help="16-byte device identifier in hexadecimal",
    )
    parser.add_argument(
        "--nonce",
        default=DEFAULT_NONCE.hex(),
        help="32-byte Off-Switch nonce in hexadecimal",
    )
    parser.add_argument("--policy-epoch", type=int, default=1)
    args = parser.parse_args()

    device_id = parse_hex(args.device_id, 16, "device id")
    nonce = parse_hex(args.nonce, 32, "nonce")
    message = build_message(device_id, nonce, args.policy_epoch)
    oqs = configure_oqs(args.oqs_install_path)

    if ALGORITHM not in oqs.get_enabled_sig_mechanisms():
        raise SystemExit(f"{ALGORITHM} is not enabled in this liboqs build")

    with oqs.Signature(ALGORITHM) as signer, oqs.Signature(ALGORITHM) as verifier:
        public_key = signer.generate_keypair()
        signature = signer.sign(message)
        valid = bool(verifier.verify(message, signature, public_key))
        changed_message_rejected = not verifier.verify(
            flip_first_bit(message), signature, public_key
        )
        changed_signature_rejected = not verifier.verify(
            message, flip_first_bit(signature), public_key
        )
        details = dict(signer.details)

    if len(public_key) != PUBLIC_KEY_BYTES:
        raise SystemExit(f"unexpected public-key length: {len(public_key)}")
    if len(signature) != SIGNATURE_BYTES:
        raise SystemExit(f"unexpected signature length: {len(signature)}")
    if not (valid and changed_message_rejected and changed_signature_rejected):
        raise SystemExit("liboqs correctness checks failed")

    reference_valid, reference_trace = verify_signature(
        message, public_key, signature
    )
    if not reference_valid:
        raise SystemExit("independent Python SLH-DSA verification failed")
    reference_changed_message_rejected = not verify_signature(
        flip_first_bit(message), public_key, signature
    )[0]
    reference_changed_signature_rejected = not verify_signature(
        message, public_key, flip_first_bit(signature)
    )[0]
    if not (
        reference_changed_message_rejected
        and reference_changed_signature_rejected
    ):
        raise SystemExit("independent Python negative checks failed")
    hmsg = bytes.fromhex(reference_trace["hmsg_hex"])
    fors_message = hmsg[:21]
    tree_index = int.from_bytes(hmsg[21:28], "big") & ((1 << 54) - 1)
    leaf_index = int.from_bytes(hmsg[28:30], "big") & ((1 << 9) - 1)

    output_dir = args.output.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "message.bin").write_bytes(message)
    (output_dir / "message.hex").write_text(
        message.hex() + "\n", encoding="ascii"
    )
    (output_dir / "public_key.bin").write_bytes(public_key)
    (output_dir / "signature.bin").write_bytes(signature)
    (output_dir / "hmsg_digest.bin").write_bytes(hmsg)
    write_stream_words(output_dir / "signature_words64.hex", signature)
    (output_dir / "public_key_elements128.hex").write_text(
        "\n".join(
            public_key[offset : offset + 16].hex()
            for offset in range(0, len(public_key), 16)
        ) + "\n",
        encoding="ascii",
    )
    (output_dir / "signature_elements128.hex").write_text(
        "\n".join(
            signature[offset : offset + 16].hex()
            for offset in range(0, len(signature), 16)
        ) + "\n",
        encoding="ascii",
    )
    (output_dir / "hmsg_digest.hex").write_text(hmsg.hex() + "\n", encoding="ascii")
    (output_dir / "reference_trace.json").write_text(
        json.dumps(reference_trace, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    (output_dir / "fors_roots.hex").write_text(
        "\n".join(reference_trace["fors"]["roots_hex"]) + "\n",
        encoding="ascii",
    )
    (output_dir / "ht_layer_roots.hex").write_text(
        "\n".join(
            layer["root_hex"] for layer in reference_trace["hypertree_layers"]
        ) + "\n",
        encoding="ascii",
    )
    (output_dir / "computed_root.bin").write_bytes(
        bytes.fromhex(reference_trace["computed_root_hex"])
    )
    (output_dir / "fors_public_key.hex").write_text(
        reference_trace["fors"]["public_key_hex"] + "\n",
        encoding="ascii",
    )
    layer_zero_wots = reference_trace["hypertree_layers"][0]["wots"]
    (output_dir / "ht_layer0_wots_public_key.hex").write_text(
        layer_zero_wots["public_key_hex"] + "\n",
        encoding="ascii",
    )
    (output_dir / "ht_layer0_wots_endpoints.hex").write_text(
        "\n".join(
            chain_value["endpoint_hex"]
            for chain_value in layer_zero_wots["chains"]
        ) + "\n",
        encoding="ascii",
    )

    metadata = {
        "algorithm": ALGORITHM,
        "format_version": 1,
        "message": {
            "bytes": len(message),
            "domain_ascii": DOMAIN.decode("ascii"),
            "device_id_hex": device_id.hex(),
            "nonce_hex": nonce.hex(),
            "policy_epoch": args.policy_epoch,
            "sha256": sha256_hex(message),
        },
        "public_key": {
            "bytes": len(public_key),
            "sha256": sha256_hex(public_key),
        },
        "signature": {
            "bytes": len(signature),
            "sha256": sha256_hex(signature),
            "stream_width_bits": 64,
            "stream_words": len(signature) // STREAM_BYTES,
            "regions": {
                "R": [0, 16],
                "FORS": [16, 2928],
                "hypertree": [2928, 7856],
            },
        },
        "hmsg": {
            "bytes": len(hmsg),
            "digest_hex": hmsg.hex(),
            "fors_message_hex": fors_message.hex(),
            "tree_index": tree_index,
            "tree_index_hex": f"{tree_index:014x}",
            "leaf_index": leaf_index,
        },
        "liboqs": {
            "version": oqs.oqs_version(),
            "claimed_nist_level": details.get("claimed_nist_level"),
            "is_euf_cma": details.get("is_euf_cma"),
        },
        "checks": {
            "valid_signature_accepted": valid,
            "changed_message_rejected": changed_message_rejected,
            "changed_signature_rejected": changed_signature_rejected,
            "independent_python_verifier_accepted": reference_valid,
            "independent_python_changed_message_rejected": (
                reference_changed_message_rejected
            ),
            "independent_python_changed_signature_rejected": (
                reference_changed_signature_rejected
            ),
        },
        "secret_key_included": False,
    }
    (output_dir / "metadata.json").write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )

    print(f"Generated {ALGORITHM} public RTL vector in {output_dir}")
    print(f"  message:   {len(message):5d} bytes  sha256={sha256_hex(message)}")
    print(f"  public key:{len(public_key):5d} bytes  sha256={sha256_hex(public_key)}")
    print(f"  signature: {len(signature):5d} bytes  sha256={sha256_hex(signature)}")
    print(f"  stream:    {len(signature) // STREAM_BYTES:5d} x 64-bit words")
    print(
        f"  H_msg:     {hmsg.hex()}  "
        f"tree=0x{tree_index:014x} leaf=0x{leaf_index:03x}"
    )
    print(
        f"  root:      {reference_trace['computed_root_hex']}  Python trace verified"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
