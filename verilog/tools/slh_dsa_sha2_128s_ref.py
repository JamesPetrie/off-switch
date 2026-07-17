#!/usr/bin/env python3
"""Small, independent SLH-DSA-SHA2-128s verification reference.

This model is intentionally straightforward rather than optimized. It mirrors
FIPS 205 verification algorithms and records intermediate values used as RTL
oracles. Only public inputs are accepted; no signing or secret-key operation
is implemented here.
"""

from __future__ import annotations

from dataclasses import dataclass
import hashlib
from typing import Any


N = 16
H = 63
D = 7
HP = 9
A = 12
K = 14
LG_W = 4
W = 1 << LG_W
WOTS_LEN1 = 32
WOTS_LEN2 = 3
WOTS_LEN = WOTS_LEN1 + WOTS_LEN2
HMSG_BYTES = 30
SIGNATURE_BYTES = 7_856
FORS_SIGNATURE_BYTES = K * (A + 1) * N
XMSS_SIGNATURE_BYTES = (WOTS_LEN + HP) * N

ADRS_WOTS_HASH = 0
ADRS_WOTS_PK = 1
ADRS_TREE = 2
ADRS_FORS_TREE = 3
ADRS_FORS_ROOTS = 4


@dataclass
class Address:
    """Numeric representation of the 32-byte FIPS 205 ADRS value."""

    layer: int = 0
    tree: int = 0
    adrs_type: int = 0
    key_pair: int = 0
    word6: int = 0
    word7: int = 0

    def set_type_and_clear(self, adrs_type: int) -> None:
        self.adrs_type = adrs_type
        self.key_pair = 0
        self.word6 = 0
        self.word7 = 0

    def set_type_and_clear_not_key_pair(self, adrs_type: int) -> None:
        self.adrs_type = adrs_type
        self.word6 = 0
        self.word7 = 0

    def compressed(self) -> bytes:
        """Return SHA-2 ADRSc = ADRS[3] || ADRS[8:16] || ADRS[19:32]."""
        return (
            self.layer.to_bytes(4, "big")[3:]
            + self.tree.to_bytes(12, "big")[4:]
            + self.adrs_type.to_bytes(4, "big")[3:]
            + self.key_pair.to_bytes(4, "big")
            + self.word6.to_bytes(4, "big")
            + self.word7.to_bytes(4, "big")
        )


def mgf1_sha256(seed: bytes, output_bytes: int) -> bytes:
    output = bytearray()
    counter = 0
    while len(output) < output_bytes:
        output.extend(hashlib.sha256(seed + counter.to_bytes(4, "big")).digest())
        counter += 1
    return bytes(output[:output_bytes])


def h_msg(message: bytes, public_key: bytes, signature: bytes) -> bytes:
    randomizer = signature[:N]
    pk_seed = public_key[:N]
    pk_root = public_key[N : 2 * N]
    # Pure SLH-DSA with an empty context hashes 0x00 || 0x00 || M.
    inner = hashlib.sha256(
        randomizer + pk_seed + pk_root + b"\x00\x00" + message
    ).digest()
    return mgf1_sha256(randomizer + pk_seed + inner, HMSG_BYTES)


def base_2b(value: bytes, bits: int, output_length: int) -> list[int]:
    output: list[int] = []
    byte_index = 0
    buffered_bits = 0
    buffer = 0
    mask = (1 << bits) - 1
    while len(output) < output_length:
        while buffered_bits < bits:
            buffer = (buffer << 8) | value[byte_index]
            byte_index += 1
            buffered_bits += 8
        buffered_bits -= bits
        output.append((buffer >> buffered_bits) & mask)
    return output


def t_l(pk_seed: bytes, adrs: Address, values: list[bytes]) -> bytes:
    if not values or any(len(value) != N for value in values):
        raise ValueError("T_l expects one or more n-byte values")
    payload = pk_seed + bytes(64 - N) + adrs.compressed() + b"".join(values)
    return hashlib.sha256(payload).digest()[:N]


def hash_f(pk_seed: bytes, adrs: Address, value: bytes) -> bytes:
    return t_l(pk_seed, adrs, [value])


def hash_h(pk_seed: bytes, adrs: Address, left: bytes, right: bytes) -> bytes:
    return t_l(pk_seed, adrs, [left, right])


def wots_digits(message: bytes) -> list[int]:
    digits = base_2b(message, LG_W, WOTS_LEN1)
    checksum = sum((W - 1) - digit for digit in digits)
    checksum <<= (8 - ((WOTS_LEN2 * LG_W) & 7)) & 7
    checksum_bytes = checksum.to_bytes((WOTS_LEN2 * LG_W + 7) // 8, "big")
    digits.extend(base_2b(checksum_bytes, LG_W, WOTS_LEN2))
    return digits


def chain(
    value: bytes,
    start: int,
    steps: int,
    pk_seed: bytes,
    adrs: Address,
) -> tuple[bytes, list[str]]:
    trace = [value.hex()]
    for hash_index in range(start, start + steps):
        adrs.word7 = hash_index
        value = hash_f(pk_seed, adrs, value)
        trace.append(value.hex())
    return value, trace


def wots_pk_from_sig(
    signature: bytes,
    message: bytes,
    pk_seed: bytes,
    adrs: Address,
) -> tuple[bytes, dict[str, Any]]:
    if len(signature) != WOTS_LEN * N:
        raise ValueError("invalid WOTS+ signature length")
    digits = wots_digits(message)
    endpoints: list[bytes] = []
    chains: list[dict[str, Any]] = []
    for chain_index, digit in enumerate(digits):
        adrs.word6 = chain_index
        endpoint, steps = chain(
            signature[chain_index * N : (chain_index + 1) * N],
            digit,
            (W - 1) - digit,
            pk_seed,
            adrs,
        )
        endpoints.append(endpoint)
        chain_trace: dict[str, Any] = {
            "chain_index": chain_index,
            "message_digit": digit,
            "signature_hex": signature[
                chain_index * N : (chain_index + 1) * N
            ].hex(),
            "endpoint_hex": endpoint.hex(),
        }
        # One complete real chain is enough to debug per-step RTL state while
        # keeping the public trace compact.
        if chain_index == 0:
            chain_trace["steps_hex"] = steps
        chains.append(chain_trace)

    adrs.set_type_and_clear_not_key_pair(ADRS_WOTS_PK)
    public_key = t_l(pk_seed, adrs, endpoints)
    return public_key, {
        "message_hex": message.hex(),
        "digits": digits,
        "chains": chains,
        "public_key_hex": public_key.hex(),
    }


def xmss_pk_from_sig(
    leaf_index: int,
    signature: bytes,
    message: bytes,
    pk_seed: bytes,
    adrs: Address,
) -> tuple[bytes, dict[str, Any]]:
    if len(signature) != XMSS_SIGNATURE_BYTES:
        raise ValueError("invalid XMSS signature length")
    wots_bytes = WOTS_LEN * N
    adrs.set_type_and_clear_not_key_pair(ADRS_WOTS_HASH)
    adrs.key_pair = leaf_index
    node, wots_trace = wots_pk_from_sig(
        signature[:wots_bytes], message, pk_seed, adrs
    )

    adrs.set_type_and_clear(ADRS_TREE)
    path_trace: list[dict[str, Any]] = []
    for height in range(HP):
        auth = signature[
            wots_bytes + height * N : wots_bytes + (height + 1) * N
        ]
        adrs.word6 = height + 1
        adrs.word7 = leaf_index >> (height + 1)
        node_before = node
        if ((leaf_index >> height) & 1) == 0:
            node = hash_h(pk_seed, adrs, node, auth)
            order = "node||auth"
        else:
            node = hash_h(pk_seed, adrs, auth, node)
            order = "auth||node"
        path_trace.append(
            {
                "height": height + 1,
                "tree_index": adrs.word7,
                "order": order,
                "node_before_hex": node_before.hex(),
                "auth_hex": auth.hex(),
                "node_after_hex": node.hex(),
            }
        )
    return node, {
        "leaf_index": leaf_index,
        "wots": wots_trace,
        "authentication_path": path_trace,
        "root_hex": node.hex(),
    }


def fors_pk_from_sig(
    signature: bytes,
    message_digest: bytes,
    pk_seed: bytes,
    adrs: Address,
) -> tuple[bytes, dict[str, Any]]:
    if len(signature) != FORS_SIGNATURE_BYTES:
        raise ValueError("invalid FORS signature length")
    indices = base_2b(message_digest, A, K)
    roots: list[bytes] = []
    trees: list[dict[str, Any]] = []
    offset = 0
    for tree_number, local_index in enumerate(indices):
        secret = signature[offset : offset + N]
        offset += N
        global_index = (tree_number << A) + local_index
        adrs.word6 = 0
        adrs.word7 = global_index
        node = hash_f(pk_seed, adrs, secret)
        tree_trace: dict[str, Any] = {
            "tree": tree_number,
            "local_index": local_index,
            "global_index": global_index,
            "secret_hex": secret.hex(),
            "leaf_hex": node.hex(),
            "authentication_path": [],
        }
        for height in range(A):
            auth = signature[offset : offset + N]
            offset += N
            adrs.word6 = height + 1
            adrs.word7 = global_index >> (height + 1)
            node_before = node
            if ((local_index >> height) & 1) == 0:
                node = hash_h(pk_seed, adrs, node, auth)
                order = "node||auth"
            else:
                node = hash_h(pk_seed, adrs, auth, node)
                order = "auth||node"
            tree_trace["authentication_path"].append(
                {
                    "height": height + 1,
                    "tree_index": adrs.word7,
                    "order": order,
                    "node_before_hex": node_before.hex(),
                    "auth_hex": auth.hex(),
                    "node_after_hex": node.hex(),
                }
            )
        roots.append(node)
        tree_trace["root_hex"] = node.hex()
        trees.append(tree_trace)

    adrs.set_type_and_clear_not_key_pair(ADRS_FORS_ROOTS)
    public_key = t_l(pk_seed, adrs, roots)
    return public_key, {
        "indices": indices,
        "trees": trees,
        "roots_hex": [root.hex() for root in roots],
        "public_key_hex": public_key.hex(),
    }


def verify_signature(
    message: bytes,
    public_key: bytes,
    signature: bytes,
) -> tuple[bool, dict[str, Any]]:
    if len(public_key) != 2 * N or len(signature) != SIGNATURE_BYTES:
        return False, {"error": "invalid public key or signature length"}
    pk_seed = public_key[:N]
    expected_root = public_key[N:]
    digest = h_msg(message, public_key, signature)
    fors_message = digest[:21]
    tree_index = int.from_bytes(digest[21:28], "big") & ((1 << (H - HP)) - 1)
    leaf_index = int.from_bytes(digest[28:30], "big") & ((1 << HP) - 1)

    fors_offset = N
    ht_offset = fors_offset + FORS_SIGNATURE_BYTES
    adrs = Address(tree=tree_index)
    adrs.set_type_and_clear_not_key_pair(ADRS_FORS_TREE)
    adrs.key_pair = leaf_index
    fors_public_key, fors_trace = fors_pk_from_sig(
        signature[fors_offset:ht_offset], fors_message, pk_seed, adrs
    )

    node = fors_public_key
    layers: list[dict[str, Any]] = []
    current_tree = tree_index
    current_leaf = leaf_index
    for layer in range(D):
        layer_signature = signature[
            ht_offset + layer * XMSS_SIGNATURE_BYTES :
            ht_offset + (layer + 1) * XMSS_SIGNATURE_BYTES
        ]
        layer_adrs = Address(layer=layer, tree=current_tree)
        node, layer_trace = xmss_pk_from_sig(
            current_leaf, layer_signature, node, pk_seed, layer_adrs
        )
        layer_trace["layer"] = layer
        layer_trace["tree_index"] = current_tree
        layers.append(layer_trace)
        if layer != D - 1:
            current_leaf = current_tree & ((1 << HP) - 1)
            current_tree >>= HP

    valid = node == expected_root
    trace = {
        "hmsg_hex": digest.hex(),
        "fors_message_hex": fors_message.hex(),
        "initial_tree_index": tree_index,
        "initial_leaf_index": leaf_index,
        "fors": fors_trace,
        "hypertree_layers": layers,
        "computed_root_hex": node.hex(),
        "expected_root_hex": expected_root.hex(),
        "valid": valid,
    }
    return valid, trace
