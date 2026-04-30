#!/usr/bin/env python3
"""
Reference LMS/WOTS+ implementation for generating test vectors.
Implements RFC 8554 with w=8, n=32 (SHA-256).

Generates test vectors for the HardCaml WOTS+ and Merkle verification engines.
"""

import hashlib
import struct
import json
import os

# Parameters: w=8, n=32 (SHA-256), p=34
W = 8
N = 32
P1 = 32  # ceil(8*n / w) = ceil(256/8) = 32
P2 = 2   # checksum digits
P = P1 + P2  # 34 total chains
MAX_COEF = (1 << W) - 1  # 255


def sha256(data: bytes) -> bytes:
    return hashlib.sha256(data).digest()


def coefs(S: bytes, w: int) -> list:
    """Extract base-2^w digits from byte string S."""
    digits = []
    for byte in S:
        digits.append(byte)  # w=8, so each byte is one digit
    return digits


def checksum(msg_coefs: list) -> list:
    """Compute WOTS+ checksum digits."""
    csum = sum(MAX_COEF - c for c in msg_coefs)
    # Encode checksum as 2 base-256 digits (big-endian)
    csum_bytes = struct.pack(">H", csum)
    return [b for b in csum_bytes]


def chain(x: bytes, i_val: int, identifier: bytes, q: int, chain_idx: int, start: int, steps: int) -> bytes:
    """
    Compute hash chain: apply H() 'steps' times starting from position 'start'.

    Per RFC 8554 Section 4.1, the chain function is:
    H(I || u32str(q) || u16str(i) || u8str(j) || tmp)
    where I=identifier, q=leaf index, i=chain index, j=step position.
    """
    tmp = x
    for j in range(start, start + steps):
        # Build hash input per RFC 8554
        msg = identifier  # I (16 bytes)
        msg += struct.pack(">I", q)       # q (4 bytes)
        msg += struct.pack(">H", chain_idx)  # i (2 bytes)
        msg += struct.pack(">B", j)       # j (1 byte)
        msg += tmp                        # tmp (32 bytes)
        tmp = sha256(msg)
    return tmp


def wots_keygen(identifier: bytes, q: int, seed: bytes):
    """Generate WOTS+ keypair.

    Per RFC 8554 Appendix A, private key elements are:
        x_q[i] = H(I || u32str(q) || u16str(i) || u8str(0xff) || SEED)
    """
    sk = []
    for i in range(P):
        sk_i = sha256(identifier + struct.pack(">I", q) + struct.pack(">H", i)
                       + struct.pack(">B", 0xff) + seed)
        sk.append(sk_i)

    # Compute public key elements: chain each sk element MAX_COEF times
    pk_elements = []
    for i in range(P):
        pk_i = chain(sk[i], 0, identifier, q, i, 0, MAX_COEF)
        pk_elements.append(pk_i)

    # Public key candidate Kc = H(I || u32str(q) || u16str(D_PBLC) || pk[0] || ... || pk[p-1])
    D_PBLC = 0x8080
    msg = identifier + struct.pack(">I", q) + struct.pack(">H", D_PBLC)
    for pk_i in pk_elements:
        msg += pk_i
    Kc = sha256(msg)

    return sk, pk_elements, Kc


def wots_sign(message_hash: bytes, sk: list, identifier: bytes, q: int):
    """Sign a message hash using WOTS+."""
    # Extract digits from message hash
    msg_digits = coefs(message_hash, W)

    # Compute and append checksum digits
    csum_digits = checksum(msg_digits)
    all_digits = msg_digits + csum_digits

    # Generate signature: chain each sk element d[i] times
    sig = []
    for i in range(P):
        sig_i = chain(sk[i], 0, identifier, q, i, 0, all_digits[i])
        sig.append(sig_i)

    return sig, all_digits


def wots_verify(message_hash: bytes, sig: list, identifier: bytes, q: int, expected_Kc: bytes):
    """Verify a WOTS+ signature. Returns True if valid."""
    # Extract digits from message hash
    msg_digits = coefs(message_hash, W)
    csum_digits = checksum(msg_digits)
    all_digits = msg_digits + csum_digits

    # Complete each chain: from position d[i] to MAX_COEF
    pk_candidate = []
    for i in range(P):
        steps = MAX_COEF - all_digits[i]
        pk_i = chain(sig[i], 0, identifier, q, i, all_digits[i], steps)
        pk_candidate.append(pk_i)

    # Compute public key candidate
    D_PBLC = 0x8080
    msg = identifier + struct.pack(">I", q) + struct.pack(">H", D_PBLC)
    for pk_i in pk_candidate:
        msg += pk_i
    Kc = sha256(msg)

    return Kc == expected_Kc


def merkle_leaf_hash(identifier: bytes, q: int, Kc: bytes) -> bytes:
    """Compute Merkle tree leaf hash per RFC 8554."""
    D_LEAF = 0x8282
    msg = identifier + struct.pack(">I", q) + struct.pack(">H", D_LEAF) + Kc
    return sha256(msg)


def merkle_internal_hash(identifier: bytes, node_num: int, left: bytes, right: bytes) -> bytes:
    """Compute Merkle tree internal node hash per RFC 8554."""
    D_INTR = 0x8383
    msg = identifier + struct.pack(">I", node_num) + struct.pack(">H", D_INTR) + left + right
    return sha256(msg)


def build_merkle_tree(identifier: bytes, leaves: list, height: int) -> tuple:
    """Build a full Merkle tree. Returns (root, all_nodes)."""
    num_leaves = 1 << height
    assert len(leaves) == num_leaves

    # Level 0 = leaves (bottom)
    nodes = [None] * (2 * num_leaves)
    for i in range(num_leaves):
        nodes[num_leaves + i] = leaves[i]

    # Build internal nodes bottom-up
    for i in range(num_leaves - 1, 0, -1):
        nodes[i] = merkle_internal_hash(identifier, i, nodes[2*i], nodes[2*i+1])

    root = nodes[1]
    return root, nodes


def merkle_auth_path(nodes: list, height: int, leaf_idx: int) -> list:
    """Extract authentication path for a given leaf."""
    num_leaves = 1 << height
    path = []
    node_idx = num_leaves + leaf_idx
    for _ in range(height):
        sibling_idx = node_idx ^ 1
        path.append(nodes[sibling_idx])
        node_idx = node_idx >> 1
    return path


def generate_test_vectors():
    """Generate test vectors for the HardCaml implementation."""

    # Fixed test parameters
    identifier = b'\x01' * 16  # I = 16 bytes of 0x01
    q = 5  # leaf index
    seed = b'\xAB' * 32  # deterministic seed
    height = 4  # small tree for testing (16 leaves)

    # Generate WOTS+ keypair
    sk, pk_elements, Kc = wots_keygen(identifier, q, seed)

    # Message to sign (the nonce in off-switch context)
    message = sha256(b"test message for off-switch")

    # Compute message hash Q per RFC 8554 Section 4.6
    # Q = H(I || u32str(q) || u16str(D_MESG) || C || message)
    C = os.urandom(32)  # randomizer
    C = b'\xCC' * 32  # deterministic for testing
    D_MESG = 0x8181
    Q_input = identifier + struct.pack(">I", q) + struct.pack(">H", D_MESG) + C + message
    Q = sha256(Q_input)

    # Sign
    sig, digits = wots_sign(Q, sk, identifier, q)

    # Verify
    assert wots_verify(Q, sig, identifier, q, Kc), "Self-verification failed!"

    # Build Merkle tree (small, height=4, 16 leaves)
    # All leaves use the same master seed; wots_keygen derives per-leaf keys via RFC 8554 Appendix A
    all_Kc = []
    for leaf_q in range(1 << height):
        _, _, leaf_Kc = wots_keygen(identifier, leaf_q, seed)
        all_Kc.append(leaf_Kc)

    # Compute leaf hashes
    leaf_hashes = [merkle_leaf_hash(identifier, i, all_Kc[i]) for i in range(1 << height)]

    # Build tree
    root, nodes = build_merkle_tree(identifier, leaf_hashes, height)

    # Get auth path for leaf q
    auth_path = merkle_auth_path(nodes, height, q)

    # Verify Merkle path
    computed = leaf_hashes[q]
    node_idx = (1 << height) + q
    for level in range(height):
        sibling = auth_path[level]
        parent_idx = node_idx >> 1
        if node_idx % 2 == 0:
            computed = merkle_internal_hash(identifier, parent_idx, computed, sibling)
        else:
            computed = merkle_internal_hash(identifier, parent_idx, sibling, computed)
        node_idx = parent_idx
    assert computed == root, "Merkle verification failed!"

    # Output test vectors
    vectors = {
        "identifier": identifier.hex(),
        "q": q,
        "seed": seed.hex(),
        "message": message.hex(),
        "C": C.hex(),
        "Q": Q.hex(),
        "digits": digits,
        "signature": [s.hex() for s in sig],
        "Kc": Kc.hex(),
        "pk_elements": [pk.hex() for pk in pk_elements],
        "tree_height": height,
        "leaf_hash": leaf_hashes[q].hex(),
        "auth_path": [node.hex() for node in auth_path],
        "merkle_root": root.hex(),
    }

    return vectors


def print_ocaml_vectors(v):
    """Print test vectors as OCaml hex literals."""
    print("(* Auto-generated test vectors from reference_lms.py *)")
    print()
    print(f'let identifier_hex = "{v["identifier"]}"')
    print(f'let q = {v["q"]}')
    print(f'let message_hex = "{v["message"]}"')
    print(f'let c_hex = "{v["C"]}"')
    print(f'let q_hash_hex = "{v["Q"]}"')
    print(f'let kc_hex = "{v["Kc"]}"')
    print(f'let leaf_hash_hex = "{v["leaf_hash"]}"')
    print(f'let merkle_root_hex = "{v["merkle_root"]}"')
    print(f'let tree_height = {v["tree_height"]}')
    print()
    print("let digits = [|")
    for i in range(0, len(v["digits"]), 8):
        chunk = v["digits"][i:i+8]
        print("  " + "; ".join(str(d) for d in chunk) + ";")
    print("|]")
    print()
    print("let signature_hex = [|")
    for s in v["signature"]:
        print(f'  "{s}";')
    print("|]")
    print()
    print("let auth_path_hex = [|")
    for node in v["auth_path"]:
        print(f'  "{node}";')
    print("|]")


def emit_sv_tree_pkg(identifier: bytes, master_seed: bytes, height: int,
                     output_path: str):
    """Emit a SystemVerilog package with the pre-built Merkle tree."""

    num_leaves = 1 << height
    total_nodes = 2 * num_leaves  # 1-based: indices 1..(2*num_leaves-1)

    # Build all leaf Kc values using RFC 8554 Appendix A derivation
    all_Kc = []
    for q in range(num_leaves):
        _, _, Kc = wots_keygen(identifier, q, master_seed)
        all_Kc.append(Kc)

    # Leaf hashes
    leaf_hashes = [merkle_leaf_hash(identifier, i, all_Kc[i])
                   for i in range(num_leaves)]

    # Build tree
    root, nodes = build_merkle_tree(identifier, leaf_hashes, height)

    # Self-check a few leaves
    for test_q in [0, 5, num_leaves - 1]:
        path = merkle_auth_path(nodes, height, test_q)
        computed = leaf_hashes[test_q]
        idx = num_leaves + test_q
        for lv in range(height):
            parent = idx >> 1
            sib = path[lv]
            if idx % 2 == 0:
                computed = merkle_internal_hash(identifier, parent, computed, sib)
            else:
                computed = merkle_internal_hash(identifier, parent, sib, computed)
            idx = parent
        assert computed == root, f"Self-check failed for leaf {test_q}"

    # Emit SV
    with open(output_path, "w") as f:
        f.write("// Auto-generated by reference_lms.py — do not edit\n")
        f.write("// Merkle tree for HSS-LMS testbench\n\n")
        f.write("package tb_hss_tree_pkg;\n\n")
        f.write("    import arith_pkg::*;\n\n")
        f.write(f"    localparam int unsigned TREE_HEIGHT = {height};\n")
        f.write(f"    localparam int unsigned NUM_LEAVES  = 1 << TREE_HEIGHT;\n\n")
        f.write(f"    localparam int unsigned NUM_NODES   = 2*NUM_LEAVES - 1;\n\n")

        f.write(f"    localparam logic [127:0] IDENTIFIER =\n")
        f.write(f"        128'h{identifier.hex()};\n\n")

        f.write(f"    localparam logic [WIDTH-1:0] MASTER_SEED =\n")
        f.write(f"        256'h{master_seed.hex()};\n\n")

        f.write(f"    localparam logic [WIDTH-1:0] ROOT =\n")
        f.write(f"        256'h{root.hex()};\n\n")

        # Tree nodes array (1-based, index 0 unused)
        f.write(f"    // Tree nodes: index 1 = root, {num_leaves}..{2*num_leaves-1} = leaves\n")
        f.write(f"    localparam logic [WIDTH-1:0] TREE [{total_nodes}] = '{{\n")
        f.write(f"        // index 0 unused (default: '0)\n")
        for i in range(1, total_nodes):
            label = ""
            if i == 1:
                label = "  // root"
            elif i >= num_leaves:
                label = f"  // leaf {i - num_leaves}"
            f.write(f"        {i:2d}: 256'h{nodes[i].hex()},{label}\n")
        f.write(f"        default: '0\n")
        f.write("    };\n\n")

        f.write("endpackage\n")

    print(f"Generated {output_path}")
    print(f"  {height}-level tree, {num_leaves} leaves, {2*num_leaves-1} nodes")


if __name__ == "__main__":
    import sys

    # Fixed parameters (must match hss_pkg.sv)
    identifier = b'\x01' * 16
    master_seed = b'\xAB' * 32
    height = 4

    vectors = generate_test_vectors()

    # emit SV tree package
    emit_sv_tree_pkg(identifier, master_seed, height, "verilog/tb/tb_hss_tree_pkg.sv")

    # Save JSON
    with open("test/test_vectors_lms.json", "w") as f:
        json.dump(vectors, f, indent=2)
    print("Saved test_vectors_lms.json")

    print()
    print_ocaml_vectors(vectors)

    print()
    print(f"Signature chains: {P}")
    print(f"Avg hashes per verification: {P * MAX_COEF // 2}")
    print(f"Digits: {vectors['digits'][:5]}... (first 5)")
    print(f"Checksum digits: {vectors['digits'][P1:]}")
