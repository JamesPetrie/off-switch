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

# RFC 8554 §5.1 typecodes — used to serialise an LMS public key that an upper
# HSS layer signs. Must agree with hss_pkg.sv::LMS_TYPE / LMS_OTSTYPE.
LMS_TYPE    = 0x00000005   # lms_sha256_n32_h5
LMS_OTSTYPE = 0x00000004   # lmots_sha256_n32_w8

D_MESG = 0x8181


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


def serialise_lms_pub(identifier: bytes, root: bytes) -> bytes:
    """Serialise an LMS public key for signing by an upper HSS layer:
        pub = u32str(LMS_TYPE) || u32str(LMS_OTSTYPE) || I || T[1]
    """
    return (struct.pack(">I", LMS_TYPE) + struct.pack(">I", LMS_OTSTYPE)
            + identifier + root)


def build_layer(identifier: bytes, master_seed: bytes, height: int) -> dict:
    """Build a full LMS layer: all leaf Kc, leaf hashes, tree, root."""
    num_leaves = 1 << height
    all_Kc = [wots_keygen(identifier, q, master_seed)[2] for q in range(num_leaves)]
    leaf_hashes = [merkle_leaf_hash(identifier, i, all_Kc[i]) for i in range(num_leaves)]
    root, nodes = build_merkle_tree(identifier, leaf_hashes, height)
    return {
        "identifier":   identifier,
        "master_seed":  master_seed,
        "height":       height,
        "root":         root,
        "nodes":        nodes,
    }


def generate_test_vectors(identifier: bytes, seed: bytes, height: int):
    """Generate single-tree test vectors for the HardCaml / OCaml reference."""

    q = 5  # leaf index

    # Generate WOTS+ keypair
    sk, pk_elements, Kc = wots_keygen(identifier, q, seed)

    # Message to sign (the nonce in off-switch context)
    message = sha256(b"test message for off-switch")

    # Compute message hash Q per RFC 8554 Section 4.6
    # Q = H(I || u32str(q) || u16str(D_MESG) || C || message)
    C = os.urandom(32)  # randomizer
    C = b'\xCC' * 32  # deterministic for testing
    Q_input = identifier + struct.pack(">I", q) + struct.pack(">H", D_MESG) + C + message
    Q = sha256(Q_input)

    # Sign
    sig, digits = wots_sign(Q, sk, identifier, q)

    # Verify
    assert wots_verify(Q, sig, identifier, q, Kc), "Self-verification failed!"

    # Build Merkle tree (small)
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
        "tree_h": height,
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
    print(f'let tree_h = {v["tree_h"]}')
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


def build_signer_layers(identifiers: list, master_seeds: list, height: int):
    """Build all HSS layers for one signer's hypertree."""
    L = len(identifiers)
    assert len(master_seeds) == L
    return [build_layer(identifiers[lv], master_seeds[lv], height) for lv in range(L)]


def emit_sv_tree_pkg(signers: list, height: int, sub_leaf_indices: list,
                     output_path: str):
    """Emit a SystemVerilog package with per-signer × per-layer Merkle trees.

    `signers` is a list of dicts, one per signer:
        {"identifiers": [bytes, ...], "master_seeds": [bytes, ...]}
    where each list has length L (=HSS_LEVELS), with index 0 = top tree.

    sub_leaf_indices[lv]: upper-tree leaf that signs pub[lv+1], for lv in 0..L-2
    (shared across signers — keeps the upper-tree leaves sticky during signing).
    """

    num_signers = len(signers)
    L           = len(signers[0]["identifiers"])
    num_leaves  = 1 << height
    total_nodes = 2 * num_leaves  # 1-based: indices 1..(2*num_leaves-1)

    assert len(sub_leaf_indices) == L - 1, \
        f"expected {L-1} sub leaf indices for {L} layers"
    for s in signers:
        assert len(s["identifiers"])  == L
        assert len(s["master_seeds"]) == L

    # Build hypertree layers per signer
    all_layers = []
    for s_idx, s in enumerate(signers):
        layers = build_signer_layers(s["identifiers"], s["master_seeds"], height)

        # Self-check each layer with a few leaves
        for lv, layer in enumerate(layers):
            nodes = layer["nodes"]
            ident = layer["identifier"]
            root  = layer["root"]
            for test_q in [0, min(5, num_leaves - 1), num_leaves - 1]:
                path = merkle_auth_path(nodes, height, test_q)
                leaf_Kc = wots_keygen(ident, test_q, layer["master_seed"])[2]
                computed = merkle_leaf_hash(ident, test_q, leaf_Kc)
                idx = num_leaves + test_q
                for lvl in range(height):
                    parent = idx >> 1
                    sib = path[lvl]
                    if idx % 2 == 0:
                        computed = merkle_internal_hash(ident, parent, computed, sib)
                    else:
                        computed = merkle_internal_hash(ident, parent, sib, computed)
                    idx = parent
                assert computed == root, \
                    f"Signer {s_idx} layer {lv} self-check failed for leaf {test_q}"

        # Cross-layer self-check: full HSS sign+verify against the top root
        _check_hss_chain(layers, height, sub_leaf_indices, 0,
                         sha256(b"hss self-check message"))
        all_layers.append(layers)

    top_roots = [all_layers[s][0]["root"] for s in range(num_signers)]

    with open(output_path, "w") as f:
        f.write("// Auto-generated by reference_lms.py — do not edit\n")
        f.write(f"// HSS-LMS testbench: {num_signers}-signer × {L}-layer Merkle tree bundle\n\n")
        f.write("package tb_hss_tree_pkg;\n\n")
        f.write("    import arith_pkg::*;\n\n")
        f.write(f"    // Emitted for {num_signers} signer(s) × {L} layer(s); regenerate via\n")
        f.write("    // reference_lms.py if more signers or levels are needed.\n")
        f.write("    // (mismatch is caught at elaboration by the array-literal size.)\n\n")
        f.write(f"    localparam int unsigned NUM_SIGNERS = {num_signers};\n")
        f.write(f"    localparam int unsigned NUM_LAYERS  = {L};\n")
        f.write(f"    localparam int unsigned TREE_H      = {height};\n")
        f.write(f"    localparam int unsigned NUM_LEAVES  = 1 << TREE_H;\n")
        f.write(f"    localparam int unsigned NUM_NODES   = 2*NUM_LEAVES - 1;\n\n")

        # Per-signer × per-layer identifiers
        # Packed [NUM_LAYERS-1:0][127:0]: emit highest layer first for readability.
        f.write(f"    localparam logic [NUM_LAYERS-1:0][127:0] IDENTIFIERS [NUM_SIGNERS] = '{{\n")
        for s in range(num_signers):
            comma = "," if s < num_signers - 1 else ""
            f.write(f"        {s}: '{{  // signer {s}\n")
            for lv in range(L - 1, -1, -1):
                tail = "," if lv > 0 else ""
                f.write(f"            128'h{all_layers[s][lv]['identifier'].hex()}{tail}  // layer {lv}\n")
            f.write(f"        }}{comma}\n")
        f.write("    };\n\n")

        # Per-signer × per-layer master seeds
        f.write(f"    localparam logic [NUM_LAYERS-1:0][WIDTH-1:0] MASTER_SEEDS [NUM_SIGNERS] = '{{\n")
        for s in range(num_signers):
            comma = "," if s < num_signers - 1 else ""
            f.write(f"        {s}: '{{  // signer {s}\n")
            for lv in range(L - 1, -1, -1):
                tail = "," if lv > 0 else ""
                f.write(f"            256'h{all_layers[s][lv]['master_seed'].hex()}{tail}  // layer {lv}\n")
            f.write(f"        }}{comma}\n")
        f.write("    };\n\n")

        # Per-signer top-tree root (HSS anchor — must match hss_pkg::PUBKEYS)
        f.write(f"    localparam logic [WIDTH-1:0] ROOTS [NUM_SIGNERS] = '{{\n")
        for s in range(num_signers):
            comma = "," if s < num_signers - 1 else ""
            f.write(f"        {s}: 256'h{top_roots[s].hex()}{comma}  // signer {s} top root\n")
        f.write("    };\n\n")

        # Upper-tree leaf indices that sign pub of the next layer (shared across signers).
        # Sized [NUM_LAYERS] so the array is always present even at L=1
        # (index 0..L-2 is meaningful; index L-1 is an unused placeholder).
        f.write(f"    localparam int unsigned SUB_LEAF_INDEX [NUM_LAYERS] = '{{\n")
        for lv in range(L):
            tail = "," if lv < L - 1 else ""
            if lv < L - 1:
                label = f"  // layer {lv} leaf that signs pub[{lv+1}]"
                val = sub_leaf_indices[lv]
            else:
                label = "  // unused (bottom layer signs the user message)"
                val = 0
            f.write(f"        {val}{tail}{label}\n")
        f.write("    };\n\n")

        # Tree nodes — [NUM_SIGNERS][NUM_LAYERS][2*NUM_LEAVES]
        f.write(f"    // Tree nodes per signer × layer: index 1 = root, {num_leaves}..{2*num_leaves-1} = leaves\n")
        f.write(f"    localparam logic [WIDTH-1:0] TREE [NUM_SIGNERS][NUM_LAYERS][{total_nodes}] = '{{\n")
        for s in range(num_signers):
            f.write(f"        {s}: '{{  // signer {s}\n")
            for lv in range(L):
                f.write(f"            // ---- layer {lv} ----\n")
                f.write(f"            '{{\n")
                f.write(f"                // index 0 unused (default: '0)\n")
                for i in range(1, total_nodes):
                    label = ""
                    if i == 1:
                        label = "  // root"
                    elif i >= num_leaves:
                        label = f"  // leaf {i - num_leaves}"
                    f.write(f"                {i:2d}: 256'h{all_layers[s][lv]['nodes'][i].hex()},{label}\n")
                f.write(f"                default: '0\n")
                tail = "" if lv == L - 1 else ","
                f.write(f"            }}{tail}\n")
            comma = "," if s < num_signers - 1 else ""
            f.write(f"        }}{comma}\n")
        f.write("    };\n\n")

        f.write("endpackage\n")

    print(f"Generated {output_path}")
    print(f"  {num_signers} signer(s) × {L}-layer trees, height {height}, "
          f"{num_leaves} leaves/tree")
    for s in range(num_signers):
        print(f"  signer {s} top root: {top_roots[s].hex()}")


def _check_hss_chain(layers, height, sub_leaf_indices, bottom_leaf, message):
    """Simulate an HSS sign+verify across all layers to sanity-check the tree
    bundle we're about to emit. Bottom-up verify mirrors the RTL."""
    L = len(layers)

    # SIGN (top→down in generation, but stored in layer-indexed arrays).
    # For each layer, pick the active leaf:
    active_leaf = [None] * L
    active_leaf[L - 1] = bottom_leaf
    for lv in range(L - 1):
        active_leaf[lv] = sub_leaf_indices[lv]

    # For each layer, compute the signed payload:
    payloads = [None] * L
    payloads[L - 1] = message
    for lv in range(L - 1):
        payloads[lv] = serialise_lms_pub(layers[lv + 1]["identifier"],
                                         layers[lv + 1]["root"])

    # WOTS+ sign and generate auth paths
    sigs = []
    auth_paths = []
    for lv in range(L):
        ident = layers[lv]["identifier"]
        seed  = layers[lv]["master_seed"]
        q     = active_leaf[lv]
        # Compute Q hash
        C = bytes([q & 0xff] * 32)  # deterministic randomizer (match tb_hss_sign_pkg)
        Q_input = (ident + struct.pack(">I", q) + struct.pack(">H", D_MESG)
                   + C + payloads[lv])
        Q = sha256(Q_input)
        sk, _, _ = wots_keygen(ident, q, seed)
        sig, _ = wots_sign(Q, sk, ident, q)
        sigs.append(sig)
        auth_paths.append(merkle_auth_path(layers[lv]["nodes"], height, q))

    # VERIFY bottom-up (mirrors the RTL)
    prev_root = None
    for lv in reversed(range(L)):
        ident = layers[lv]["identifier"]
        q     = active_leaf[lv]
        # payload: either user message (bottom) or serialised pub built from
        # the previous layer's computed root
        if lv == L - 1:
            payload = message
        else:
            payload = serialise_lms_pub(layers[lv + 1]["identifier"], prev_root)
        C = bytes([q & 0xff] * 32)
        Q_input = (ident + struct.pack(">I", q) + struct.pack(">H", D_MESG)
                   + C + payload)
        Q = sha256(Q_input)
        # Recover pk candidate from signature
        all_digits = coefs(Q, W) + checksum(coefs(Q, W))
        pk_candidate = []
        for i in range(P):
            steps = MAX_COEF - all_digits[i]
            pk_i = chain(sigs[lv][i], 0, ident, q, i, all_digits[i], steps)
            pk_candidate.append(pk_i)
        # Kc
        msg = ident + struct.pack(">I", q) + struct.pack(">H", 0x8080)
        for pk_i in pk_candidate:
            msg += pk_i
        Kc = sha256(msg)
        # Leaf
        leaf = merkle_leaf_hash(ident, q, Kc)
        # Walk up
        node_idx = (1 << height) + q
        cur = leaf
        for lvl in range(height):
            parent = node_idx >> 1
            sib = auth_paths[lv][lvl]
            if node_idx % 2 == 0:
                cur = merkle_internal_hash(ident, parent, cur, sib)
            else:
                cur = merkle_internal_hash(ident, parent, sib, cur)
            node_idx = parent
        prev_root = cur

    assert prev_root == layers[0]["root"], \
        "HSS cross-layer self-check failed: computed top root != expected"


if __name__ == "__main__":
    import sys

    # Tree height (must match hss_pkg::TREE_H) and layer count (HSS_LEVELS).
    height     = 5
    NUM_LAYERS = 2

    # Per-signer × per-layer keys. Layer 0 = top, layer L-1 = bottom.
    # Each signer gets a distinct identifier/seed pattern so traces are
    # easy to disambiguate.
    #   signer 0: identifiers 0x01.., 0x02..    seeds 0xAB.., 0xAA..
    #   signer 1: identifiers 0x11.., 0x12..    seeds 0xCD.., 0xCC..
    signers = [
        {
            "identifiers":  [bytes([0x01 + lv] * 16) for lv in range(NUM_LAYERS)],
            "master_seeds": [bytes([0xAB - lv] * 32) for lv in range(NUM_LAYERS)],
        },
        {
            "identifiers":  [bytes([0x11 + lv] * 16) for lv in range(NUM_LAYERS)],
            "master_seeds": [bytes([0xCD - lv] * 32) for lv in range(NUM_LAYERS)],
        },
    ]

    # Pre-chosen leaf in layer lv that signs pub[lv+1]. Pick leaf 0 for
    # reproducibility and to keep the upper-tree leaf sticky across bottom
    # signings (tb_hss_sign_pkg only advances the bottom leaf).
    sub_leaf_indices = [0] * (NUM_LAYERS - 1)

    # Single-tree test vectors still derived from signer 0's top-layer key
    # (kept for the OCaml reference vectors).
    vectors = generate_test_vectors(signers[0]["identifiers"][0],
                                    signers[0]["master_seeds"][0], height)

    # emit SV tree package
    emit_sv_tree_pkg(signers, height, sub_leaf_indices,
                     "verilog/tb/tb_hss_tree_pkg.sv")

    # Save JSON (single-tree vectors, for OCaml reference)
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
