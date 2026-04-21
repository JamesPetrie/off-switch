package ecdsa_pkg;

    import arith_pkg::*;

    // -------------------------------------------------------------------------
    // Coordinate system constants
    // -------------------------------------------------------------------------

    // Affine coordinate Z value is fixed (on affine -> projective conversion)
    localparam logic [WIDTH-1:0] AFFINE_Z = 1;

    // -------------------------------------------------------------------------
    // secp256k1 constants
    // -------------------------------------------------------------------------

    // field prime: p = 2^256 - 2^32 - 977
    // factored out 2**32 to avoid 2**256 overflow
    parameter logic [WIDTH-1:0] PRIME_P =
        256'd2**32 * (256'd2**224 - 256'd1) - 256'd977;

    // scurve order n (no closed form exists)
    parameter logic [WIDTH-1:0] PRIME_N =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE_BAAEDCE6AF48A03BBFD25E8CD0364141;

    // curve parameters: y² = x³ + ax + b, a=0, b=7
    parameter logic [WIDTH-1:0] CURVE_A1 = 1 * 0;  // 1*a
    parameter logic [WIDTH-1:0] CURVE_B3 = 3 * 7;  // 3*b

    // Generator point G
    parameter logic [WIDTH-1:0]
    G_X = 256'h79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
    G_Y = 256'h483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8,
    G_Z = AFFINE_Z;

    // -------------------------------------------------------------------------
    // ECDSA public key format
    // -------------------------------------------------------------------------

    typedef struct packed {
        logic [WIDTH-1:0] q_x;
        logic [WIDTH-1:0] q_y;
        logic [WIDTH-1:0] q_z;
        // Extended to also contain G + Q (Assuming G + Q calculated with pk Q)
        logic [WIDTH-1:0] gpq_x;
        logic [WIDTH-1:0] gpq_y;
        logic [WIDTH-1:0] gpq_z;
    } pubkey_t;

    parameter int NUM_SIGNERS = 2;

    parameter pubkey_t PUBKEYS [NUM_SIGNERS] = '{
        // Signer 0 (d=2)
        0: '{q_x:    256'hc6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5,
             q_y:    256'h1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a,
             q_z:    AFFINE_Z,
             gpq_x:  256'hf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9,
             gpq_y:  256'h388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672,
             gpq_z:  AFFINE_Z},
        // Signer 1 (d=3)
        // Note: [1].q = [0].gpq (3Q = Q + 2Q)
        1: '{q_x:    256'hf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9,
             q_y:    256'h388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672,
             q_z:    AFFINE_Z,
             gpq_x:  256'he493dbf1c10d80f3581e4904930b1404cc6c13900ee0758474fa94abe8c4cd13,
             gpq_y:  256'h51ed993ea0d455b75642e2098ea51448d967ae33bfbdfe40cfe97bdc47739922,
             gpq_z:  AFFINE_Z}
    };

    // -------------------------------------------------------------------------
    // ECDSA license format
    // -------------------------------------------------------------------------

    typedef struct packed {
        logic [WIDTH-1:0] r;
        logic [WIDTH-1:0] s;
    } license_t;

endpackage
