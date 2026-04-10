package secp256k1_pkg;

    import arith_pkg::*;

    // -------------------------------------------------------------------------
    // secp256k1 constants
    // -------------------------------------------------------------------------

    // field prime: p = 2^256 - 2^32 - 977
    // factored out 2**32 to avoid 2**256 overflow
    localparam logic [WIDTH-1:0] PRIME_P =
        256'd2**32 * (256'd2**224 - 256'd1) - 256'd977;

    // scurve order n (no closed form exists)
    localparam logic [WIDTH-1:0] PRIME_N =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE_BAAEDCE6AF48A03BBFD25E8CD0364141;

    // curve parameters: y² = x³ + ax + b, a=0, b=7
    localparam logic [WIDTH-1:0] CURVE_A1 = 1 * 0;  // 1*a
    localparam logic [WIDTH-1:0] CURVE_B3 = 3 * 7;  // 3*b

    // Generator point G
    localparam logic [WIDTH-1:0]
    G_X = 256'h79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
    G_Y = 256'h483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8,
    G_Z = 1; // projective coordinate for affine points have Z=1

endpackage
