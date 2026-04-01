// Modular arithmetic utilities for testbenches (simulation only).
//
// All functions operate on WIDTH-bit unsigned integers modulo a prime m.
// Not synthesisable — uses full-width multiply and Fermat's little theorem.

package tb_math_pkg;

    import arith_pkg::*;

    localparam int W2 = 2 * WIDTH;

    // (a + b) mod m
    function automatic logic [WIDTH-1:0] mod_add(
        input logic [WIDTH-1:0] a, b, m
    );
        logic [WIDTH:0] sum = {1'b0, a} + {1'b0, b};
        return WIDTH'(sum >= {1'b0, m} ? sum - {1'b0, m} : sum);
    endfunction

    // (a - b) mod m
    function automatic logic [WIDTH-1:0] mod_sub(
        input logic [WIDTH-1:0] a, b, m
    );
        return a >= b ? a - b : m - (b - a);
    endfunction

    // (a * b) mod m
    function automatic logic [WIDTH-1:0] mod_mul(
        input logic [WIDTH-1:0] a, b, m
    );
        logic [W2-1:0] product = W2'(a) * W2'(b);
        return WIDTH'(product % W2'(m));
    endfunction

    // a^(-1) mod m  — Fermat's little theorem: a^(m-2) mod m
    function automatic logic [WIDTH-1:0] mod_inv(
        input logic [WIDTH-1:0] a, m
    );
        logic [WIDTH-1:0] exp    = m - 2;
        logic [WIDTH-1:0] result = 1;
        logic [WIDTH-1:0] base   = a;

        for (int i = 0; i < WIDTH; i++) begin
            if (exp[i])
                result = mod_mul(result, base, m);
            base = mod_mul(base, base, m);
        end

        return result;
    endfunction

    // -------------------------------------------------------------------------
    // secp256k1 constants
    // -------------------------------------------------------------------------

    localparam logic [WIDTH-1:0] SECP256K1_P =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F;
    localparam logic [WIDTH-1:0] SECP256K1_N =
        256'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141;
    localparam logic [WIDTH-1:0] SECP256K1_GX =
        256'h79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798;
    localparam logic [WIDTH-1:0] SECP256K1_GY =
        256'h483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8;

    // -------------------------------------------------------------------------
    // EC point type (affine).  valid=0 means point at infinity.
    // -------------------------------------------------------------------------

    typedef struct packed {
        logic             valid;
        logic [WIDTH-1:0] x;
        logic [WIDTH-1:0] y;
    } ec_point_t;

    localparam ec_point_t EC_INF = '{valid: 1'b0, x: '0, y: '0};
    localparam ec_point_t EC_G   = '{valid: 1'b1, x: SECP256K1_GX, y: SECP256K1_GY};

    // -------------------------------------------------------------------------
    // Affine point addition over secp256k1 (mod p)
    // -------------------------------------------------------------------------

    function automatic ec_point_t ec_add(input ec_point_t p1, input ec_point_t p2);
        logic [WIDTH-1:0] p, lam, x3, y3;
        p = SECP256K1_P;

        if (!p1.valid) return p2;
        if (!p2.valid) return p1;

        if (p1.x == p2.x) begin
            if (p1.y == p2.y) begin
                // Doubling: λ = 3x₁² / 2y₁
                lam = mod_mul(mod_mul(256'd3, mod_mul(p1.x, p1.x, p), p),
                              mod_inv(mod_add(p1.y, p1.y, p), p), p);
            end else begin
                return EC_INF;
            end
        end else begin
            // General: λ = (y₂ - y₁) / (x₂ - x₁)
            lam = mod_mul(mod_sub(p2.y, p1.y, p), mod_inv(mod_sub(p2.x, p1.x, p), p), p);
        end

        x3 = mod_sub(mod_sub(mod_mul(lam, lam, p), p1.x, p), p2.x, p);
        y3 = mod_sub(mod_mul(lam, mod_sub(p1.x, x3, p), p), p1.y, p);
        return '{valid: 1'b1, x: x3, y: y3};
    endfunction

    // -------------------------------------------------------------------------
    // Scalar multiplication: k * P  (double-and-add)
    // -------------------------------------------------------------------------

    function automatic ec_point_t ec_mul(input logic [WIDTH-1:0] k, input ec_point_t pt);
        ec_point_t acc = EC_INF;
        ec_point_t cur = pt;

        for (int i = 0; i < WIDTH; i++) begin
            if (k[i])
                acc = ec_add(acc, cur);
            cur = ec_add(cur, cur);
        end

        return acc;
    endfunction

    // -------------------------------------------------------------------------
    // ECDSA signature type and sign function
    // -------------------------------------------------------------------------

    typedef struct packed {
        logic [WIDTH-1:0] r;
        logic [WIDTH-1:0] s;
    } ecdsa_sig_t;

    // Sign message hash z with private key d and nonce k
    function automatic ecdsa_sig_t ecdsa_sign(
        input logic [WIDTH-1:0] z, d, k
    );
        ec_point_t kg;
        logic [WIDTH-1:0] n, r_val, k_inv, s_val;

        n   = SECP256K1_N;
        kg  = ec_mul(k, EC_G);
        r_val = kg.x % n;
        k_inv = mod_inv(k, n);
        s_val = mod_mul(k_inv, mod_add(z, mod_mul(r_val, d, n), n), n);

        return '{r: r_val, s: s_val};
    endfunction

endpackage
