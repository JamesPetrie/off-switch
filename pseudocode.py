# Domain parameters (per NIST):
# p: field size (prime modulus for the field)
# n: order of generator G (prime)
# a, b: curve equation coefficients (y² = x³ + ax + b)
# G: generator point (base point) = (xG, yG)
# Q: public key point = (xQ, yQ)
# h: cofactor (not used in this implementation)

def verify(e, r, s) -> bool:
    """ECDSA signature verification (FIPS 186-4)
    e: message hash 
    r, s: signature components
    Returns: True if signature is valid, False otherwise
    """
    
    # Check that r and s are in valid range [1, n-1]
    if r >= n or s >= n or r < 1 or s < 1:
        return False
    
    # Check that e is in valid range [0, n-1]
    if e >= n or e < 0:
        return False
    
    # Compute w = s^(-1) mod n
    w = modinv(s, n)
    
    # Compute scalar multipliers
    u1 = modmul(e, w, n)
    u2 = modmul(r, w, n)
    
    # Precompute Q + G for Shamir's trick
    # todo: precompute and check validity before compile time
    x_QplusG, y_QplusG = add(xQ, yQ, xG, yG)
    # Check that precomputed point is not infinity
    # (G, Q, and Q+G must all be valid for the algorithm)
    if y_QplusG == 0:
        return False
    
    # Initialize accumulator to point at infinity
    x_acc = 0
    y_acc = 0
    
    # Simultaneous scalar multiplication: u1*G + u2*Q
    # Using Shamir's trick with double-and-add
    # Process bits from most significant to least significant
    width = bitlength(n)
    
    for i in range(width - 1, -1, -1):
        # Double the accumulator (unless it's the point at infinity)
        if y_acc != 0:
            x_acc, y_acc = double(x_acc, y_acc)
        
        # Extract i-th bit of u1 and u2
        bit_u1 = (u1 >> i) & 1
        bit_u2 = (u2 >> i) & 1
        
        # Determine which point to add based on bit pattern
        if (bit_u1, bit_u2) == (0, 0):
            continue  # Add point at infinity (no-op)
        elif (bit_u1, bit_u2) == (1, 0):
            x_add, y_add = xG, yG
        elif (bit_u1, bit_u2) == (0, 1):
            x_add, y_add = xQ, yQ
        elif (bit_u1, bit_u2) == (1, 1):
            x_add, y_add = x_QplusG, y_QplusG
        
        # Add selected point to accumulator
        if y_acc == 0:
            # Accumulator is at infinity, so result is the point to add
            x_acc, y_acc = x_add, y_add
        elif x_acc == x_add:
            if modadd(y_acc, y_add, p) == 0:
                # Points are inverses, result is point at infinity
                x_acc, y_acc = 0, 0
            else:
                # Same point, use doubling
                x_acc, y_acc = double(x_acc, y_acc)
        else:
            # Different x-coordinates, use addition
            x_acc, y_acc = add(x_acc, y_acc, x_add, y_add)
    
    # Verify signature: check if x-coordinate of result equals r
    # Result must not be point at infinity
    if y_acc == 0:
        return False
    
    # Compare x_acc with r (both interpreted mod n per FIPS 186-4)
    return (modadd(x_acc, r, n, subtract = TRUE) == 0)


def add(xP, yP, xQ, yQ):
    """Point addition on elliptic curve
    Returns: (xR, yR) = P + Q
    """
    # Compute slope: λ = (yQ - yP) / (xQ - xP) mod p
    numerator = modadd(yQ, yP, p, subtract=True)
    denominator = modadd(xQ, xP, p, subtract=True)
    λ = modmul(numerator, modinv(denominator, p), p)
    
    # Compute result coordinates
    # xR = λ² - xP - xQ mod p
    λ_squared = modmul(λ, λ, p)
    xR = modadd(λ_squared, xP, p, subtract=True)
    xR = modadd(xR, xQ, p, subtract=True)
    
    # yR = λ(xP - xR) - yP mod p
    xP_minus_xR = modadd(xP, xR, p, subtract=True)
    yR = modmul(λ, xP_minus_xR, p)
    yR = modadd(yR, yP, p, subtract=True)
    
    return xR, yR


def double(xP, yP):
    """Point doubling on elliptic curve
    Returns: (xR, yR) = 2P
    """
    # Compute slope: λ = (3xP² + a) / (2yP) mod p
    xP_squared = modmul(xP, xP, p)
    numerator = modmul(xP_squared, 3, p)
    numerator = modadd(numerator, a, p)
    
    denominator = modmul(yP, 2, p)
    λ = modmul(numerator, modinv(denominator, p), p)
    
    # Compute result coordinates
    # xR = λ² - 2xP mod p
    λ_squared = modmul(λ, λ, p)
    two_xP = modmul(xP, 2, p)
    xR = modadd(λ_squared, two_xP, p, subtract=True)
    
    # yR = λ(xP - xR) - yP mod p
    xP_minus_xR = modadd(xP, xR, p, subtract=True)
    yR = modmul(λ, xP_minus_xR, p)
    yR = modadd(yR, yP, p, subtract=True)
    
    return xR, yR