open Base

let () =
  Stdio.printf "=== Point Addition Debug Test ===\n\n";
  
  let prime_p = Arith.Config.prime_p in
  
  (* secp256k1: y² = x³ + 7, so a = 0, b = 7, b3 = 21 *)
  let param_a = Z.zero in
  let param_b3 = Z.of_int 21 in
  
  (* Generator point G *)
  let g_x = Z.of_string_base 16 "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" in
  let g_y = Z.of_string_base 16 "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8" in
  
  let mod_add a b = Z.((a + b) mod prime_p) in
  let mod_sub a b = Z.(erem (a - b) prime_p) in
  let mod_mul a b = Z.((a * b) mod prime_p) in
  let mod_inv a = Z.invert a prime_p in
  
  (* Execute the algorithm manually *)
  let x1, y1, z1 = g_x, g_y, Z.one in
  let x2, y2, z2 = g_x, g_y, Z.one in  (* G + G *)
  let a = param_a in
  let b3 = param_b3 in
  
  Stdio.printf "Inputs:\n";
  Stdio.printf "  X1 = %s\n" (Z.to_string x1);
  Stdio.printf "  Y1 = %s\n" (Z.to_string y1);
  Stdio.printf "  Z1 = %s\n" (Z.to_string z1);
  Stdio.printf "  X2 = %s\n" (Z.to_string x2);
  Stdio.printf "  Y2 = %s\n" (Z.to_string y2);
  Stdio.printf "  Z2 = %s\n" (Z.to_string z2);
  Stdio.printf "  a  = %s\n" (Z.to_string a);
  Stdio.printf "  b3 = %s\n\n" (Z.to_string b3);
  
  (* Step 1-3 *)
  let t0 = mod_mul x1 x2 in
  let t1 = mod_mul y1 y2 in
  let t2 = mod_mul z1 z2 in
  Stdio.printf "Step 1: t0 = X1*X2 = %s\n" (Z.to_string t0);
  Stdio.printf "Step 2: t1 = Y1*Y2 = %s\n" (Z.to_string t1);
  Stdio.printf "Step 3: t2 = Z1*Z2 = %s\n" (Z.to_string t2);
  
  (* Step 4-8 *)
  let t3 = mod_add x1 y1 in
  let t4 = mod_add x2 y2 in
  let t3 = mod_mul t3 t4 in
  let t4 = mod_add t0 t1 in
  let t3 = mod_sub t3 t4 in
  Stdio.printf "Step 4-8: t3 = %s\n" (Z.to_string t3);
  
  (* Step 9-13 *)
  let t4 = mod_add x1 z1 in
  let t5 = mod_add x2 z2 in
  let t4 = mod_mul t4 t5 in
  let t5 = mod_add t0 t2 in
  let t4 = mod_sub t4 t5 in
  Stdio.printf "Step 9-13: t4 = %s\n" (Z.to_string t4);
  
  (* Step 14-18 *)
  let t5 = mod_add y1 z1 in
  let x3 = mod_add y2 z2 in
  let t5 = mod_mul t5 x3 in
  let x3 = mod_add t1 t2 in
  let t5 = mod_sub t5 x3 in
  Stdio.printf "Step 14-18: t5 = %s\n" (Z.to_string t5);
  
  (* Step 19-24 *)
  let z3 = mod_mul a t4 in
  let x3 = mod_mul b3 t2 in
  let z3 = mod_add x3 z3 in
  let x3 = mod_sub t1 z3 in
  let z3 = mod_add t1 z3 in
  let y3 = mod_mul x3 z3 in
  Stdio.printf "Step 19-24: x3=%s, y3=%s, z3=%s\n" 
    (Z.to_string x3) (Z.to_string y3) (Z.to_string z3);
  
  (* Step 25-29 *)
  let t1 = mod_add t0 t0 in
  let t1 = mod_add t1 t0 in
  let t2 = mod_mul a t2 in
  let t4 = mod_mul b3 t4 in
  let t1 = mod_add t1 t2 in
  Stdio.printf "Step 25-29: t1=%s, t2=%s, t4=%s\n" 
    (Z.to_string t1) (Z.to_string t2) (Z.to_string t4);
  
  (* Step 30-34 *)
  let t2 = mod_sub t0 t2 in
  let t2 = mod_mul a t2 in
  let t4 = mod_add t4 t2 in
  let t0 = mod_mul t1 t4 in
  let y3 = mod_add y3 t0 in
  Stdio.printf "Step 30-34: t0=%s, t2=%s, t4=%s, y3=%s\n" 
    (Z.to_string t0) (Z.to_string t2) (Z.to_string t4) (Z.to_string y3);
  
  (* Step 35-40 *)
  let t0 = mod_mul t5 t4 in
  let x3 = mod_mul t3 x3 in
  let x3 = mod_sub x3 t0 in
  let t0 = mod_mul t3 t1 in
  let z3 = mod_mul t5 z3 in
  let z3 = mod_add z3 t0 in
  Stdio.printf "Step 35-40: x3=%s, y3=%s, z3=%s\n" 
    (Z.to_string x3) (Z.to_string y3) (Z.to_string z3);
  
  (* Convert to affine *)
  Stdio.printf "\nFinal projective result:\n";
  Stdio.printf "  X3 = %s\n" (Z.to_string x3);
  Stdio.printf "  Y3 = %s\n" (Z.to_string y3);
  Stdio.printf "  Z3 = %s\n" (Z.to_string z3);
  
  if not (Z.equal z3 Z.zero) then begin
    let z3_inv = mod_inv z3 in
    let aff_x = mod_mul x3 z3_inv in
    let aff_y = mod_mul y3 z3_inv in
    Stdio.printf "\nAffine result (X3/Z3, Y3/Z3):\n";
    Stdio.printf "  x = %s\n" (Z.to_string aff_x);
    Stdio.printf "  y = %s\n" (Z.to_string aff_y);
    
    (* Expected 2G *)
    let lambda = mod_mul 
      (mod_mul (Z.of_int 3) (mod_mul g_x g_x)) 
      (mod_inv (mod_mul (Z.of_int 2) g_y)) in
    let exp_x = mod_sub (mod_mul lambda lambda) (mod_add g_x g_x) in
    let exp_y = mod_sub (mod_mul lambda (mod_sub g_x exp_x)) g_y in
    Stdio.printf "\nExpected 2G (affine doubling formula):\n";
    Stdio.printf "  x = %s\n" (Z.to_string exp_x);
    Stdio.printf "  y = %s\n" (Z.to_string exp_y);
    
    if Z.equal aff_x exp_x && Z.equal aff_y exp_y then
      Stdio.printf "\n✓ Algorithm produces correct result!\n"
    else
      Stdio.printf "\n✗ Algorithm result doesn't match expected!\n"
  end else
    Stdio.printf "\nResult is point at infinity (Z3 = 0)\n"