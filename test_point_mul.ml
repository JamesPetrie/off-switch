open Base
open Hardcaml

let () =
  Stdio.printf "=== Scalar Multiplication Unit Test ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
 let module Sim = Cyclesim.With_interface(Point_mul.I)(Point_mul.O) in
 let sim = Sim.create (Point_mul.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let prime_p = Arith.Config.prime_p in
  
  (* secp256k1 curve parameters: y² = x³ + 7, so a = 0, b = 7, b3 = 21 *)
  let param_a = Z.zero in
  let param_b3 = Z.of_int 21 in
  
  (* Generator point G for secp256k1 - must match Config *)
  let g_x = Z.of_string_base 16 "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" in
  let g_y = Z.of_string_base 16 "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8" in
  
  (* Curve order n for secp256k1 *)
  let curve_order = Z.of_string_base 16 "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141" in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in
  
  let bits_to_z bits = Z.of_string_base 2 (Bits.to_bstr bits) in
  
  (* Modular arithmetic helpers *)
  let mod_add a b = Z.((a + b) mod prime_p) in
  let mod_sub a b = Z.(erem (a - b) prime_p) in
  let mod_mul a b = Z.((a * b) mod prime_p) in
  let mod_inv a = Z.invert a prime_p in
  
  (* Convert projective (X:Y:Z) to affine (x, y) *)
  let proj_to_affine x y z =
    if Z.equal z Z.zero then
      None  (* Point at infinity *)
    else
      let z_inv = mod_inv z in
      let aff_x = mod_mul x z_inv in
      let aff_y = mod_mul y z_inv in
      Some (aff_x, aff_y)
  in
  
  (* Check if projective point equals affine point *)
  let proj_equals_affine ~proj_x ~proj_y ~proj_z ~aff_x ~aff_y =
    match proj_to_affine proj_x proj_y proj_z with
    | None -> false
    | Some (x, y) -> Z.equal x aff_x && Z.equal y aff_y
  in
  
  (* Reference implementation: affine point addition *)
  let affine_add (x1, y1) (x2, y2) =
    if Z.equal x1 x2 then begin
      if Z.equal y1 y2 then
        (* Point doubling *)
        let lambda = mod_mul (mod_mul (Z.of_int 3) (mod_mul x1 x1)) (mod_inv (mod_mul (Z.of_int 2) y1)) in
        let x3 = mod_sub (mod_mul lambda lambda) (mod_add x1 x2) in
        let y3 = mod_sub (mod_mul lambda (mod_sub x1 x3)) y1 in
        Some (x3, y3)
      else
        (* P + (-P) = O *)
        None
    end else begin
      let lambda = mod_mul (mod_sub y2 y1) (mod_inv (mod_sub x2 x1)) in
      let x3 = mod_sub (mod_sub (mod_mul lambda lambda) x1) x2 in
      let y3 = mod_sub (mod_mul lambda (mod_sub x1 x3)) y1 in
      Some (x3, y3)
    end
  in
  
  (* Reference scalar multiplication using double-and-add *)
  let scalar_mult k (x, y) =
    let rec loop n acc pt =
      if Z.equal n Z.zero then acc
      else
        let acc' = 
          if Z.(equal (n land one) one) then
            match acc with
            | None -> Some pt
            | Some a -> affine_add a pt
          else acc
        in
        let pt' = 
          match affine_add pt pt with
          | None -> pt
          | Some p -> p
        in
        loop Z.(n asr 1) acc' pt'
    in
    loop k None (x, y)
  in
  
  let reset () =
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.scalar := Bits.zero 256;
    inputs.param_a := Bits.zero 256;
    inputs.param_b3 := Bits.zero 256;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in
  
  let run_scalar_mult ~scalar =
    reset ();
    
    inputs.scalar := z_to_bits scalar;
    inputs.param_a := z_to_bits param_a;
    inputs.param_b3 := z_to_bits param_b3;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    
    let max_cycles = 500000 in
    let rec wait n =
      if n >= max_cycles then begin
        Stdio.printf "  TIMEOUT after %d cycles\n" max_cycles;
        None
      end else if Bits.to_bool !(outputs.done_) then begin
        let result_x = bits_to_z !(outputs.x) in
        let result_y = bits_to_z !(outputs.y) in
        let result_z = bits_to_z !(outputs.z) in
        Stdio.printf "  Completed in %d cycles\n" n;
        Some (result_x, result_y, result_z)
      end else begin
        Cyclesim.cycle sim;
        wait (n + 1)
      end
    in
    wait 0
  in
  
  let results = ref [] in
  let record result = results := result :: !results in
  
  (* ============================================== *)
  (* TEST 1: [1]G = G                              *)
  (* ============================================== *)
  
  Stdio.printf "Test 1: [1]G = G\n";
  Stdio.printf "  Scalar = 1\n";
  
  (match run_scalar_mult ~scalar:Z.one with
  | None -> record false
  | Some (rx, ry, rz) ->
      Stdio.printf "  Result (projective): X=%s..., Z=%s...\n"
        (String.prefix (Z.to_string rx) 20)
        (String.prefix (Z.to_string rz) 20);
      let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:g_x ~aff_y:g_y in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 2: [2]G = 2G                             *)
  (* ============================================== *)
  
  Stdio.printf "Test 2: [2]G = 2G\n";
  Stdio.printf "  Scalar = 2\n";
  
  let expected_2g = scalar_mult (Z.of_int 2) (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:(Z.of_int 2) with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_2g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected (affine): x=%s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 3: [3]G = 3G                             *)
  (* ============================================== *)
  
  Stdio.printf "Test 3: [3]G = 3G\n";
  Stdio.printf "  Scalar = 3 (binary: 11)\n";
  
  let expected_3g = scalar_mult (Z.of_int 3) (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:(Z.of_int 3) with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_3g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected 3G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 4: [7]G = 7G                             *)
  (* ============================================== *)
  
  Stdio.printf "Test 4: [7]G = 7G\n";
  Stdio.printf "  Scalar = 7 (binary: 111)\n";
  
  let expected_7g = scalar_mult (Z.of_int 7) (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:(Z.of_int 7) with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_7g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected 7G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 5: [10]G = 10G                           *)
  (* ============================================== *)
  
  Stdio.printf "Test 5: [10]G = 10G\n";
  Stdio.printf "  Scalar = 10 (binary: 1010)\n";
  
  let expected_10g = scalar_mult (Z.of_int 10) (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:(Z.of_int 10) with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_10g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected 10G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 6: [100]G = 100G                         *)
  (* ============================================== *)
  
  Stdio.printf "Test 6: [100]G = 100G\n";
  Stdio.printf "  Scalar = 100 (binary: 1100100)\n";
  
  let expected_100g = scalar_mult (Z.of_int 100) (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:(Z.of_int 100) with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_100g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected 100G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 7: [255]G = 255G                         *)
  (* ============================================== *)
  
  Stdio.printf "Test 7: [255]G = 255G\n";
  Stdio.printf "  Scalar = 255 (binary: 11111111)\n";
  
  let expected_255g = scalar_mult (Z.of_int 255) (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:(Z.of_int 255) with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_255g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected 255G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 8: Power of 2 - [256]G                   *)
  (* ============================================== *)
  
  Stdio.printf "Test 8: [256]G = 256G\n";
  Stdio.printf "  Scalar = 256 (binary: 100000000)\n";
  
  let expected_256g = scalar_mult (Z.of_int 256) (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:(Z.of_int 256) with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_256g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected 256G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 9: Large power of 2 - [2^16]G            *)
  (* ============================================== *)
  
  Stdio.printf "Test 9: [2^16]G = 65536G\n";
  Stdio.printf "  Scalar = 65536\n";
  
  let scalar_2_16 = Z.shift_left Z.one 16 in
  let expected_2_16_g = scalar_mult scalar_2_16 (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_2_16 with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_2_16_g with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 10: Sparse bits - [2^100 + 1]G           *)
  (* ============================================== *)
  
  Stdio.printf "Test 10: [2^100 + 1]G\n";
  Stdio.printf "  Scalar has bits set at positions 0 and 100 only\n";
  
  let scalar_sparse = Z.(shift_left one 100 + one) in
  let expected_sparse = scalar_mult scalar_sparse (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_sparse with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_sparse with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 11: Dense bits in low byte               *)
  (* ============================================== *)
  
  Stdio.printf "Test 11: [0xFF]G = 255G (all low bits set)\n";
  
  let scalar_ff = Z.of_int 0xFF in
  let expected_ff = scalar_mult scalar_ff (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_ff with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_ff with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 12: Alternating bits pattern             *)
  (* ============================================== *)
  
  Stdio.printf "Test 12: [0xAAAA]G (alternating bits: 1010...)\n";
  
  let scalar_aaaa = Z.of_int 0xAAAA in
  let expected_aaaa = scalar_mult scalar_aaaa (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_aaaa with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_aaaa with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 13: Known ECDSA-like scalar              *)
  (* ============================================== *)
  
  Stdio.printf "Test 13: Large random-looking scalar\n";
  
  let scalar_large = Z.of_string_base 16 "DEADBEEFCAFEBABE0123456789ABCDEF" in
  Stdio.printf "  Scalar = 0x%s\n" (Z.format "%X" scalar_large);
  
  let expected_large = scalar_mult scalar_large (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_large with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_large with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 14: 128-bit scalar                       *)
  (* ============================================== *)
  
  Stdio.printf "Test 14: 128-bit scalar\n";
  
  let scalar_128 = Z.of_string_base 16 "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" in
  Stdio.printf "  Scalar = 2^128 - 1\n";
  
  let expected_128 = scalar_mult scalar_128 (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_128 with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_128 with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 15: Full 256-bit scalar (max value)      *)
  (* ============================================== *)
  
  Stdio.printf "Test 15: Full 256-bit scalar (2^256 - 1)\n";
  
  let scalar_max = Z.(shift_left one 256 - one) in
  Stdio.printf "  Scalar = 2^256 - 1\n";
  
  let expected_max = scalar_mult scalar_max (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_max with
  | None -> record false
  | Some (rx, ry, rz) ->
      match expected_max with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected x = %s...\n" (String.prefix (Z.to_string ex) 30);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 16: [n-1]G where n is curve order        *)
  (* ============================================== *)
  
  Stdio.printf "Test 16: [n-1]G = -G (one less than curve order)\n";
  
  let scalar_n_minus_1 = Z.(curve_order - one) in
  Stdio.printf "  Scalar = n - 1\n";
  
  (* [n-1]G = -G, which has same x as G but negated y *)
  let expected_neg_g_y = mod_sub Z.zero g_y in
  
  (match run_scalar_mult ~scalar:scalar_n_minus_1 with
  | None -> record false
  | Some (rx, ry, rz) ->
      Stdio.printf "  Result x = %s...\n" (String.prefix (Z.to_string rx) 30);
      let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:g_x ~aff_y:expected_neg_g_y in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 17: [n]G = O (curve order gives infinity)*)
  (* ============================================== *)
  
  Stdio.printf "Test 17: [n]G = O (curve order gives point at infinity)\n";
  
  Stdio.printf "  Scalar = n (curve order)\n";
  
  (match run_scalar_mult ~scalar:curve_order with
  | None -> record false
  | Some (_rx, _ry, rz) ->
      Stdio.printf "  Result Z = %s\n" (Z.to_string rz);
      let pass = Z.equal rz Z.zero in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 18: [0]G = O (zero scalar)               *)
  (* ============================================== *)
  
  Stdio.printf "Test 18: [0]G = O (zero scalar gives point at infinity)\n";
  
  (match run_scalar_mult ~scalar:Z.zero with
  | None -> record false
  | Some (_rx, _ry, rz) ->
      Stdio.printf "  Result Z = %s\n" (Z.to_string rz);
      let pass = Z.equal rz Z.zero in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 19: Repeated scalar multiplications      *)
  (* ============================================== *)
  
  Stdio.printf "Test 19: Repeated scalar multiplications (consistency check)\n";
  
  let repeated_pass = ref true in
  let test_scalars = [5; 17; 42; 100; 1000] in
  
  List.iter test_scalars ~f:(fun s ->
    let scalar = Z.of_int s in
    let expected = scalar_mult scalar (g_x, g_y) in
    
    match run_scalar_mult ~scalar with
    | None -> 
        Stdio.printf "  [%d]G: TIMEOUT\n" s;
        repeated_pass := false
    | Some (rx, ry, rz) ->
        match expected with
        | None ->
            if not (Z.equal rz Z.zero) then begin
              Stdio.printf "  [%d]G: expected infinity, got finite point\n" s;
              repeated_pass := false
            end else
              Stdio.printf "  [%d]G: OK (infinity)\n" s
        | Some (ex, ey) ->
            if proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey then
              Stdio.printf "  [%d]G: OK\n" s
            else begin
              Stdio.printf "  [%d]G: MISMATCH\n" s;
              repeated_pass := false
            end);
  
  Stdio.printf "  %s\n\n" (if !repeated_pass then "PASS ✓" else "FAIL ✗");
  record !repeated_pass;
  
  (* ============================================== *)
  (* TEST 20: Bit pattern edge cases               *)
  (* ============================================== *)
  
  Stdio.printf "Test 20: Bit pattern edge cases\n";
  
  let edge_pass = ref true in
  
  (* Single high bit *)
  let scalar_high = Z.shift_left Z.one 255 in
  Stdio.printf "  Testing 2^255 (single high bit)...\n";
  let expected_high = scalar_mult scalar_high (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_high with
  | None -> 
      Stdio.printf "    TIMEOUT\n";
      edge_pass := false
  | Some (rx, ry, rz) ->
      match expected_high with
      | None ->
          if not (Z.equal rz Z.zero) then edge_pass := false
      | Some (ex, ey) ->
          if not (proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey) then
            edge_pass := false);
  
  (* High bit and low bit *)
  let scalar_high_low = Z.(shift_left one 255 + one) in
  Stdio.printf "  Testing 2^255 + 1 (high and low bits)...\n";
  let expected_high_low = scalar_mult scalar_high_low (g_x, g_y) in
  
  (match run_scalar_mult ~scalar:scalar_high_low with
  | None -> 
      Stdio.printf "    TIMEOUT\n";
      edge_pass := false
  | Some (rx, ry, rz) ->
      match expected_high_low with
      | None ->
          if not (Z.equal rz Z.zero) then edge_pass := false
      | Some (ex, ey) ->
          if not (proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey) then
            edge_pass := false);
  
  Stdio.printf "  %s\n\n" (if !edge_pass then "PASS ✓" else "FAIL ✗");
  record !edge_pass;
  
  (* ============================================== *)
  (* TEST SUMMARY                                  *)
  (* ============================================== *)
  
  let results_list = List.rev !results in
  let passed = List.count results_list ~f:Fn.id in
  let total = List.length results_list in
  
  Stdio.printf "=== Test Summary ===\n";
  Stdio.printf "Passed: %d/%d\n" passed total;
  
  if passed = total then begin
    Stdio.printf "\n";
    Stdio.printf "███████╗██╗   ██╗ ██████╗ ██████╗███████╗███████╗███████╗\n";
    Stdio.printf "██╔════╝██║   ██║██╔════╝██╔════╝██╔════╝██╔════╝██╔════╝\n";
    Stdio.printf "███████╗██║   ██║██║     ██║     █████╗  ███████╗███████╗\n";
    Stdio.printf "╚════██║██║   ██║██║     ██║     ██╔══╝  ╚════██║╚════██║\n";
    Stdio.printf "███████║╚██████╔╝╚██████╗╚██████╗███████║███████║███████║\n";
    Stdio.printf "╚══════╝ ╚═════╝  ╚═════╝ ╚═════╝╚══════╝╚══════╝╚══════╝\n";
    Stdio.printf "\nAll scalar multiplication tests passed! ✓✓✓\n"
  end else
    Stdio.printf "\n✗ Some tests failed - review above for details\n"