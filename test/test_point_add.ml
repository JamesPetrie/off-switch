open Base
open Hardcaml

let () =
  Stdio.printf "=== Point Addition Unit Test ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Point_add.I)(Point_add.O) in
  let sim = Sim.create (Point_add.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let prime_p = Arith.Config.prime_p in
  
  (* secp256k1 curve parameters: y² = x³ + 7, so a = 0, b = 7, b3 = 21 *)
  let param_a = Z.zero in
  let param_b3 = Z.of_int 21 in
  
  (* Generator point G for secp256k1 *)
  let g_x = Z.of_string_base 16 "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" in
  let g_y = Z.of_string_base 16 "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8" in
  
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
  
  (* Scalar multiplication using double-and-add *)
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
          | None -> pt  (* shouldn't happen for valid points *)
          | Some p -> p
        in
        loop Z.(n asr 1) acc' pt'
    in
    loop k None (x, y)
  in
  
  let reset () =
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.x1 := Bits.zero 256;
    inputs.y1 := Bits.zero 256;
    inputs.z1 := Bits.zero 256;
    inputs.x2 := Bits.zero 256;
    inputs.y2 := Bits.zero 256;
    inputs.z2 := Bits.zero 256;
    inputs.param_a := Bits.zero 256;
    inputs.param_b3 := Bits.zero 256;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in
  
  let run_point_add ~x1 ~y1 ~z1 ~x2 ~y2 ~z2 =
    reset ();
    
    inputs.x1 := z_to_bits x1;
    inputs.y1 := z_to_bits y1;
    inputs.z1 := z_to_bits z1;
    inputs.x2 := z_to_bits x2;
    inputs.y2 := z_to_bits y2;
    inputs.z2 := z_to_bits z2;
    inputs.param_a := z_to_bits param_a;
    inputs.param_b3 := z_to_bits param_b3;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    
    let max_cycles = 50000 in
    let rec wait n =
      if n >= max_cycles then begin
        Stdio.printf "  TIMEOUT after %d cycles\n" max_cycles;
        None
      end else if Bits.to_bool !(outputs.done_) then begin
        let result_x = bits_to_z !(outputs.x3) in
        let result_y = bits_to_z !(outputs.y3) in
        let result_z = bits_to_z !(outputs.z3) in
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
  (* TEST 1: G + G = 2G (Point Doubling)           *)
  (* ============================================== *)
  
  Stdio.printf "Test 1: G + G = 2G (Point Doubling)\n";
  Stdio.printf "  G_x = %s\n" (Z.to_string g_x);
  Stdio.printf "  G_y = %s\n" (Z.to_string g_y);
  
  let expected_2g = affine_add (g_x, g_y) (g_x, g_y) in
  
  (match run_point_add ~x1:g_x ~y1:g_y ~z1:Z.one ~x2:g_x ~y2:g_y ~z2:Z.one with
  | None -> record false
  | Some (rx, ry, rz) ->
      Stdio.printf "  Result (projective): X=%s..., Y=%s..., Z=%s...\n"
        (String.prefix (Z.to_string rx) 20)
        (String.prefix (Z.to_string ry) 20)
        (String.prefix (Z.to_string rz) 20);
      match expected_2g with
      | None -> 
          Stdio.printf "  Expected: point at infinity\n";
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          Stdio.printf "  Expected (affine): x=%s..., y=%s...\n"
            (String.prefix (Z.to_string ex) 20)
            (String.prefix (Z.to_string ey) 20);
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 2: G + 2G = 3G                           *)
  (* ============================================== *)
  
  Stdio.printf "Test 2: G + 2G = 3G\n";
  
  let p2g = affine_add (g_x, g_y) (g_x, g_y) in
  (match p2g with
  | None -> 
      Stdio.printf "  ERROR: 2G is point at infinity\n";
      record false
  | Some (x_2g, y_2g) ->
      Stdio.printf "  2G_x = %s...\n" (String.prefix (Z.to_string x_2g) 30);
      Stdio.printf "  2G_y = %s...\n" (String.prefix (Z.to_string y_2g) 30);
      
      let expected_3g = affine_add (g_x, g_y) (x_2g, y_2g) in
      
      (match run_point_add ~x1:g_x ~y1:g_y ~z1:Z.one ~x2:x_2g ~y2:y_2g ~z2:Z.one with
      | None -> record false
      | Some (rx, ry, rz) ->
          match expected_3g with
          | None ->
              let pass = Z.equal rz Z.zero in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass
          | Some (ex, ey) ->
              Stdio.printf "  Expected 3G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
              Stdio.printf "  Expected 3G_y = %s...\n" (String.prefix (Z.to_string ey) 30);
              let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass));
  
  (* ============================================== *)
  (* TEST 3: 2G + 2G = 4G                          *)
  (* ============================================== *)
  
  Stdio.printf "Test 3: 2G + 2G = 4G\n";
  
  (match p2g with
  | None -> record false
  | Some (x_2g, y_2g) ->
      let expected_4g = affine_add (x_2g, y_2g) (x_2g, y_2g) in
      
      (match run_point_add ~x1:x_2g ~y1:y_2g ~z1:Z.one ~x2:x_2g ~y2:y_2g ~z2:Z.one with
      | None -> record false
      | Some (rx, ry, rz) ->
          match expected_4g with
          | None ->
              let pass = Z.equal rz Z.zero in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass
          | Some (ex, ey) ->
              Stdio.printf "  Expected 4G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
              Stdio.printf "  Expected 4G_y = %s...\n" (String.prefix (Z.to_string ey) 30);
              let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass));
  
  (* ============================================== *)
  (* TEST 4: Compute 5G via additions              *)
  (* ============================================== *)
  
  Stdio.printf "Test 4: 2G + 3G = 5G\n";
  
  let p3g = 
    match p2g with
    | None -> None
    | Some (x_2g, y_2g) -> affine_add (g_x, g_y) (x_2g, y_2g)
  in
  
  (match p2g, p3g with
  | Some (x_2g, y_2g), Some (x_3g, y_3g) ->
      let expected_5g = affine_add (x_2g, y_2g) (x_3g, y_3g) in
      
      (match run_point_add ~x1:x_2g ~y1:y_2g ~z1:Z.one ~x2:x_3g ~y2:y_3g ~z2:Z.one with
      | None -> record false
      | Some (rx, ry, rz) ->
          match expected_5g with
          | None ->
              let pass = Z.equal rz Z.zero in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass
          | Some (ex, ey) ->
              Stdio.printf "  Expected 5G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
              Stdio.printf "  Expected 5G_y = %s...\n" (String.prefix (Z.to_string ey) 30);
              let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass)
  | _ ->
      Stdio.printf "  ERROR: Could not compute 2G or 3G\n";
      record false);
  
  (* ============================================== *)
  (* TEST 5: P + O = P (identity element)          *)
  (* ============================================== *)
  
  Stdio.printf "Test 5: G + O = G (adding point at infinity)\n";
  Stdio.printf "  Using O = (0:1:0) as point at infinity\n";
  
  (* Point at infinity in projective: (0:1:0) *)
  (match run_point_add ~x1:g_x ~y1:g_y ~z1:Z.one ~x2:Z.zero ~y2:Z.one ~z2:Z.zero with
  | None -> record false
  | Some (rx, ry, rz) ->
      let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:g_x ~aff_y:g_y in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 6: O + P = P (identity element)          *)
  (* ============================================== *)
  
  Stdio.printf "Test 6: O + G = G (point at infinity + G)\n";
  
  (match run_point_add ~x1:Z.zero ~y1:Z.one ~z1:Z.zero ~x2:g_x ~y2:g_y ~z2:Z.one with
  | None -> record false
  | Some (rx, ry, rz) ->
      let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:g_x ~aff_y:g_y in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
 (* ============================================== *)
  (* TEST 7: P + (-P) = O (inverse)                *)
  (* ============================================== *)
  
  Stdio.printf "Test 7: G + (-G) = O (point plus its inverse)\n";
  
  let neg_g_y = mod_sub Z.zero g_y in  (* -G has same x, negated y *)
  Stdio.printf "  -G_y = %s...\n" (String.prefix (Z.to_string neg_g_y) 30);
  
  (match run_point_add ~x1:g_x ~y1:g_y ~z1:Z.one ~x2:g_x ~y2:neg_g_y ~z2:Z.one with
  | None -> record false
  | Some (_rx, _ry, rz) ->
      Stdio.printf "  Result Z = %s\n" (Z.to_string rz);
      (* Result should be point at infinity: Z = 0 *)
      let pass = Z.equal rz Z.zero in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 8: Non-trivial projective coordinates    *)
  (* ============================================== *)
  
  Stdio.printf "Test 8: Addition with non-trivial Z coordinates\n";
  
  (* Represent G as (2*G_x : 2*G_y : 2) which equals (G_x : G_y : 1) *)
  let z_val = Z.of_int 2 in
  let x1_proj = mod_mul g_x z_val in
  let y1_proj = mod_mul g_y z_val in
  
  (* Represent 2G similarly *)
  (match p2g with
  | None -> record false
  | Some (x_2g, y_2g) ->
      let z2_val = Z.of_int 3 in
      let x2_proj = mod_mul x_2g z2_val in
      let y2_proj = mod_mul y_2g z2_val in
      
      let expected_3g = affine_add (g_x, g_y) (x_2g, y_2g) in
      
      (match run_point_add ~x1:x1_proj ~y1:y1_proj ~z1:z_val ~x2:x2_proj ~y2:y2_proj ~z2:z2_val with
      | None -> record false
      | Some (rx, ry, rz) ->
          match expected_3g with
          | None ->
              let pass = Z.equal rz Z.zero in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass
          | Some (ex, ey) ->
              Stdio.printf "  Expected 3G (affine): x=%s...\n" (String.prefix (Z.to_string ex) 30);
              let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass));
  
  (* ============================================== *)
  (* TEST 9: Verify 7G via scalar multiplication   *)
  (* ============================================== *)
  
  Stdio.printf "Test 9: Compute 7G = 3G + 4G and verify\n";
  
  let p4g = 
    match p2g with
    | None -> None
    | Some (x_2g, y_2g) -> affine_add (x_2g, y_2g) (x_2g, y_2g)
  in
  
  (match p3g, p4g with
  | Some (x_3g, y_3g), Some (x_4g, y_4g) ->
      let expected_7g = scalar_mult (Z.of_int 7) (g_x, g_y) in
      
      (match run_point_add ~x1:x_3g ~y1:y_3g ~z1:Z.one ~x2:x_4g ~y2:y_4g ~z2:Z.one with
      | None -> record false
      | Some (rx, ry, rz) ->
          match expected_7g with
          | None ->
              let pass = Z.equal rz Z.zero in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass
          | Some (ex, ey) ->
              Stdio.printf "  Expected 7G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
              Stdio.printf "  Expected 7G_y = %s...\n" (String.prefix (Z.to_string ey) 30);
              let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
              Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
              record pass)
  | _ ->
      Stdio.printf "  ERROR: Could not compute 3G or 4G\n";
      record false);
  
  (* ============================================== *)
  (* TEST 10: Larger scalar - 100G                 *)
  (* ============================================== *)
  
  Stdio.printf "Test 10: Verify 100G = 64G + 32G + 4G\n";
  
  (* Compute powers of 2 times G *)
  let compute_2pow_g n =
    let rec loop i pt =
      if i >= n then Some pt
      else
        match affine_add pt pt with
        | None -> None
        | Some p -> loop (i + 1) p
    in
    loop 0 (g_x, g_y)
  in
  
  let p4g_direct = compute_2pow_g 2 in   (* 4G = 2^2 * G *)
  let p32g = compute_2pow_g 5 in          (* 32G = 2^5 * G *)
  let p64g = compute_2pow_g 6 in          (* 64G = 2^6 * G *)
  
  (match p4g_direct, p32g, p64g with
  | Some (x_4g, y_4g), Some (x_32g, y_32g), Some (x_64g, y_64g) ->
      (* First compute 64G + 32G = 96G *)
      (match run_point_add ~x1:x_64g ~y1:y_64g ~z1:Z.one ~x2:x_32g ~y2:y_32g ~z2:Z.one with
      | None -> record false
      | Some (rx_96, ry_96, rz_96) ->
          (* Then compute 96G + 4G = 100G *)
          (match run_point_add ~x1:rx_96 ~y1:ry_96 ~z1:rz_96 ~x2:x_4g ~y2:y_4g ~z2:Z.one with
          | None -> record false
          | Some (rx, ry, rz) ->
              let expected_100g = scalar_mult (Z.of_int 100) (g_x, g_y) in
              match expected_100g with
              | None ->
                  let pass = Z.equal rz Z.zero in
                  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
                  record pass
              | Some (ex, ey) ->
                  Stdio.printf "  Expected 100G_x = %s...\n" (String.prefix (Z.to_string ex) 30);
                  let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
                  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
                  record pass))
  | _ ->
      Stdio.printf "  ERROR: Could not compute required points\n";
      record false);


      (* ============================================== *)
(* REPEATED OPERATIONS TEST                       *)
(* ============================================== *)

Stdio.printf "=== Repeated Operations Test ===\n\n";

(* Compute 10G by doing G + G + G + ... (10 additions) *)
Stdio.printf "Test: Compute 10G via 9 sequential additions\n";

let repeated_pass = ref true in
let current_x = ref g_x in
let current_y = ref g_y in
let current_z = ref Z.one in

for i = 1 to 9 do
  (* current += G *)
  match run_point_add 
    ~x1:!current_x ~y1:!current_y ~z1:!current_z 
    ~x2:g_x ~y2:g_y ~z2:Z.one 
  with
  | None -> 
      Stdio.printf "  Addition %d: TIMEOUT\n" i;
      repeated_pass := false
  | Some (rx, ry, rz) ->
      current_x := rx;
      current_y := ry;
      current_z := rz;
      if i % 3 = 0 then
        Stdio.printf "  Addition %d complete\n" i
done;

(* Verify final result is 10G *)
let expected_10g = scalar_mult (Z.of_int 10) (g_x, g_y) in
(match expected_10g with
| None -> 
    Stdio.printf "  Expected: point at infinity\n";
    repeated_pass := false
| Some (ex, ey) ->
    let pass = proj_equals_affine ~proj_x:!current_x ~proj_y:!current_y ~proj_z:!current_z ~aff_x:ex ~aff_y:ey in
    Stdio.printf "  Final result matches 10G: %b\n" pass;
    if not pass then repeated_pass := false);

Stdio.printf "  %s\n\n" (if !repeated_pass then "PASS ✓" else "FAIL ✗");
record !repeated_pass;

(* ============================================== *)
(* ALTERNATING DOUBLING AND ADDITION TEST        *)
(* ============================================== *)

Stdio.printf "=== Double-and-Add Pattern Test ===\n\n";

(* Simulate scalar multiplication pattern: double, conditionally add *)
(* Compute 11G = 1011 in binary: G -> 2G -> 2*2G+G=5G -> 2*5G+G=11G *)
Stdio.printf "Test: Compute 11G using double-and-add pattern\n";

let dbl_add_pass = ref true in

(* Start with G *)
let acc_x = ref g_x in
let acc_y = ref g_y in
let acc_z = ref Z.one in

(* Process bits of 11 = 1011 from MSB-1 downward: 0, 1, 1 *)
let bits = [0; 1; 1] in

List.iteri bits ~f:(fun i bit ->
  (* Double *)
  Stdio.printf "  Step %d: Double\n" (i * 2);
  (match run_point_add 
    ~x1:!acc_x ~y1:!acc_y ~z1:!acc_z 
    ~x2:!acc_x ~y2:!acc_y ~z2:!acc_z 
  with
  | None -> 
      Stdio.printf "    TIMEOUT on double\n";
      dbl_add_pass := false
  | Some (rx, ry, rz) ->
      acc_x := rx;
      acc_y := ry;
      acc_z := rz);
  
  (* Conditionally add G if bit is 1 *)
  if bit = 1 then begin
    Stdio.printf "  Step %d: Add G (bit=1)\n" (i * 2 + 1);
    (match run_point_add 
      ~x1:!acc_x ~y1:!acc_y ~z1:!acc_z 
      ~x2:g_x ~y2:g_y ~z2:Z.one 
    with
    | None -> 
        Stdio.printf "    TIMEOUT on add\n";
        dbl_add_pass := false
    | Some (rx, ry, rz) ->
        acc_x := rx;
        acc_y := ry;
        acc_z := rz)
  end else
    Stdio.printf "  Step %d: Skip add (bit=0)\n" (i * 2 + 1)
);

(* Verify result is 11G *)
let expected_11g = scalar_mult (Z.of_int 11) (g_x, g_y) in
(match expected_11g with
| None -> 
    Stdio.printf "  Expected: point at infinity\n";
    dbl_add_pass := false
| Some (ex, ey) ->
    let pass = proj_equals_affine ~proj_x:!acc_x ~proj_y:!acc_y ~proj_z:!acc_z ~aff_x:ex ~aff_y:ey in
    Stdio.printf "  Result matches 11G: %b\n" pass;
    if not pass then dbl_add_pass := false);

Stdio.printf "  %s\n\n" (if !dbl_add_pass then "PASS ✓" else "FAIL ✗");
record !dbl_add_pass;

(* ============================================== *)
(* MIXED POINT SOURCES TEST                      *)
(* ============================================== *)

Stdio.printf "=== Mixed Point Sources Test ===\n\n";

(* Simulate table lookup pattern - alternate between different source points *)
Stdio.printf "Test: Alternating between different precomputed points\n";

(* Precompute 1G, 2G, 3G as our "table" *)
let table = Array.create ~len:4 (Z.zero, Z.zero) in
table.(0) <- (g_x, g_y);  (* 1G *)

(match scalar_mult (Z.of_int 2) (g_x, g_y) with
| Some p -> table.(1) <- p
| None -> ());

(match scalar_mult (Z.of_int 3) (g_x, g_y) with
| Some p -> table.(2) <- p
| None -> ());

(* Compute: G + 2G + 3G + G + 2G = 9G *)
let indices = [0; 1; 2; 0; 1] in  (* Table indices to use *)
let mixed_pass = ref true in

let acc_x = ref Z.zero in
let acc_y = ref Z.one in
let acc_z = ref Z.zero in  (* Start at point at infinity *)

List.iteri indices ~f:(fun i idx ->
  let (tx, ty) = table.(idx) in
  Stdio.printf "  Op %d: Add table[%d] (%dG)\n" i idx (idx + 1);
  
  (match run_point_add 
    ~x1:!acc_x ~y1:!acc_y ~z1:!acc_z 
    ~x2:tx ~y2:ty ~z2:Z.one 
  with
  | None -> 
      Stdio.printf "    TIMEOUT\n";
      mixed_pass := false
  | Some (rx, ry, rz) ->
      acc_x := rx;
      acc_y := ry;
      acc_z := rz)
);

(* 1 + 2 + 3 + 1 + 2 = 9G *)
let expected_9g = scalar_mult (Z.of_int 9) (g_x, g_y) in
(match expected_9g with
| None -> 
    Stdio.printf "  Expected: point at infinity\n";
    mixed_pass := false
| Some (ex, ey) ->
    let pass = proj_equals_affine ~proj_x:!acc_x ~proj_y:!acc_y ~proj_z:!acc_z ~aff_x:ex ~aff_y:ey in
    Stdio.printf "  Result matches 9G: %b\n" pass;
    if not pass then mixed_pass := false);

Stdio.printf "  %s\n\n" (if !mixed_pass then "PASS ✓" else "FAIL ✗");
record !mixed_pass;

(* ============================================== *)
(* ACCUMULATOR GROWS THEN RESETS TEST            *)
(* ============================================== *)

Stdio.printf "=== Accumulator Reset Pattern Test ===\n\n";

(* Pattern: build up accumulator, verify, reset, build again *)
Stdio.printf "Test: Multiple independent scalar multiplications\n";

let reset_pass = ref true in

(* First: compute 5G *)
Stdio.printf "  Computing 5G...\n";
let acc1_x = ref g_x in
let acc1_y = ref g_y in
let acc1_z = ref Z.one in

for _ = 1 to 4 do
  match run_point_add 
    ~x1:!acc1_x ~y1:!acc1_y ~z1:!acc1_z 
    ~x2:g_x ~y2:g_y ~z2:Z.one 
  with
  | None -> reset_pass := false
  | Some (rx, ry, rz) ->
      acc1_x := rx;
      acc1_y := ry;
      acc1_z := rz
done;

let expected_5g = scalar_mult (Z.of_int 5) (g_x, g_y) in
(match expected_5g with
| Some (ex, ey) ->
    let pass = proj_equals_affine ~proj_x:!acc1_x ~proj_y:!acc1_y ~proj_z:!acc1_z ~aff_x:ex ~aff_y:ey in
    Stdio.printf "    5G correct: %b\n" pass;
    if not pass then reset_pass := false
| None -> reset_pass := false);

(* Second: fresh start, compute 7G *)
Stdio.printf "  Computing 7G (fresh start)...\n";
let acc2_x = ref g_x in
let acc2_y = ref g_y in
let acc2_z = ref Z.one in

for _ = 1 to 6 do
  match run_point_add 
    ~x1:!acc2_x ~y1:!acc2_y ~z1:!acc2_z 
    ~x2:g_x ~y2:g_y ~z2:Z.one 
  with
  | None -> reset_pass := false
  | Some (rx, ry, rz) ->
      acc2_x := rx;
      acc2_y := ry;
      acc2_z := rz
done;

let expected_7g = scalar_mult (Z.of_int 7) (g_x, g_y) in
(match expected_7g with
| Some (ex, ey) ->
    let pass = proj_equals_affine ~proj_x:!acc2_x ~proj_y:!acc2_y ~proj_z:!acc2_z ~aff_x:ex ~aff_y:ey in
    Stdio.printf "    7G correct: %b\n" pass;
    if not pass then reset_pass := false
| None -> reset_pass := false);

(* Third: compute 5G + 7G = 12G using results from above *)
Stdio.printf "  Computing 5G + 7G = 12G...\n";
(match run_point_add 
  ~x1:!acc1_x ~y1:!acc1_y ~z1:!acc1_z 
  ~x2:!acc2_x ~y2:!acc2_y ~z2:!acc2_z 
with
| None -> 
    Stdio.printf "    TIMEOUT\n";
    reset_pass := false
| Some (rx, ry, rz) ->
    let expected_12g = scalar_mult (Z.of_int 12) (g_x, g_y) in
    (match expected_12g with
    | Some (ex, ey) ->
        let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
        Stdio.printf "    12G correct: %b\n" pass;
        if not pass then reset_pass := false
    | None -> reset_pass := false));

Stdio.printf "  %s\n\n" (if !reset_pass then "PASS ✓" else "FAIL ✗");
record !reset_pass;

(* ============================================== *)
(* RAPID SMALL OPERATIONS TEST                   *)
(* ============================================== *)

Stdio.printf "=== Rapid Small Operations Test ===\n\n";

(* Many quick additions with small coordinates to stress control flow *)
Stdio.printf "Test: 20 rapid point additions\n";

let rapid_pass = ref true in

(* Actually, let's just use multiples of G to ensure valid points *)
let acc_x = ref g_x in
let acc_y = ref g_y in
let acc_z = ref Z.one in

for i = 1 to 20 do
  match run_point_add 
    ~x1:!acc_x ~y1:!acc_y ~z1:!acc_z 
    ~x2:g_x ~y2:g_y ~z2:Z.one 
  with
  | None -> 
      Stdio.printf "  Op %d: TIMEOUT\n" i;
      rapid_pass := false
  | Some (rx, ry, rz) ->
      acc_x := rx;
      acc_y := ry;
      acc_z := rz
done;

(* Verify: should be 21G *)
let expected_21g = scalar_mult (Z.of_int 21) (g_x, g_y) in
(match expected_21g with
| None -> 
    Stdio.printf "  Expected: point at infinity\n";
    rapid_pass := false
| Some (ex, ey) ->
    let pass = proj_equals_affine ~proj_x:!acc_x ~proj_y:!acc_y ~proj_z:!acc_z ~aff_x:ex ~aff_y:ey in
    Stdio.printf "  After 20 additions, result matches 21G: %b\n" pass;
    if not pass then rapid_pass := false);

Stdio.printf "  %s\n\n" (if !rapid_pass then "PASS ✓" else "FAIL ✗");
record !rapid_pass;
  
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
    Stdio.printf "███████║╚██████╔╝╚██████╗╚██████╗███████╗███████║███████║\n";
    Stdio.printf "╚══════╝ ╚═════╝  ╚═════╝ ╚═════╝╚══════╝╚══════╝╚══════╝\n";
    Stdio.printf "\nAll point addition tests passed! ✓✓✓\n"
  end else
    Stdio.printf "\n✗ Some tests failed - review above for details\n"