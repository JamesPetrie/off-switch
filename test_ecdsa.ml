open Base
open Hardcaml

let () =
  Stdio.printf "=== Double Scalar Multiplication Unit Test ===\n";
  Stdio.printf "=== Computes u*G + v*Q where Q = 2G ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Ecdsa.I)(Ecdsa.O) in
  let sim = Sim.create (Ecdsa.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let prime_p = Arith.Config.prime_p in
  
  (* secp256k1 curve parameters: y² = x³ + 7, so a = 0, b = 7, b3 = 21 *)
  let param_a = Z.zero in
  let param_b3 = Z.of_int 21 in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in
  
  let bits_to_z bits = Z.of_string_base 2 (Bits.to_bstr bits) in
  
  (* Modular arithmetic helpers *)
  let mod_mul a b = Z.((a * b) mod prime_p) in
  let mod_inv a = Z.invert a prime_p in
  
  (* Convert projective (X:Y:Z) to affine (x, y) *)
  let proj_to_affine x y z =
    if Z.equal z Z.zero then
      None
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
  
  (* Reference: affine point addition *)
  let affine_add (x1, y1) (x2, y2) =
    let mod_add a b = Z.((a + b) mod prime_p) in
    let mod_sub a b = Z.(erem (a - b) prime_p) in
    if Z.equal x1 x2 then begin
      if Z.equal y1 y2 then
        let lambda = mod_mul (mod_mul (Z.of_int 3) (mod_mul x1 x1)) (mod_inv (mod_mul (Z.of_int 2) y1)) in
        let x3 = mod_sub (mod_mul lambda lambda) (mod_add x1 x2) in
        let y3 = mod_sub (mod_mul lambda (mod_sub x1 x3)) y1 in
        Some (x3, y3)
      else
        None
    end else begin
      let lambda = mod_mul (mod_sub y2 y1) (mod_inv (mod_sub x2 x1)) in
      let x3 = mod_sub (mod_sub (mod_mul lambda lambda) x1) x2 in
      let y3 = mod_sub (mod_mul lambda (mod_sub x1 x3)) y1 in
      Some (x3, y3)
    end
  in
  
  (* Reference: scalar multiplication *)
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
  
  (* Generator point G *)
  let g_x = Z.of_string "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" in
  let g_y = Z.of_string "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8" in
  
  let reset () =
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.scalar_u := Bits.zero 256;
    inputs.scalar_v := Bits.zero 256;
    inputs.param_a := Bits.zero 256;
    inputs.param_b3 := Bits.zero 256;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in
  
  let run_double_scalar_mult ~u ~v =
    reset ();
    
    inputs.scalar_u := z_to_bits u;
    inputs.scalar_v := z_to_bits v;
    inputs.param_a := z_to_bits param_a;
    inputs.param_b3 := z_to_bits param_b3;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    
    let max_cycles = 5_000_000 in
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
  
  (* Since Q = 2G, we have u*G + v*Q = u*G + v*2G = (u + 2v)*G *)
  let expected_result u v =
    let combined = Z.(u + (of_int 2) * v) in
    scalar_mult combined (g_x, g_y)
  in
  

Stdio.printf "=== Verifying hardcoded constants ===\n\n";

(* Check Q = 2G *)
let q_x = Z.of_string "0xc6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5" in
let q_y = Z.of_string "0x1ae168fea63dc339a3c58419466ceae1061b7cd988a6f08680ae1fbd1f2e7682" in

(match scalar_mult (Z.of_int 2) (g_x, g_y) with
| None -> Stdio.printf "2G = infinity (ERROR)\n"
| Some (expected_x, expected_y) ->
    let q_ok = Z.equal q_x expected_x && Z.equal q_y expected_y in
    Stdio.printf "Q = 2G? %b\n" q_ok;
    if not q_ok then begin
      Stdio.printf "  Expected x: %s\n" (Z.to_string expected_x);
      Stdio.printf "  Got x:      %s\n" (Z.to_string q_x);
    end);

(* Check G+Q = 3G *)
let gpq_x = Z.of_string "0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9" in
let gpq_y = Z.of_string "0x388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672" in

(match scalar_mult (Z.of_int 3) (g_x, g_y) with
| None -> Stdio.printf "3G = infinity (ERROR)\n"
| Some (expected_x, expected_y) ->
    let gpq_ok = Z.equal gpq_x expected_x && Z.equal gpq_y expected_y in
    Stdio.printf "G+Q = 3G? %b\n" gpq_ok;
    if not gpq_ok then begin
      Stdio.printf "  Expected x: %s\n" (Z.to_string expected_x);
      Stdio.printf "  Got x:      %s\n" (Z.to_string gpq_x);
    end);

Stdio.printf "\n";


(match scalar_mult (Z.of_int 2) (g_x, g_y) with
| None -> Stdio.printf "2G = infinity (ERROR)\n"
| Some (expected_x, expected_y) ->
    let x_ok = Z.equal q_x expected_x in
    let y_ok = Z.equal q_y expected_y in
    Stdio.printf "Q = 2G? x matches: %b, y matches: %b\n" x_ok y_ok;
    if not y_ok then begin
      Stdio.printf "  Expected y: %s\n" (Z.to_string expected_y);
      Stdio.printf "  Got y:      %s\n" (Z.to_string q_y);
    end);
  (* ============================================== *)
  (* TEST 1: u=1, v=0 → 1*G + 0*Q = G              *)
  (* ============================================== *)
  
  Stdio.printf "Test 1: u=1, v=0 → 1*G + 0*Q = G\n";
  
  let u = Z.one in
  let v = Z.zero in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  Expected: G\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 2: u=0, v=1 → 0*G + 1*Q = 2G             *)
  (* ============================================== *)
  
Stdio.printf "Test 2: u=0, v=1 → 0*G + 1*Q = 2G\n";

let u = Z.zero in
let v = Z.one in

(match run_double_scalar_mult ~u ~v with
| None -> record false
| Some (rx, ry, rz) ->
    (* Debug: check if result equals G *)
    let is_g = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:g_x ~aff_y:g_y in
    Stdio.printf "  Result equals G? %b\n" is_g;
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  Expected: 2G\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 3: u=1, v=1 → 1*G + 1*Q = 3G             *)
  (* ============================================== *)
  
  Stdio.printf "Test 3: u=1, v=1 → 1*G + 1*Q = 3G\n";
  
  let u = Z.one in
  let v = Z.one in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  Expected: 3G\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 4: u=0, v=0 → 0*G + 0*Q = infinity       *)
  (* ============================================== *)
  
  Stdio.printf "Test 4: u=0, v=0 → 0*G + 0*Q = infinity\n";
  
  let u = Z.zero in
  let v = Z.zero in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (_rx, _ry, rz) ->
      let pass = Z.equal rz Z.zero in
      Stdio.printf "  Result Z = %s\n" (Z.to_string rz);
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 5: u=5, v=3 → 5*G + 3*Q = 11G            *)
  (* ============================================== *)
  
  Stdio.printf "Test 5: u=5, v=3 → 5*G + 3*Q = 5G + 6G = 11G\n";
  
  let u = Z.of_int 5 in
  let v = Z.of_int 3 in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  Expected: 11G\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 6: u=100, v=50 → 100*G + 50*Q = 200G     *)
  (* ============================================== *)
  
  Stdio.printf "Test 6: u=100, v=50 → 100*G + 50*Q = 100G + 100G = 200G\n";
  
  let u = Z.of_int 100 in
  let v = Z.of_int 50 in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  Expected: 200G\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 7: u=0xFF, v=0xAA → alternating bits     *)
  (* ============================================== *)
  
  Stdio.printf "Test 7: u=0xFF, v=0xAA → mixed bit patterns\n";
  
  let u = Z.of_int 0xFF in
  let v = Z.of_int 0xAA in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  Expected: (u + 2v)G = %sG\n" (Z.to_string Z.(u + (of_int 2) * v));
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 8: Large scalars                         *)
  (* ============================================== *)
  
  Stdio.printf "Test 8: Large scalars u=2^128, v=2^64\n";
  
  let u = Z.shift_left Z.one 128 in
  let v = Z.shift_left Z.one 64 in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 9: Both scalars have high bits set       *)
  (* ============================================== *)
  
  Stdio.printf "Test 9: u=2^255, v=2^255 (both have MSB set)\n";
  
  let u = Z.shift_left Z.one 255 in
  let v = Z.shift_left Z.one 255 in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 10: Random-looking values                *)
  (* ============================================== *)
  
  Stdio.printf "Test 10: u=0xDEADBEEF, v=0xCAFEBABE\n";
  
  let u = Z.of_string "0xDEADBEEF" in
  let v = Z.of_string "0xCAFEBABE" in
  
  (match run_double_scalar_mult ~u ~v with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result u v in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  Expected: (u + 2v)G = %sG\n" (Z.to_string Z.(u + (of_int 2) * v));
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
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
    Stdio.printf "\nAll double scalar multiplication tests passed! ✓✓✓\n"
  end else
    Stdio.printf "\n✗ Some tests failed - review above for details\n"