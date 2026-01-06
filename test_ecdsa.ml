open Base
open Hardcaml

let () =
  Stdio.printf "=== ECDSA Scalar Derivation Test ===\n";
  Stdio.printf "=== Computes u1*G + u2*Q from (z, r, s) ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Ecdsa.I)(Ecdsa.O) in
  let sim = Sim.create (Ecdsa.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let prime_p = Arith.Config.prime_p in
  let prime_n = Arith.Config.prime_n in
  
  (* secp256k1 curve parameters *)
  let param_a = Z.zero in
  let param_b3 = Z.of_int 21 in
  
  (* Generator point G *)
  let g_x = Z.of_string "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" in
  let g_y = Z.of_string "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8" in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in
  
  let bits_to_z bits = Z.of_string_base 2 (Bits.to_bstr bits) in
  
  (* Modular arithmetic helpers *)
  let mod_mul_p a b = Z.((a * b) mod prime_p) in
  let mod_inv_p a = Z.invert a prime_p in
  let mod_inv_n a = Z.invert a prime_n in
  let mod_mul_n a b = Z.((a * b) mod prime_n) in
  
  (* Convert projective to affine *)
  let proj_to_affine x y z =
    if Z.equal z Z.zero then
      None
    else
      let z_inv = mod_inv_p z in
      let aff_x = mod_mul_p x z_inv in
      let aff_y = mod_mul_p y z_inv in
      Some (aff_x, aff_y)
  in
  
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
        let lambda = mod_mul_p (mod_mul_p (Z.of_int 3) (mod_mul_p x1 x1)) (mod_inv_p (mod_mul_p (Z.of_int 2) y1)) in
        let x3 = mod_sub (mod_mul_p lambda lambda) (mod_add x1 x2) in
        let y3 = mod_sub (mod_mul_p lambda (mod_sub x1 x3)) y1 in
        Some (x3, y3)
      else
        None
    end else begin
      let lambda = mod_mul_p (mod_sub y2 y1) (mod_inv_p (mod_sub x2 x1)) in
      let x3 = mod_sub (mod_sub (mod_mul_p lambda lambda) x1) x2 in
      let y3 = mod_sub (mod_mul_p lambda (mod_sub x1 x3)) y1 in
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
  
  (* Q = 2G *)
  let q_x = Z.of_string "0xc6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5" in
  let q_y = Z.of_string "12158399299693830322967808612713398636155367887041628176798871954788371653930" in
  
  (* Compute expected u1*G + u2*Q *)
  let expected_result ~z ~r ~s =
    let w = mod_inv_n s in
    let u1 = mod_mul_n z w in
    let u2 = mod_mul_n r w in
    Stdio.printf "  Computed u1 = %s...\n" (String.prefix (Z.to_string u1) 30);
    Stdio.printf "  Computed u2 = %s...\n" (String.prefix (Z.to_string u2) 30);
    
    let u1_g = scalar_mult u1 (g_x, g_y) in
    let u2_q = scalar_mult u2 (q_x, q_y) in
    match u1_g, u2_q with
    | None, None -> None
    | Some p, None -> Some p
    | None, Some p -> Some p
    | Some p1, Some p2 -> affine_add p1 p2
  in
  
  let reset () =
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.z := Bits.zero 256;
    inputs.r := Bits.zero 256;
    inputs.s := Bits.zero 256;
    inputs.param_a := Bits.zero 256;
    inputs.param_b3 := Bits.zero 256;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in
  
  let run_ecdsa ~z ~r ~s =
    reset ();
    
    inputs.z := z_to_bits z;
    inputs.r := z_to_bits r;
    inputs.s := z_to_bits s;
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
        let result_z = bits_to_z !(outputs.z_out) in
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
  (* TEST 1: Simple values                         *)
  (* ============================================== *)
  
  Stdio.printf "Test 1: Simple values z=1, r=2, s=3\n";
  
  let z = Z.one in
  let r = Z.of_int 2 in
  let s = Z.of_int 3 in
  
  (match run_ecdsa ~z ~r ~s with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result ~z ~r ~s in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  Expected: infinity\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 2: Larger values                         *)
  (* ============================================== *)
  
  Stdio.printf "Test 2: Larger values z=0xDEADBEEF, r=0xCAFEBABE, s=0x12345678\n";
  
  let z = Z.of_string "0xDEADBEEF" in
  let r = Z.of_string "0xCAFEBABE" in
  let s = Z.of_string "0x12345678" in
  
  (match run_ecdsa ~z ~r ~s with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result ~z ~r ~s in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  Expected: infinity\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 3: 128-bit values                        *)
  (* ============================================== *)
  
  Stdio.printf "Test 3: 128-bit values\n";
  
  let z = Z.of_string "0x123456789ABCDEF0123456789ABCDEF0" in
  let r = Z.of_string "0xFEDCBA9876543210FEDCBA9876543210" in
  let s = Z.of_string "0xAAAABBBBCCCCDDDDEEEEFFFF00001111" in
  
  (match run_ecdsa ~z ~r ~s with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result ~z ~r ~s in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  Expected: infinity\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 4: s = 1 (w = 1, so u1 = z, u2 = r)      *)
  (* ============================================== *)
  
  Stdio.printf "Test 4: s=1 means u1=z, u2=r directly\n";
  
  let z = Z.of_int 5 in
  let r = Z.of_int 3 in
  let s = Z.one in
  
  Stdio.printf "  This should compute 5*G + 3*Q = 5G + 6G = 11G\n";
  
  (match run_ecdsa ~z ~r ~s with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result ~z ~r ~s in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  Expected: infinity\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 5: z = 0 (u1 = 0)                        *)
  (* ============================================== *)
  
  Stdio.printf "Test 5: z=0 means u1=0, result is just u2*Q\n";
  
  let z = Z.zero in
  let r = Z.of_int 7 in
  let s = Z.of_int 11 in
  
  (match run_ecdsa ~z ~r ~s with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result ~z ~r ~s in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  Expected: infinity\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 6: r = 0 (u2 = 0)                        *)
  (* ============================================== *)
  
  Stdio.printf "Test 6: r=0 means u2=0, result is just u1*G\n";
  
  let z = Z.of_int 13 in
  let r = Z.zero in
  let s = Z.of_int 17 in
  
  (match run_ecdsa ~z ~r ~s with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result ~z ~r ~s in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  Expected: infinity\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);
  
  (* ============================================== *)
  (* TEST 7: 256-bit realistic values              *)
  (* ============================================== *)
  
  Stdio.printf "Test 7: 256-bit realistic ECDSA-like values\n";
  
  let z = Z.of_string "0xb94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9" in
  let r = Z.of_string "0x241097efbf8c27ad1ff6d370a878a6c3a5f1d2c5e4f3c2b1a0987654321fedcb" in
  let s = Z.of_string "0x3b78ce563f89a0ed9414f5aa28ad0d96d6795f9c63012d1acde45678c0def123" in
  
  (match run_ecdsa ~z ~r ~s with
  | None -> record false
  | Some (rx, ry, rz) ->
      let expected = expected_result ~z ~r ~s in
      match expected with
      | None ->
          let pass = Z.equal rz Z.zero in
          Stdio.printf "  Expected: infinity\n";
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass
      | Some (ex, ey) ->
          let pass = proj_equals_affine ~proj_x:rx ~proj_y:ry ~proj_z:rz ~aff_x:ex ~aff_y:ey in
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
    Stdio.printf "\nAll ECDSA scalar derivation tests passed! ✓✓✓\n"
  end else
    Stdio.printf "\n✗ Some tests failed - review above for details\n"