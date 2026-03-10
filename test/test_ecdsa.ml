open Base
open Hardcaml

let () =
  Stdio.printf "=== ECDSA Signature Verification Test ===\n\n";

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

  (* Private key d = 2, Public key Q = 2G *)
  let private_key = Z.of_int 2 in

  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in

  (* Modular arithmetic helpers *)
  let mod_mul_p a b = Z.((a * b) mod prime_p) in
  let mod_inv_p a = Z.invert a prime_p in
  let mod_inv_n a = Z.invert a prime_n in
  let mod_mul_n a b = Z.((a * b) mod prime_n) in
  let mod_add_n a b = Z.((a + b) mod prime_n) in

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

  (* Generate a valid ECDSA signature for message hash z using nonce k *)
  let sign ~z ~k =
    (* R = k*G *)
    match scalar_mult k (g_x, g_y) with
    | None -> None
    | Some (rx, _ry) ->
        let r = Z.(rx mod prime_n) in
        if Z.equal r Z.zero then None
        else
          (* s = k^(-1) * (z + r*d) mod n *)
          let k_inv = mod_inv_n k in
          let s = mod_mul_n k_inv (mod_add_n z (mod_mul_n r private_key)) in
          if Z.equal s Z.zero then None
          else Some (r, s)
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

  let run_verify ~z ~r ~s =
    reset ();

    inputs.z := z_to_bits z;
    inputs.r := z_to_bits r;
    inputs.s := z_to_bits s;
    inputs.param_a := z_to_bits param_a;
    inputs.param_b3 := z_to_bits param_b3;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;

    let max_cycles = 15_000_000 in
    let rec wait n =
      if n >= max_cycles then begin
        Stdio.printf "  TIMEOUT after %d cycles\n" max_cycles;
        None
      end else if Bits.to_bool !(outputs.done_) then begin
        let valid = Bits.to_bool !(outputs.valid) in
        Stdio.printf "  Completed in %d cycles\n" n;
        Some valid
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
  (* TEST 1: Valid signature with small values     *)
  (* ============================================== *)

  Stdio.printf "Test 1: Valid signature (z=12345, k=7)\n";

  let z = Z.of_int 12345 in
  let k = Z.of_int 7 in

  (match sign ~z ~k with
  | None ->
      Stdio.printf "  Failed to generate signature\n";
      record false
  | Some (r, s) ->
      Stdio.printf "  Generated r = %s...\n" (String.prefix (Z.to_string r) 30);
      Stdio.printf "  Generated s = %s...\n" (String.prefix (Z.to_string s) 30);
      match run_verify ~z ~r ~s with
      | None -> record false
      | Some valid ->
          Stdio.printf "  Valid = %b (expected true)\n" valid;
          let pass = valid in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);

  (* ============================================== *)
  (* TEST 2: Valid signature with larger values    *)
  (* ============================================== *)

  Stdio.printf "Test 2: Valid signature (z=0xDEADBEEF, k=0x123456)\n";

  let z = Z.of_string "0xDEADBEEF" in
  let k = Z.of_string "0x123456" in

  (match sign ~z ~k with
  | None ->
      Stdio.printf "  Failed to generate signature\n";
      record false
  | Some (r, s) ->
      Stdio.printf "  Generated r = %s...\n" (String.prefix (Z.to_string r) 30);
      Stdio.printf "  Generated s = %s...\n" (String.prefix (Z.to_string s) 30);
      match run_verify ~z ~r ~s with
      | None -> record false
      | Some valid ->
          Stdio.printf "  Valid = %b (expected true)\n" valid;
          let pass = valid in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);

  (* ============================================== *)
  (* TEST 3: Valid signature with 256-bit hash     *)
  (* ============================================== *)

  Stdio.printf "Test 3: Valid signature (256-bit z, k)\n";

  let z = Z.of_string "0xb94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9" in
  let k = Z.of_string "0x3b78ce563f89a0ed9414f5aa28ad0d96d6795f9c63" in

  (match sign ~z ~k with
  | None ->
      Stdio.printf "  Failed to generate signature\n";
      record false
  | Some (r, s) ->
      Stdio.printf "  Generated r = %s...\n" (String.prefix (Z.to_string r) 30);
      Stdio.printf "  Generated s = %s...\n" (String.prefix (Z.to_string s) 30);
      match run_verify ~z ~r ~s with
      | None -> record false
      | Some valid ->
          Stdio.printf "  Valid = %b (expected true)\n" valid;
          let pass = valid in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);

  (* ============================================== *)
  (* TEST 4: Invalid signature - wrong z           *)
  (* ============================================== *)

  Stdio.printf "Test 4: Invalid signature (wrong message hash)\n";

  let z = Z.of_int 12345 in
  let k = Z.of_int 7 in

  (match sign ~z ~k with
  | None ->
      Stdio.printf "  Failed to generate signature\n";
      record false
  | Some (r, s) ->
      let wrong_z = Z.of_int 99999 in  (* Different message *)
      Stdio.printf "  Verifying with wrong z = 99999\n";
      match run_verify ~z:wrong_z ~r ~s with
      | None -> record false
      | Some valid ->
          Stdio.printf "  Valid = %b (expected false)\n" valid;
          let pass = not valid in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);

  (* ============================================== *)
  (* TEST 5: Invalid signature - wrong r           *)
  (* ============================================== *)

  Stdio.printf "Test 5: Invalid signature (wrong r)\n";

  let z = Z.of_int 12345 in
  let k = Z.of_int 7 in

  (match sign ~z ~k with
  | None ->
      Stdio.printf "  Failed to generate signature\n";
      record false
  | Some (_r, s) ->
      let wrong_r = Z.of_int 11111 in  (* Different r *)
      Stdio.printf "  Verifying with wrong r = 11111\n";
      match run_verify ~z ~r:wrong_r ~s with
      | None -> record false
      | Some valid ->
          Stdio.printf "  Valid = %b (expected false)\n" valid;
          let pass = not valid in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);

  (* ============================================== *)
  (* TEST 6: Invalid signature - wrong s           *)
  (* ============================================== *)

  Stdio.printf "Test 6: Invalid signature (wrong s)\n";

  let z = Z.of_int 12345 in
  let k = Z.of_int 7 in

  (match sign ~z ~k with
  | None ->
      Stdio.printf "  Failed to generate signature\n";
      record false
  | Some (r, _s) ->
      let wrong_s = Z.of_int 22222 in  (* Different s *)
      Stdio.printf "  Verifying with wrong s = 22222\n";
      match run_verify ~z ~r ~s:wrong_s with
      | None -> record false
      | Some valid ->
          Stdio.printf "  Valid = %b (expected false)\n" valid;
          let pass = not valid in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);

  (* ============================================== *)
  (* TEST 7: Valid signature - another random case *)
  (* ============================================== *)

  Stdio.printf "Test 7: Valid signature (z=0xCAFEBABE, k=0x999)\n";

  let z = Z.of_string "0xCAFEBABE" in
  let k = Z.of_string "0x999" in

  (match sign ~z ~k with
  | None ->
      Stdio.printf "  Failed to generate signature\n";
      record false
  | Some (r, s) ->
      Stdio.printf "  Generated r = %s...\n" (String.prefix (Z.to_string r) 30);
      Stdio.printf "  Generated s = %s...\n" (String.prefix (Z.to_string s) 30);
      match run_verify ~z ~r ~s with
      | None -> record false
      | Some valid ->
          Stdio.printf "  Valid = %b (expected true)\n" valid;
          let pass = valid in
          Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
          record pass);

  (* ============================================== *)
  (* TEST 8: Completely random signature (invalid) *)
  (* ============================================== *)

  Stdio.printf "Test 8: Random values (should be invalid)\n";

  let z = Z.of_string "0x1111111111111111" in
  let r = Z.of_string "0x2222222222222222" in
  let s = Z.of_string "0x3333333333333333" in

  (match run_verify ~z ~r ~s with
  | None -> record false
  | Some valid ->
      Stdio.printf "  Valid = %b (expected false)\n" valid;
      let pass = not valid in
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
    Stdio.printf "\nAll ECDSA verification tests passed! ✓✓✓\n";
  end else begin
    Stdio.printf "\n✗ Some tests failed - review above for details\n";
    failwith "checks failed";
  end