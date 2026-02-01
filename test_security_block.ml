(* test_security_block.ml *)

open Base
open Hardcaml

let () =
  Stdio.printf "=== Security Block Test ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Security_block.I)(Security_block.O) in
  let sim = Sim.create (Security_block.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let prime_n = Arith.Config.prime_n in
  
  (* secp256k1 curve parameters *)
  let param_a = Z.zero in
  let param_b3 = Z.of_int 21 in
  
  (* Generator point G *)
  let g_x = Z.of_string "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" in
  let g_y = Z.of_string "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8" in
  
  (* Private key d = 2, Public key Q = 2G (must match ECDSA module's hardcoded Q) *)
  let private_key = Z.of_int 2 in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in
  
  let int8_to_bits i =
    Bits.of_int ~width:8 (i land 0xFF)
  in
  
  let bits_to_int8 b =
    let v = Bits.to_int b in
    (* Sign extend if negative *)
    if v > 127 then v - 256 else v
  in
  
  (* Modular arithmetic helpers *)
  let mod_inv_n a = Z.invert a prime_n in
  let mod_mul_n a b = Z.((a * b) mod prime_n) in
  let mod_add_n a b = Z.((a + b) mod prime_n) in
  let prime_p = Arith.Config.prime_p in
  let mod_mul_p a b = Z.((a * b) mod prime_p) in
  let mod_inv_p a = Z.invert a prime_p in
  
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
    match scalar_mult k (g_x, g_y) with
    | None -> None
    | Some (rx, _ry) ->
        let r = Z.(rx mod prime_n) in
        if Z.equal r Z.zero then None
        else
          let k_inv = mod_inv_n k in
          let s = mod_mul_n k_inv (mod_add_n z (mod_mul_n r private_key)) in
          if Z.equal s Z.zero then None
          else Some (r, s)
  in
  
  let reset () =
    inputs.clear := Bits.vdd;
    inputs.license_valid := Bits.gnd;
    inputs.license_r := Bits.zero 256;
    inputs.license_s := Bits.zero 256;
    inputs.workload_valid := Bits.gnd;
    inputs.int8_a := Bits.zero 8;
    inputs.int8_b := Bits.zero 8;
    inputs.param_a := z_to_bits param_a;
    inputs.param_b3 := z_to_bits param_b3;
    inputs.trng_seed := Bits.zero 256;
    inputs.trng_load_seed := Bits.gnd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in
  
  let cycle ?(n=1) () =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  
  let get_allowance () =
    Bits.to_int !(outputs.allowance)
  in
  
  let get_enabled () =
    Bits.to_bool !(outputs.enabled)
  in
  
  let get_nonce () =
    Bits.to_z ~signedness:Unsigned !(outputs.nonce)
  in
  
  let get_nonce_ready () =
    Bits.to_bool !(outputs.nonce_ready)
  in
  
  let get_state () =
    Bits.to_int !(outputs.state_debug)
  in
  
  let get_licenses_accepted () =
    Bits.to_int !(outputs.licenses_accepted)
  in
  
  let get_licenses_rejected () =
    Bits.to_int !(outputs.licenses_rejected)
  in
  
  let state_name state =
    match state with
    | 0 -> "Init_delay"
    | 1 -> "Request_nonce"
    | 2 -> "Wait_nonce"
    | 3 -> "Publish"
    | 4 -> "Verify_start"
    | 5 -> "Verify_wait"
    | 6 -> "Update"
    | _ -> "Unknown"
  in
  
  (* Wait for nonce_ready to go high *)
  let wait_for_nonce_ready ~max_cycles =
    let rec loop n =
      if n >= max_cycles then begin
        Stdio.printf "    TIMEOUT waiting for nonce_ready after %d cycles\n" max_cycles;
        None
      end else if get_nonce_ready () then begin
        Stdio.printf "    nonce_ready after %d cycles\n" n;
        Some (get_nonce ())
      end else begin
        Cyclesim.cycle sim;
        loop (n + 1)
      end
    in
    loop 0
  in
  
  (* Submit a license and wait for verification to complete *)
  let submit_license ~r ~s =
    inputs.license_valid := Bits.vdd;
    inputs.license_r := z_to_bits r;
    inputs.license_s := z_to_bits s;
    Cyclesim.cycle sim;
    inputs.license_valid := Bits.gnd;
    inputs.license_r := Bits.zero 256;
    inputs.license_s := Bits.zero 256;
    
    (* Wait for state machine to return to Publish (state 3) *)
    let max_cycles = 5_000_000 in
    let rec loop n =
      if n >= max_cycles then begin
        Stdio.printf "    TIMEOUT waiting for verification after %d cycles\n" max_cycles;
        false
      end else if get_state () = 3 then begin  (* Back to Publish *)
        Stdio.printf "    Verification completed in %d cycles\n" n;
        true
      end else begin
        Cyclesim.cycle sim;
        loop (n + 1)
      end
    in
    loop 0
  in
  
  (* Submit workload and get result *)
  let do_workload ~a ~b =
    inputs.workload_valid := Bits.vdd;
    inputs.int8_a := int8_to_bits a;
    inputs.int8_b := int8_to_bits b;
    Cyclesim.cycle sim;
    inputs.workload_valid := Bits.gnd;
    (* Result available after one cycle due to pipeline register *)
    let result = bits_to_int8 !(outputs.int8_result) in
    let valid = Bits.to_bool !(outputs.result_valid) in
    (result, valid)
  in
  
  let results = ref [] in
  let record result = results := result :: !results in
  
  (* ============================================== *)
  (* TEST 1: Initial state - allowance is 0        *)
  (* ============================================== *)
  
  Stdio.printf "Test 1: Initial state - allowance is 0, workload blocked\n";
  
  reset ();
  
  let initial_allowance = get_allowance () in
  let initial_enabled = get_enabled () in
  
  Stdio.printf "  Initial allowance = %d (expected 0)\n" initial_allowance;
  Stdio.printf "  Initial enabled = %b (expected false)\n" initial_enabled;
  
  let pass = (initial_allowance = 0) && (not initial_enabled) in
  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
  record pass;
  
  (* ============================================== *)
  (* TEST 2: Workload blocked when allowance = 0   *)
  (* ============================================== *)
  
  Stdio.printf "Test 2: Workload blocked when allowance = 0\n";
  
  reset ();
  
  let (result, valid) = do_workload ~a:10 ~b:20 in
  
  Stdio.printf "  Workload: 10 + 20\n";
  Stdio.printf "  Result = %d (expected 0 due to gating)\n" result;
  Stdio.printf "  Valid = %b\n" valid;
  
  let pass = (result = 0) in
  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
  record pass;
  
  (* ============================================== *)
  (* TEST 3: State machine reaches Publish state   *)
  (* ============================================== *)
  
  Stdio.printf "Test 3: State machine reaches Publish state with valid nonce\n";
  
  reset ();
  
  (* Set a known seed for deterministic testing *)
  inputs.trng_seed := z_to_bits (Z.of_int 42);
  inputs.trng_load_seed := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.trng_load_seed := Bits.gnd;
  
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None -> 
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some nonce ->
      Stdio.printf "  Nonce = %s\n" (Z.to_string nonce);
      Stdio.printf "  State = %s\n" (state_name (get_state ()));
      let pass = get_nonce_ready () in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass);
  
  (* ============================================== *)
  (* TEST 4: Valid license increments allowance    *)
  (* ============================================== *)
  
  Stdio.printf "Test 4: Valid license increments allowance\n";
  
  reset ();
  
  (* Set a known seed *)
  inputs.trng_seed := z_to_bits (Z.of_int 12345);
  inputs.trng_load_seed := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.trng_load_seed := Bits.gnd;
  
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some nonce ->
      Stdio.printf "  Nonce (z) = %s\n" (Z.to_string nonce);
      
      let allowance_before = get_allowance () in
      let accepted_before = get_licenses_accepted () in
      
      (* Sign the nonce *)
      let k = Z.of_int 7 in  (* Signing nonce *)
      (match sign ~z:nonce ~k with
      | None ->
          Stdio.printf "  Failed to generate signature\n";
          record false
      | Some (r, s) ->
          Stdio.printf "  Generated r = %s...\n" (String.prefix (Z.to_string r) 30);
          Stdio.printf "  Generated s = %s...\n" (String.prefix (Z.to_string s) 30);
          
          if submit_license ~r ~s then begin
            let allowance_after = get_allowance () in
            let accepted_after = get_licenses_accepted () in
            
            Stdio.printf "  Allowance before = %d\n" allowance_before;
            Stdio.printf "  Allowance after = %d\n" allowance_after;
            Stdio.printf "  Licenses accepted: %d -> %d\n" accepted_before accepted_after;
            
            let pass = (allowance_after > allowance_before) && (accepted_after = accepted_before + 1) in
            Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
            record pass
          end else
            record false));
  
  (* ============================================== *)
  (* TEST 5: Workload works after valid license    *)
  (* ============================================== *)
  
  Stdio.printf "Test 5: Workload works after valid license\n";
  
  (* Continue from previous state where allowance > 0 *)
  let enabled_now = get_enabled () in
  Stdio.printf "  Enabled = %b (expected true)\n" enabled_now;
  
  let (result, valid) = do_workload ~a:10 ~b:20 in
  
  Stdio.printf "  Workload: 10 + 20\n";
  Stdio.printf "  Result = %d (expected 30)\n" result;
  Stdio.printf "  Valid = %b\n" valid;
  
  let pass = (result = 30) && valid && enabled_now in
  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
  record pass;
  
  (* ============================================== *)
  (* TEST 6: Invalid license does not increment    *)
  (* ============================================== *)
  
  Stdio.printf "Test 6: Invalid license does not increment allowance\n";
  
  reset ();
  
  (* Set a known seed *)
  inputs.trng_seed := z_to_bits (Z.of_int 99999);
  inputs.trng_load_seed := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.trng_load_seed := Bits.gnd;
  
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some nonce ->
      Stdio.printf "  Nonce (z) = %s\n" (Z.to_string nonce);
      
      let allowance_before = get_allowance () in
      let rejected_before = get_licenses_rejected () in
      
      (* Submit a wrong signature *)
      let wrong_r = Z.of_int 11111 in
      let wrong_s = Z.of_int 22222 in
      
      Stdio.printf "  Submitting invalid signature (r=11111, s=22222)\n";
      
      if submit_license ~r:wrong_r ~s:wrong_s then begin
        let allowance_after = get_allowance () in
        let rejected_after = get_licenses_rejected () in
        
        Stdio.printf "  Allowance before = %d\n" allowance_before;
        Stdio.printf "  Allowance after = %d\n" allowance_after;
        Stdio.printf "  Licenses rejected: %d -> %d\n" rejected_before rejected_after;
        
        let pass = (allowance_after = allowance_before) && (rejected_after = rejected_before + 1) in
        Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
        record pass
      end else
        record false);
  
  (* ============================================== *)
  (* TEST 7: Signed Int8 addition - positive       *)
  (* ============================================== *)
  
  Stdio.printf "Test 7: Signed Int8 addition - positive values\n";
  
  reset ();
  
  (* First get a valid license *)
  inputs.trng_seed := z_to_bits (Z.of_int 777);
  inputs.trng_load_seed := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.trng_load_seed := Bits.gnd;
  
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some nonce ->
      let k = Z.of_int 13 in
      (match sign ~z:nonce ~k with
      | None ->
          Stdio.printf "  Failed to generate signature\n";
          record false
      | Some (r, s) ->
          if submit_license ~r ~s then begin
            let (result, _) = do_workload ~a:50 ~b:30 in
            Stdio.printf "  Workload: 50 + 30 = %d (expected 80)\n" result;
            let pass = (result = 80) in
            Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
            record pass
          end else
            record false));
  
  (* ============================================== *)
  (* TEST 8: Signed Int8 addition - negative       *)
  (* ============================================== *)
  
  Stdio.printf "Test 8: Signed Int8 addition - negative values\n";
  
  (* Continue with existing allowance *)
  let (result, _) = do_workload ~a:(-10) ~b:(-20) in
  Stdio.printf "  Workload: -10 + -20 = %d (expected -30)\n" result;
  let pass = (result = -30) in
  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
  record pass;
  
  (* ============================================== *)
  (* TEST 9: Signed Int8 addition - mixed signs    *)
  (* ============================================== *)
  
  Stdio.printf "Test 9: Signed Int8 addition - mixed signs\n";
  
  let (result, _) = do_workload ~a:100 ~b:(-30) in
  Stdio.printf "  Workload: 100 + -30 = %d (expected 70)\n" result;
  let pass = (result = 70) in
  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
  record pass;
  
  (* ============================================== *)
  (* TEST 10: Signed Int8 addition - wrapping      *)
  (* ============================================== *)
  
  Stdio.printf "Test 10: Signed Int8 addition - overflow wrapping\n";
  
  let (result, _) = do_workload ~a:127 ~b:1 in
  Stdio.printf "  Workload: 127 + 1 = %d (expected -128 due to wrapping)\n" result;
  let pass = (result = -128) in
  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
  record pass;
  
  (* ============================================== *)
  (* TEST 11: Allowance decrements with workload   *)
  (* ============================================== *)
  
  Stdio.printf "Test 11: Allowance decrements with workload\n";
  
  let allowance_before = get_allowance () in
  
  (* Do several workload operations *)
  for _ = 1 to 10 do
    let _ = do_workload ~a:1 ~b:1 in ()
  done;
  
  let allowance_after = get_allowance () in
  
  Stdio.printf "  Allowance before = %d\n" allowance_before;
  Stdio.printf "  Allowance after = %d\n" allowance_after;
  Stdio.printf "  Decrement = %d (expected ~10)\n" (allowance_before - allowance_after);
  
  (* Allow some tolerance due to pipeline timing *)
  let pass = (allowance_before - allowance_after >= 9) && (allowance_before - allowance_after <= 11) in
  Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
  record pass;
  
  (* ============================================== *)
  (* TEST 12: New nonce after license processing   *)
  (* ============================================== *)
  
  Stdio.printf "Test 12: New nonce generated after license processing\n";
  
  reset ();
  
  inputs.trng_seed := z_to_bits (Z.of_int 1000);
  inputs.trng_load_seed := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.trng_load_seed := Bits.gnd;
  
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some nonce1 ->
      Stdio.printf "  First nonce = %s\n" (Z.to_string nonce1);
      
      let k = Z.of_int 17 in
      (match sign ~z:nonce1 ~k with
      | None ->
          Stdio.printf "  Failed to generate signature\n";
          record false
      | Some (r, s) ->
          if submit_license ~r ~s then begin
            (* After verification, should have new nonce *)
            let nonce2 = get_nonce () in
            Stdio.printf "  Second nonce = %s\n" (Z.to_string nonce2);
            
            let pass = not (Z.equal nonce1 nonce2) in
            Stdio.printf "  Nonces different = %b (expected true)\n" pass;
            Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
            record pass
          end else
            record false));
  
  (* ============================================== *)
  (* TEST 13: License for wrong nonce is rejected  *)
  (* ============================================== *)
  
  Stdio.printf "Test 13: License signed for wrong nonce is rejected\n";
  
  reset ();
  
  inputs.trng_seed := z_to_bits (Z.of_int 5000);
  inputs.trng_load_seed := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.trng_load_seed := Bits.gnd;
  
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some nonce ->
      Stdio.printf "  Actual nonce = %s\n" (Z.to_string nonce);
      
      let allowance_before = get_allowance () in
      
      (* Sign a DIFFERENT nonce *)
      let wrong_nonce = Z.of_int 9999 in
      let k = Z.of_int 7 in
      Stdio.printf "  Signing wrong nonce = %s\n" (Z.to_string wrong_nonce);
      
      (match sign ~z:wrong_nonce ~k with
      | None ->
          Stdio.printf "  Failed to generate signature\n";
          record false
      | Some (r, s) ->
          if submit_license ~r ~s then begin
            let allowance_after = get_allowance () in
            
            Stdio.printf "  Allowance before = %d\n" allowance_before;
            Stdio.printf "  Allowance after = %d\n" allowance_after;
            
            let pass = (allowance_after = allowance_before) in
            Stdio.printf "  Allowance unchanged = %b (expected true)\n" pass;
            Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
            record pass
          end else
            record false));
  
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
    Stdio.printf "\nAll Security Block tests passed! ✓✓✓\n"
  end else
    Stdio.printf "\n✗ Some tests failed - review above for details\n"