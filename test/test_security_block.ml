(* test_security_block.ml - Security block tests with HSS/LMS verification

   Tests 14 security properties using HSS/LMS hash-based signatures.
   Test vectors from reference_lms.py.
*)

open Base
open Hardcaml

module State = Security_block.State

(* HSS test vectors *)
let identifier_hex = "01010101010101010101010101010101"
let q = 5
let c_hex = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
let merkle_root_hex = "1c2e06bc6fe6bc3f7c6e7fc998277f82fd8fa2f79aa26eb1d3c66999083d4ce5"
let tree_height = 4
let signed_message_hex = "0b98309ccea6343bf486b4b04ec7d7b7a5b5adda1edf46e43a15b5e99edc21b4"

let signature_hex = [|
  "3a480283407063ca6a57ad7201dce50eed9d7ffe3eb8eeff0d8729ab6c69138e";
  "bda0576ef69ede2a299e5909cdf76c4beb21c752584ed14aeaab9a832176ad82";
  "ad53eea01adbc6cc2843dd2339e21bc3db8d7631fafc3c90172b9e349afc00fd";
  "3b49a4edd1a859efde2412d990101c5295c43a747e0bfedaf6316ce9dfa2df37";
  "a31c08a41d14877e0900e1edfcfd9cda520d2099be46a19fdfd98314c97b7a6c";
  "ed985ecad9eff85bb39578751b35ac960605155f7087f1bd6e93c54a4c475431";
  "b0a8911e31adc20f59baed39d27cfea6263a904f57313d394870edd2fccea3c4";
  "e905292f112d21bbdf2f09e512f81e20321974d3b89f6b37a1f60dd99f7382db";
  "8646c92a0c0e4b5c483dbd8cf27b93406bcc80fb53aa252115a5542350bee06b";
  "b2e5aef13580eea74b5d923672f6cc6e7aa84a62bd90cd673a501a6897c49acd";
  "9f57d5bd1a6f74584f7f60ffea7a9d06c99377783b09868a515b884230b7c6a9";
  "b9a0e9c4dc02ea6619a0353fb0e5b30dfad2038a5b3b6736a03f97cd8de79854";
  "dc86e794edbb8a7103ab5b2837e1f4c1bab260d6399bb4d8e698bffbba5f2663";
  "ca689a94cb780d89cf76906b98c88bbd2a953d65eb9200695b8d7d937678c9f5";
  "8294eee9ee409fc48839b6eaae7f3f6dab91bdc039879333754a8b58d636929c";
  "10dd2807334d8df653995b3bab70d66f79e5ba2a5ef751992706c7aad52989f2";
  "b41f98dea875f68edffece8623e2de6225f9e2ddd8e982d5f11342371662bc51";
  "747bdb5fb0b81e3b0db585e6d0065024cf2146a5a21f76686da03e8faf850a79";
  "848544d345860ab1e2d8518a45dd26dcd924d74b088b8ef21e73d59dc996adcb";
  "a88c0b422826e37d1815eb1e6b3da2129e26c0b992fd4f0112c362a160b7b59e";
  "fe117086153eae51639f815ce11aa2df07f969b37a6712879499da1e445bcb63";
  "3eff23af41275547be3cb3021b57872ef1c9855c323bb3826afccc7a4dfbcf2c";
  "be723107964340550c5d5e79a37fbf9c893ad88060310387dfbfb12800b28080";
  "3bc358a50ac437946044353efc8922f1925dc4b05f43979fdf8e03b3045493e0";
  "771c28c482520e0fd639e0076e2c6b2d054124f90acb2690154555116e8d796f";
  "5cbfcc9e4287814f56573038875ba5539ea72c028636f55489c014cee30df046";
  "b0086d805aa6698d8d0e25dcc26e5a6924c4842ece8666fd1dfaa8feaabf236d";
  "59ccc128fc44877abbf74b50ec940f73ef1a405d36b1917e50bbcaac0c044db7";
  "8fb3c75fc722075b140236c472062befafd6e27fc8f9e9eb75cff151b29e957c";
  "75e2b540b4eee1d133180c386658833d5ebcd0655d7b3f5d53dd547c4c050d90";
  "9ff47e0c4d3410a85214ddba0b769d8af436eb54943a2e788f076d8a257d525b";
  "eb3e49cc6204bf7b4493a756bd5a3eb9551fd2009464b3161f2f7917c83049b5";
  "e7b8728e14a0c7e19b0881311b01383329e4a64a7bf88cb1de2a002c547da905";
  "ebd37232f7e264d3fce5c4731cbcfcf83a1080380f4a53d0a48a634b98632f01";
|]

let auth_path_hex = [|
  "88e8870b6c5c462d0b438df1d9ec4fbf0da2dc0968b7908c943840d19342f63b";
  "bb2e3fd7fa4a51914aec2b24dafcdf68c292f2b019021f47cc2320dbf8174bb9";
  "04e412c0e49cf794208062b8917d60d21d4410f738792bf3f3e2472a0de192ba";
  "ae749d878fe57433df878f2b4155c32cfec2358d46b64d8bca3759e5802a6a2d";
|]

let hex_to_bits ~width hex_str =
  Bits.of_hex ~signedness:Unsigned ~width hex_str

let bits_to_hex b =
  let width = Bits.width b in
  let num_nibbles = (width + 3) / 4 in
  let s = Buffer.create num_nibbles in
  let i = ref (width - 1) in
  while !i >= 0 do
    let nibble_val = ref 0 in
    for bit_pos = 3 downto 0 do
      if !i >= 0 then begin
        if Bits.to_int (Bits.select b !i !i) = 1 then
          nibble_val := !nibble_val lor (1 lsl bit_pos);
        Int.decr i
      end
    done;
    Buffer.add_char s (Printf.sprintf "%x" !nibble_val).[0]
  done;
  Buffer.contents s

(* 256-bit subtraction using Z arithmetic *)
let bits_sub_int b n =
  let z = Bits.to_z ~signedness:Unsigned b in
  let result = Z.(z - of_int n) in
  let hex = Z.format "%064x" result in
  hex_to_bits ~width:256 hex

let () =
  Stdio.printf "=== Security Block Test (HSS/LMS) ===\n\n";

  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Security_block.I)(Security_block.O) in
  let sim = Sim.create (Security_block.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in

  let int8_to_bits i = Bits.of_int ~width:8 (i land 0xFF) in
  let bits_to_int8 b =
    let v = Bits.to_int b in
    if v > 127 then v - 256 else v
  in
  let bits_to_int64 b = Bits.to_int64 b in

  let reset () =
    inputs.clear := Bits.vdd;
    inputs.license_submit := Bits.gnd;
    inputs.license_leaf_index := Bits.zero 32;
    inputs.license_randomizer := Bits.zero 256;
    inputs.sig_element := Bits.zero 256;
    inputs.sig_element_valid := Bits.gnd;
    inputs.auth_node := Bits.zero 256;
    inputs.auth_node_valid := Bits.gnd;
    inputs.root_pub_key := hex_to_bits ~width:256 merkle_root_hex;
    inputs.identifier := hex_to_bits ~width:128 identifier_hex;
    inputs.tree_height := Bits.of_int ~width:6 tree_height;
    inputs.workload_valid := Bits.gnd;
    inputs.int8_a := Bits.zero 8;
    inputs.int8_b := Bits.zero 8;
    inputs.trng_seed := Bits.zero 256;
    inputs.trng_load_seed := Bits.gnd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in

  let get_allowance () = bits_to_int64 !(outputs.allowance) in
  let get_enabled () = Bits.to_bool !(outputs.enabled) in
  let get_nonce_ready () = Bits.to_bool !(outputs.nonce_ready) in
  let get_state () = List.nth_exn State.all (Bits.to_int !(outputs.state_debug)) in
  let get_licenses_accepted () = Bits.to_int !(outputs.licenses_accepted) in
  let state_to_string s = Sexp.to_string (State.sexp_of_t s) in

  let wait_for_nonce_ready ~max_cycles =
    let rec loop n =
      if n >= max_cycles then begin
        Stdio.printf "    TIMEOUT waiting for nonce_ready after %d cycles\n" max_cycles;
        None
      end else if get_nonce_ready () then begin
        Stdio.printf "    nonce_ready after %d cycles\n" n;
        Some ()
      end else begin
        Cyclesim.cycle sim;
        loop (n + 1)
      end
    in
    loop 0
  in

  (* Calibrate TRNG offset: load seed=0, measure nonce = offset *)
  Stdio.printf "Calibrating TRNG offset...\n";
  reset ();
  inputs.trng_seed := Bits.zero 256;
  inputs.trng_load_seed := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.trng_load_seed := Bits.gnd;
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None -> failwith "TRNG calibration failed"
  | Some () -> ());
  let trng_offset = Bits.to_int !(outputs.nonce) in
  Stdio.printf "  TRNG offset = %d\n\n" trng_offset;

  (* Compute the seed that produces signed_message as the nonce *)
  let calibrated_seed = bits_sub_int (hex_to_bits ~width:256 signed_message_hex) trng_offset in

  let setup_trng () =
    inputs.trng_seed := calibrated_seed;
    inputs.trng_load_seed := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.trng_load_seed := Bits.gnd
  in

  let submit_hss_license ~leaf_index ~randomizer_hex ~sig_hex ~auth_hex =
    inputs.license_submit := Bits.vdd;
    inputs.license_leaf_index := Bits.of_int ~width:32 leaf_index;
    inputs.license_randomizer := hex_to_bits ~width:256 randomizer_hex;
    Cyclesim.cycle sim;
    inputs.license_submit := Bits.gnd;

    let max_cycles = 2_000_000 in
    let rec loop n last_state =
      if n >= max_cycles then begin
        Stdio.printf "    TIMEOUT waiting for verification after %d cycles\n" max_cycles;
        None
      end else begin
        let current_state = get_state () in
        if (State.equal last_state Verify_wait) && not (State.equal current_state Verify_wait) then begin
          Stdio.printf "    Verification completed in %d cycles\n" n;
          Some current_state
        end else begin
          if Bits.to_bool !(outputs.request_sig_element) then begin
            let idx = Bits.to_int !(outputs.sig_element_index) in
            if idx < Array.length sig_hex then begin
              inputs.sig_element := hex_to_bits ~width:256 sig_hex.(idx);
              inputs.sig_element_valid := Bits.vdd;
              Cyclesim.cycle sim;
              inputs.sig_element_valid := Bits.gnd;
            end else
              Cyclesim.cycle sim
          end
          else if Bits.to_bool !(outputs.request_auth_node) then begin
            let level = Bits.to_int !(outputs.auth_level) in
            if level < Array.length auth_hex then begin
              inputs.auth_node := hex_to_bits ~width:256 auth_hex.(level);
              inputs.auth_node_valid := Bits.vdd;
              Cyclesim.cycle sim;
              inputs.auth_node_valid := Bits.gnd;
            end else
              Cyclesim.cycle sim
          end
          else
            Cyclesim.cycle sim;
          loop (n + 1) current_state
        end
      end
    in
    loop 0 (get_state ())
  in

  let submit_valid_license () =
    submit_hss_license ~leaf_index:q ~randomizer_hex:c_hex
      ~sig_hex:signature_hex ~auth_hex:auth_path_hex
  in

  let do_workload ~a ~b =
    inputs.workload_valid := Bits.vdd;
    inputs.int8_a := int8_to_bits a;
    inputs.int8_b := int8_to_bits b;
    Cyclesim.cycle sim;
    inputs.workload_valid := Bits.gnd;
    Cyclesim.cycle sim;
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
  Stdio.printf "  Initial allowance = %Ld (expected 0)\n" initial_allowance;
  Stdio.printf "  Initial enabled = %b (expected false)\n" initial_enabled;
  let pass = (Int64.(=) initial_allowance 0L) && (not initial_enabled) in
  Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
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
  Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
  record pass;

  (* ============================================== *)
  (* TEST 3: State machine reaches Publish state   *)
  (* ============================================== *)

  Stdio.printf "Test 3: State machine reaches Publish state with valid nonce\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      let nonce_hex = bits_to_hex !(outputs.nonce) in
      Stdio.printf "  Nonce = %s\n" nonce_hex;
      Stdio.printf "  Expected = %s\n" signed_message_hex;
      Stdio.printf "  Match = %b\n" (String.equal nonce_hex signed_message_hex);
      let pass = get_nonce_ready () && String.equal nonce_hex signed_message_hex in
      Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
      record pass);

  (* ============================================== *)
  (* TEST 4: Valid license increments allowance    *)
  (* ============================================== *)

  Stdio.printf "Test 4: Valid HSS license increments allowance\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      let allowance_before = get_allowance () in
      let accepted_before = get_licenses_accepted () in
      (match submit_valid_license () with
      | None -> record false
      | Some new_state ->
          let allowance_after = get_allowance () in
          let accepted_after = get_licenses_accepted () in
          Stdio.printf "  Allowance before = %Ld\n" allowance_before;
          Stdio.printf "  Allowance after = %Ld\n" allowance_after;
          Stdio.printf "  Licenses accepted: %d -> %d\n" accepted_before accepted_after;
          Stdio.printf "  New state = %s (expected Request_nonce)\n" (state_to_string new_state);
          let pass = Int64.(>) allowance_after allowance_before
                    && (accepted_after = accepted_before + 1)
                    && (State.equal new_state Request_nonce) in
          Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
          record pass));

  (* ============================================== *)
  (* TEST 5: Workload works after valid license    *)
  (* ============================================== *)

  Stdio.printf "Test 5: Workload works after valid license\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      (match submit_valid_license () with
      | None -> record false
      | Some _ ->
          (match wait_for_nonce_ready ~max_cycles:200 with
          | None -> record false
          | Some () ->
              let enabled_now = get_enabled () in
              Stdio.printf "  Enabled = %b (expected true)\n" enabled_now;
              let (result, valid) = do_workload ~a:10 ~b:20 in
              Stdio.printf "  Workload: 10 + 20\n";
              Stdio.printf "  Result = %d (expected 30)\n" result;
              Stdio.printf "  Valid = %b\n" valid;
              let pass = (result = 30) && valid && enabled_now in
              Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
              record pass)));

  (* ============================================== *)
  (* TEST 6: Invalid license - same nonce retained *)
  (* ============================================== *)

  Stdio.printf "Test 6: Invalid license does not increment allowance\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      let allowance_before = get_allowance () in
      Stdio.printf "  Submitting invalid HSS signature (wrong randomizer)\n";
      let wrong_c = "0000000000000000000000000000000000000000000000000000000000000000" in
      (match submit_hss_license ~leaf_index:q ~randomizer_hex:wrong_c
               ~sig_hex:signature_hex ~auth_hex:auth_path_hex with
      | None -> record false
      | Some new_state ->
          let allowance_after = get_allowance () in
          Stdio.printf "  Allowance before = %Ld\n" allowance_before;
          Stdio.printf "  Allowance after = %Ld\n" allowance_after;
          Stdio.printf "  New state = %s (expected Publish)\n" (state_to_string new_state);
          let pass = Int64.(=) allowance_after allowance_before
                    && (State.equal new_state Publish) in
          Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
          record pass));

  (* ============================================== *)
  (* TEST 7: Signed Int8 addition - positive       *)
  (* ============================================== *)

  Stdio.printf "Test 7: Signed Int8 addition - positive values\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      (match submit_valid_license () with
      | None -> record false
      | Some _ ->
          (match wait_for_nonce_ready ~max_cycles:200 with
          | None -> record false
          | Some () ->
              let (result, _) = do_workload ~a:50 ~b:30 in
              Stdio.printf "  Workload: 50 + 30 = %d (expected 80)\n" result;
              let pass = (result = 80) in
              Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
              record pass)));

  (* ============================================== *)
  (* TEST 8: Signed Int8 addition - negative       *)
  (* ============================================== *)

  Stdio.printf "Test 8: Signed Int8 addition - negative values\n";
  let (result, _) = do_workload ~a:(-10) ~b:(-20) in
  Stdio.printf "  Workload: -10 + -20 = %d (expected -30)\n" result;
  let pass = (result = -30) in
  Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
  record pass;

  (* ============================================== *)
  (* TEST 9: Signed Int8 addition - mixed signs    *)
  (* ============================================== *)

  Stdio.printf "Test 9: Signed Int8 addition - mixed signs\n";
  let (result, _) = do_workload ~a:100 ~b:(-30) in
  Stdio.printf "  Workload: 100 + -30 = %d (expected 70)\n" result;
  let pass = (result = 70) in
  Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
  record pass;

  (* ============================================== *)
  (* TEST 10: Signed Int8 addition - wrapping      *)
  (* ============================================== *)

  Stdio.printf "Test 10: Signed Int8 addition - overflow wrapping\n";
  let (result, _) = do_workload ~a:127 ~b:1 in
  Stdio.printf "  Workload: 127 + 1 = %d (expected -128 due to wrapping)\n" result;
  let pass = (result = -128) in
  Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
  record pass;

  (* ============================================== *)
  (* TEST 11: Allowance decrements each cycle      *)
  (* ============================================== *)

  Stdio.printf "Test 11: Allowance decrements each clock cycle\n";
  let allowance_before = get_allowance () in
  for _ = 1 to 100 do
    Cyclesim.cycle sim
  done;
  let allowance_after = get_allowance () in
  let decrement = Int64.(-) allowance_before allowance_after in
  Stdio.printf "  Allowance before = %Ld\n" allowance_before;
  Stdio.printf "  Allowance after = %Ld\n" allowance_after;
  Stdio.printf "  Decrement over 100 cycles = %Ld (expected 100)\n" decrement;
  let pass = Int64.(=) decrement 100L in
  Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
  record pass;

  (* ============================================== *)
  (* TEST 12: New nonce after valid license only   *)
  (* ============================================== *)

  Stdio.printf "Test 12: New nonce generated after valid license only\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      let nonce1 = bits_to_hex !(outputs.nonce) in
      Stdio.printf "  First nonce = %s...\n" (String.prefix nonce1 32);
      (match submit_valid_license () with
      | None -> record false
      | Some _ ->
          (match wait_for_nonce_ready ~max_cycles:200 with
          | None -> record false
          | Some () ->
              let nonce2 = bits_to_hex !(outputs.nonce) in
              Stdio.printf "  Second nonce = %s...\n" (String.prefix nonce2 32);
              let pass = not (String.equal nonce1 nonce2) in
              Stdio.printf "  Nonces different = %b (expected true)\n" pass;
              Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
              record pass)));

  (* ============================================== *)
  (* TEST 13: License with wrong leaf_index fails  *)
  (* ============================================== *)

  Stdio.printf "Test 13: License with wrong leaf index is rejected\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      let allowance_before = get_allowance () in
      (match submit_hss_license ~leaf_index:3 ~randomizer_hex:c_hex
               ~sig_hex:signature_hex ~auth_hex:auth_path_hex with
      | None -> record false
      | Some new_state ->
          let allowance_after = get_allowance () in
          Stdio.printf "  Allowance before = %Ld\n" allowance_before;
          Stdio.printf "  Allowance after = %Ld\n" allowance_after;
          Stdio.printf "  New state = %s (expected Publish)\n" (state_to_string new_state);
          let pass = Int64.(=) allowance_after allowance_before
                    && (State.equal new_state Publish) in
          Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
          record pass));

  (* ============================================== *)
  (* TEST 14: Cannot replay same license twice     *)
  (* ============================================== *)

  Stdio.printf "Test 14: Cannot replay same valid license twice\n";
  reset ();
  setup_trng ();
  (match wait_for_nonce_ready ~max_cycles:200 with
  | None ->
      Stdio.printf "  Failed to reach Publish state\n";
      record false
  | Some () ->
      let accepted_before = get_licenses_accepted () in
      (* First submission: valid *)
      (match submit_valid_license () with
      | None -> record false
      | Some _ ->
          let accepted_after_first = get_licenses_accepted () in
          Stdio.printf "  First submission: accepted = %b\n" (accepted_after_first > accepted_before);
          (* After valid license, nonce changes. Replay with old signature fails. *)
          (match wait_for_nonce_ready ~max_cycles:200 with
          | None -> record false
          | Some () ->
              let nonce2 = bits_to_hex !(outputs.nonce) in
              Stdio.printf "  New nonce = %s...\n" (String.prefix nonce2 32);
              Stdio.printf "  Attempting to replay same signature...\n";
              (match submit_valid_license () with
              | None -> record false
              | Some new_state ->
                  let accepted_after_second = get_licenses_accepted () in
                  Stdio.printf "  Second submission: accepted = %b\n"
                    (accepted_after_second > accepted_after_first);
                  Stdio.printf "  New state = %s\n" (state_to_string new_state);
                  let pass = (accepted_after_first = accepted_before + 1)
                            && (accepted_after_second = accepted_after_first)
                            && (State.equal new_state Publish) in
                  Stdio.printf "  Replay rejected = %b (expected true)\n" pass;
                  Stdio.printf "  %s\n\n" (if pass then "PASS" else "FAIL");
                  record pass))));

  (* ============================================== *)
  (* TEST SUMMARY                                  *)
  (* ============================================== *)

  let results_list = List.rev !results in
  let passed = List.count results_list ~f:Fn.id in
  let total = List.length results_list in

  Stdio.printf "=== Test Summary ===\n";
  Stdio.printf "Passed: %d/%d\n" passed total;

  if passed = total then
    Stdio.printf "\nAll Security Block tests passed!\n"
  else begin
    Stdio.printf "\nSome tests failed - review above for details\n";
    failwith "checks failed";
  end
