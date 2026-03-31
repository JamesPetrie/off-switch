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
let merkle_root_hex = "ae247eab7668d68995dfd7ad9c8fa2fc9c3ab078ea2a68b5e3013346ed394a37"
let tree_height = 4
let signed_message_hex = "0b98309ccea6343bf486b4b04ec7d7b7a5b5adda1edf46e43a15b5e99edc21b4"

let signature_hex = [|
  "0e2de492299e8156dfacbf1ec03bbcb52599ede943788bed3920d55728eb1e34";
  "82c3921882f45aa372c5a0a179026a37ab37c4d38eaf362c3bd00c66fd9334df";
  "adba34ba09cd0ccfa87b06db382ae56c01c33dda9353101b97ebed53cd72f34b";
  "ac7e002764095bde7aad441861a35c1b43160707c7c694c43cadd512ff2ee2e3";
  "717fa7feab85a0530ff203c357d7f4dfea8718aa1140ac4483bb0bd4b53aea38";
  "d7c241fd4ccc716ccaf74d875f412cf36abd0d8904ade866ad80f145ec5407d0";
  "b29c3dcc944f13f1b1f070c83924a0582896473d5844f5694c6a1cf5d6a866f0";
  "8ca6470cc80da05012cc7afd833a77c529824c9e3927264585b864259b3ece80";
  "cdbf66f9838c333acb89393b2b91071f12fd4ff440adc971e9100296201c9c5c";
  "65ba8116a0e1a92be8218d5706bb47487f4f2e3e03002e9f6d6f481542e1c9df";
  "da62bd591104856f9a2ec77bcf0aa8cf70c6dd2ab27412ac56717a45372bf441";
  "c05fab0cac40f5ce84d4666c392efe7199dd204f075a7ded2a7a0523960240bc";
  "f26762ea763c1733a1a30a7af78230182e38d8abbfa0e7d1337f66aee3eba688";
  "ad46cdfded589a821ec611c38544f3e2d3aa79e20c9caf06e46365d65d2b9d3a";
  "7c3307dcf2d58ed3de594d77cfafd07f955f8e935a60c04e27e272aa5a403721";
  "e8ea5539f2e2a97385fc67ee58e06452f5814927784fd9994882d5d88bd1ff6e";
  "4bb1d2e0943b956aacdcdca044bef49302a95760f094ebd55b9848149c129917";
  "2d9d6a92ffebbf39a75f9cb06ded43e6b457d0ddfa0f7fd19f68d8c2fc269bd1";
  "5248aca1d28ba43e8d2f06c609bffa700e1355860209b1b5fd9df89274f61548";
  "4b989de6a2f1375c356414a0390b429c87ac31e297354bfa550c7ca97b13d9e8";
  "1cf524de022aa1d3ccd6655797dd99e692e9471cc898765d3bdbd0394b6f827f";
  "5ecb607fdd644fe1e7054aee620ed3866d5d8c5246d93b3cc4321ac3bacc2b56";
  "999497c46e6a987771e44851fc89fdbc7c16c70535aec9d74d3d9f476cca50bc";
  "c448b625002da22f57385838a0af7ff19c9f1b2949552e08f9e7028f43e1b948";
  "13db9981798fd544385e44334390434bcd07885f527829557eefefc105f324d8";
  "6940fe1e194db390947835f5596529a9b950078f902d77408e979f2621333ec5";
  "5b3e28e058b54acaa76341672dbe9c823ba8194af396ab8d1cffd58f108c67e2";
  "f71544dd12ec9659b579871d341b0e48e4de10e4ed84d3c001f1e3e2021c1555";
  "96d08adc070a0c963057a63ac6b6550c5cff32470c25d945ba541ca9bb36f14b";
  "4ec09ccc34246687955f05cb00b899c97a81027e1168432a127061ce23c65461";
  "b6b414251cf1e68a90e59d6d69873cde55a8a27263a580522743f2658094920b";
  "34f7c494ecdf5bdb3b691d4e0bb7b1719b89b44c7305b18afb00f58a3c1777e6";
  "6716bdca8af8d614d4fb002009ebdbb988a893ce3bc92d168df119a99e467366";
  "5699094123021c7920326789738bb988fee851655a8f81572b239e77bddbe5cc";
|]

let auth_path_hex = [|
  "16a07337b0e9698fba159da2615cd486c9aff5c3c158a17afb83c5fc7a7893f8";
  "73606768f67c693edc536ff1c75f233ee9e597bc83dc089d9acdb8c772ec449c";
  "71cb251154f21ba3ce401ffbc63c430ea98eccf5a3b9b7f3a70e872958ba7ea2";
  "66f7d9a0d2c34f8eeedd0e73a75b39518bbfed788ebe83103bcc97b8cb1b09a2";
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
