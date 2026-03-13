open Base
open Hardcaml

module ModInvWithModAdd = struct
  module I = Mod_inv.ModInv.I
  module O = Mod_inv.ModInv.O

  let create scope (i : _ I.t) =
    let mod_add_result_w   = Signal.wire Mod_inv.Config.width in
    let mod_add_ready_w    = Signal.wire 1 in
    let mod_add_adjusted_w = Signal.wire 1 in

    let inv_out = Mod_inv.ModInv.create (Scope.sub_scope scope "mod_inv")
      { i with
        mod_add_result   = mod_add_result_w
      ; mod_add_ready    = mod_add_ready_w
      ; mod_add_adjusted = mod_add_adjusted_w
      }
    in

    let add_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_add")
      { Mod_add.ModAdd.I.
        clock    = i.clock
      ; clear    = i.clear
      ; valid    = inv_out.mod_add_valid
      ; a        = inv_out.mod_add_a
      ; b        = inv_out.mod_add_b
      ; modulus  = i.modulus
      ; subtract = inv_out.mod_add_subtract
      }
    in

    Signal.assign mod_add_result_w   add_out.result;
    Signal.assign mod_add_ready_w    add_out.ready;
    Signal.assign mod_add_adjusted_w add_out.adjusted;

    inv_out
end

let test () =
  Stdio.printf "=== ModInv Hardware Test (256-bit with Zarith) ===\n";
  Stdio.printf "=== Assuming odd prime modulus ===\n\n";

  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(ModInvWithModAdd.I)(ModInvWithModAdd.O) in
  let sim = Sim.create (ModInvWithModAdd.create scope) in

  let vcd_file = "./test_mod_inv.vcd" in
  let oc = Stdio.Out_channel.create vcd_file in
  let sim = Vcd.wrap oc sim in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((Mod_inv.Config.width + 3) / 4) ~char:'0' in
    Bits.of_hex ~width:Mod_inv.Config.width padded
  in

  let test_case name x_z mod_z should_have_inverse expected_result_z_opt =
    Stdio.printf "Test: %s\n" name;
    Stdio.printf "  x       = %s\n" (Z.to_string x_z);
    Stdio.printf "  modulus = %s\n" (Z.to_string mod_z);

    let x_bits = z_to_bits x_z in
    let mod_bits = z_to_bits mod_z in

    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.x := Bits.zero Mod_inv.Config.width;
    inputs.modulus := Bits.zero Mod_inv.Config.width;
    Cyclesim.cycle sim;

    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;

    inputs.x := x_bits;
    inputs.modulus := mod_bits;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;

    inputs.start := Bits.gnd;

    let max_cycles = 3000 in
    let rec wait cycle_count =
      if cycle_count >= max_cycles then begin
        Stdio.printf "  ERROR: Timeout after %d cycles\n\n" max_cycles;
        false
      end else if Bits.to_bool !(outputs.valid) then begin
        let result_bits = !(outputs.result) in
        let inverse_exists = Bits.to_bool !(outputs.exists) in

        Stdio.printf "  Completed in %d cycles\n" cycle_count;
        Stdio.printf "  Inverse exists: %b\n" inverse_exists;

        let test_passed =
          if Bool.(<>) inverse_exists should_have_inverse then begin
            Stdio.printf "  ERROR: Expected inverse to %sexist ✗\n"
              (if should_have_inverse then "" else "not ");
            false
          end else if inverse_exists then begin
            let result_z =
              let bin_str = Bits.to_bstr result_bits in
              Z.of_string_base 2 bin_str
            in
            Stdio.printf "  result  = %s\n" (Z.to_string result_z);

            let product = Z.(x_z * result_z mod mod_z) in
            let verified = Z.equal product Z.one in

            Stdio.printf "  Verification: (x * result) mod modulus = %s\n"
              (Z.to_string product);

            if verified then
              Stdio.printf "  Mathematical verification: PASS ✓\n"
            else
              Stdio.printf "  Mathematical verification: FAIL ✗\n";

            let matches_expected =
              match expected_result_z_opt with
              | Some expected_z when Z.equal result_z expected_z ->
                  Stdio.printf "  Matches expected value ✓\n";
                  true
              | Some expected_z ->
                  Stdio.printf "  Note: Expected %s but got %s\n"
                    (Z.to_string expected_z) (Z.to_string result_z);
                  verified
              | None ->
                  verified
            in

            verified && matches_expected
          end else begin
            Stdio.printf "  Correctly determined no inverse exists ✓\n";
            true
          end
        in

        Stdio.printf "\n";
        test_passed
      end else begin
        Cyclesim.cycle sim;
        wait (cycle_count + 1)
      end
    in
    wait 0
  in

  let results = [
    test_case "3^(-1) mod 7"
      (Z.of_int 3) (Z.of_int 7) true (Some (Z.of_int 5));

    test_case "7^(-1) mod 13"
      (Z.of_int 7) (Z.of_int 13) true (Some (Z.of_int 2));

    test_case "5^(-1) mod 11"
      (Z.of_int 5) (Z.of_int 11) true (Some (Z.of_int 9));

    test_case "17^(-1) mod 31"
      (Z.of_int 17) (Z.of_int 31) true (Some (Z.of_int 11));

    test_case "42^(-1) mod 2017"
      (Z.of_int 42) (Z.of_int 2017) true (Some (Z.of_int 1969));

    test_case "123^(-1) mod 257"
      (Z.of_int 123) (Z.of_int 257) true None;

    test_case "1^(-1) mod 7"
      (Z.of_int 1) (Z.of_int 7) true (Some (Z.of_int 1));

    test_case "2^(-1) mod 3"
      (Z.of_int 2) (Z.of_int 3) true (Some (Z.of_int 2));

    test_case "Large 64-bit inverse"
      (Z.of_string "123456789012345")
      (Z.of_string "999999999999999989")
      true None;

    test_case "128-bit inverse"
      (Z.of_string "123456789012345678901234567890")
      (Z.of_string "340282366920938463463374607431768211297")
      true None;

    test_case "254-bit: inverse mod 254-bit prime"
      (Z.of_string "123456789012345678901234567890")
      (Z.of_string "28948022309329048855892746252171976963317496166410141009864396001978282409867")
      true None;

    test_case "255-bit: inverse mod 255-bit prime"
      (Z.of_string "987654321098765432109876543210")
      (Z.of_string "57896044618658097711785492504343953926634992332820282019728792003956564819949")
      true None;

    test_case "256-bit: inverse mod secp256k1 prime"
      (Z.of_string "12345678901234567890")
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663")
      true None;

    test_case "256-bit: inverse mod large odd prime"
      (Z.of_string "999999999999999999")
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007913129639747")
      true None;

    test_case "Inverse mod secp256k1 curve order n"
      (Z.of_string "999999999999999999")
      (Z.of_string "0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141")
      true None;

    test_case "Edge: (m-1)^(-1) mod m"
      (Z.of_int 12)
      (Z.of_int 13)
      true (Some (Z.of_int 12));

    test_case "Random 256-bit x mod secp256k1 prime"
      (Z.of_string "110707857788799430918272628231015831947039709193752578283967915890624753014285")
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663")
      true None;
  ] in

  let passed = List.count results ~f:Fn.id in
  let total = List.length results in

  Stdio.printf "=== Test Summary ===\n";
  Stdio.printf "Passed: %d/%d\n" passed total;

  Stdio.Out_channel.close oc;
  Stdio.printf "Saved waveform to %s\n" vcd_file;

  if passed = total then begin
    Stdio.printf "\n";
    Stdio.printf "███████╗██╗   ██╗ ██████╗ ██████╗███████╗███████╗███████╗\n";
    Stdio.printf "██╔════╝██║   ██║██╔════╝██╔════╝██╔════╝██╔════╝██╔════╝\n";
    Stdio.printf "███████╗██║   ██║██║     ██║     █████╗  ███████╗███████╗\n";
    Stdio.printf "╚════██║██║   ██║██║     ██║     ██╔══╝  ╚════██║╚════██║\n";
    Stdio.printf "███████║╚██████╔╝╚██████╗╚██████╗███████╗███████║███████║\n";
    Stdio.printf "╚══════╝ ╚═════╝  ╚═════╝ ╚═════╝╚══════╝╚══════╝╚══════╝\n";
    Stdio.printf "\nAll cryptographic tests passed! ✓✓✓\n";
  end else begin
    Stdio.printf "Some tests failed - NOT READY FOR PRODUCTION ✗\n";
    failwith "checks failed";
  end

let () = test ()
