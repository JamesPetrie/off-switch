open Base
open Hardcaml

module ModMulWithModAdd = struct
  module I = Mod_mul.ModMul.I
  module O = Mod_mul.ModMul.O

  let create scope (i : _ I.t) =
    let mod_add_result_w = Signal.wire Mod_mul.Config.width in
    let mod_add_ready_w  = Signal.wire 1 in

    let mul_out = Mod_mul.ModMul.create (Scope.sub_scope scope "mod_mul")
      { i with
        mod_add_result = mod_add_result_w
      ; mod_add_ready  = mod_add_ready_w
      }
    in

    let add_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_add")
      { Mod_add.ModAdd.I.
        clock    = i.clock
      ; clear    = i.clear
      ; valid    = mul_out.mod_add_valid
      ; a        = mul_out.mod_add_a
      ; b        = mul_out.mod_add_b
      ; modulus  = i.modulus
      ; subtract = mul_out.mod_add_subtract
      }
    in

    Signal.(mod_add_result_w <== add_out.result);
    Signal.(mod_add_ready_w  <== add_out.ready);

    mul_out
end

let test () =
  Stdio.printf "=== ModMul Hardware Test (256-bit with Zarith and num_bits) ===\n\n";

  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(ModMulWithModAdd.I)(ModMulWithModAdd.O) in
  let sim = Sim.create (ModMulWithModAdd.create scope) in

  let vcd_file = "./test_mod_mul.vcd" in
  let oc = Stdio.Out_channel.create vcd_file in
  let sim = Vcd.wrap oc sim in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((Mod_mul.Config.width + 3) / 4) ~char:'0' in
    Bits.of_hex ~width:Mod_mul.Config.width padded
  in

  let z_num_bits z =
    if Z.equal z Z.zero then 0
    else Z.numbits z
  in

  let test_case name x_z y_z mod_z =
    Stdio.printf "Test: %s\n" name;
    Stdio.printf "  x       = %s\n" (Z.to_string x_z);
    Stdio.printf "  y       = %s\n" (Z.to_string y_z);
    Stdio.printf "  modulus = %s\n" (Z.to_string mod_z);

    let expected_z = Z.((x_z * y_z) mod mod_z) in
    Stdio.printf "  expected= %s\n" (Z.to_string expected_z);

    let x_bits = z_to_bits x_z in
    let y_bits = z_to_bits y_z in
    let mod_bits = z_to_bits mod_z in
    let num_bits = z_num_bits y_z in

    Stdio.printf "  y bits  = %d (optimization: %dx speedup)\n"
      num_bits (Mod_mul.Config.width / (max 1 num_bits));

    (* Reset *)
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.x := Bits.zero Mod_mul.Config.width;
    inputs.y := Bits.zero Mod_mul.Config.width;
    inputs.modulus := Bits.zero Mod_mul.Config.width;
    inputs.num_bits := Bits.zero 9;
    Cyclesim.cycle sim;

    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;

    (* Start computation *)
    inputs.x := x_bits;
    inputs.y := y_bits;
    inputs.modulus := mod_bits;
    inputs.num_bits := Bits.of_int ~width:9 num_bits;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;

    inputs.start := Bits.gnd;

    (* Wait for result *)
    let max_cycles = 1500 in
    let rec wait cycle_count =
      if cycle_count >= max_cycles then begin
        Stdio.printf "  ERROR: Timeout after %d cycles\n\n" max_cycles;
        false
      end else if Bits.to_bool !(outputs.valid) then begin
        let result_bits = !(outputs.result) in
        let result_z =
          let bin_str = Bits.to_bstr result_bits in
          Z.of_string_base 2 bin_str
        in

        Stdio.printf "  Completed in %d cycles\n" cycle_count;
        Stdio.printf "  result  = %s\n" (Z.to_string result_z);

        let verified = Z.equal result_z expected_z in

        if verified then
          Stdio.printf "  Verification: PASS ✓\n"
        else
          Stdio.printf "  Verification: FAIL ✗\n";

        Stdio.printf "\n";
        verified
      end else begin
        Cyclesim.cycle sim;
        wait (cycle_count + 1)
      end
    in
    wait 0
  in

  let results = [
    test_case "3 * 5 mod 7"
      (Z.of_int 3) (Z.of_int 5) (Z.of_int 7);

    test_case "12 * 15 mod 17"
      (Z.of_int 12) (Z.of_int 15) (Z.of_int 17);

    test_case "123 * 456 mod 1009"
      (Z.of_int 123) (Z.of_int 456) (Z.of_int 1009);

    test_case "Edge: x * 0 mod m"
      (Z.of_int 12345) (Z.of_int 0) (Z.of_int 7919);

    test_case "Edge: x * 1 mod m"
      (Z.of_int 12345) (Z.of_int 1) (Z.of_int 7919);

    test_case "Large: 123456 * 789012 mod 1000003"
      (Z.of_int 123456) (Z.of_int 789012) (Z.of_int 1000003);

    test_case "64-bit multiplication"
      (Z.of_string "12345678901234")
      (Z.of_string "98765432109876")
      (Z.of_string "999999999999999989");

    test_case "128-bit multiplication"
      (Z.of_string "123456789012345678901234567890")
      (Z.of_string "987654321098765432109876543210")
      (Z.of_string "340282366920938463463374607431768211297");

    test_case "256-bit: multiplication mod secp256k1 prime"
      (Z.of_string "123456789012345678901234567890123456789")
      (Z.of_string "987654321098765432109876543210987654321")
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663");

    test_case "Worst case: y = 2^255 - 1 (255 bits all set, max cycles)"
      (Z.of_string "123456789012345678901234567890123456789")
      (Z.sub (Z.shift_left Z.one 255) Z.one)  (* 2^255 - 1, 255 bits all set, less than modulus *)
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663");
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
    Stdio.printf "\nAll tests passed! ✓✓✓\n";
  end else begin
    Stdio.printf "Some tests failed ✗\n";
    failwith "checks failed";
  end

let () = test ()
