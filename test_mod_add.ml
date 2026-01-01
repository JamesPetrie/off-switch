open Base
open Hardcaml

let test () =
  let test_modulus_z = 
    Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663"
  in
  
  Stdio.printf "=== ModAdd Hardware Test (256-bit modular add/sub) ===\n\n";
  Stdio.printf "Test Modulus (n) = %s\n\n" (Z.to_string test_modulus_z);

  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Mod_add.ModAdd.I)(Mod_add.ModAdd.O) in
  let sim = Sim.create (Mod_add.ModAdd.create scope) in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((Mod_add.Config.width + 3) / 4) ~char:'0' in
    Bits.of_hex ~width:Mod_add.Config.width padded
  in

  let bits_to_z bits =
    let bin_str = Bits.to_bstr bits in
    Z.of_string_base 2 bin_str
  in

  let test_case name a_z b_z modulus_z is_sub =
    let op_str = if is_sub then "-" else "+" in
    Stdio.printf "Test: %s\n" name;
    Stdio.printf "  a %s b mod n\n" op_str;
    Stdio.printf "  a = %s\n" (Z.to_string a_z);
    Stdio.printf "  b = %s\n" (Z.to_string b_z);
    Stdio.printf "  n = %s\n" (Z.to_string modulus_z);

    let expected_z = 
      let raw = if is_sub then Z.(a_z - b_z) else Z.(a_z + b_z) in
      Z.(erem raw modulus_z)
    in
    Stdio.printf "  expected = %s\n" (Z.to_string expected_z);

    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.a := Bits.zero Mod_add.Config.width;
    inputs.b := Bits.zero Mod_add.Config.width;
    inputs.modulus := Bits.zero Mod_add.Config.width;
    inputs.subtract := Bits.gnd;
    Cyclesim.cycle sim;

    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;

    inputs.a := z_to_bits a_z;
    inputs.b := z_to_bits b_z;
    inputs.modulus := z_to_bits modulus_z;
    inputs.subtract := if is_sub then Bits.vdd else Bits.gnd;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;

    inputs.start := Bits.gnd;

    let max_cycles = 20 in
    let rec wait cycle_count =
      if cycle_count >= max_cycles then begin
        Stdio.printf "  ERROR: Timeout after %d cycles\n\n" max_cycles;
        false
      end else if Bits.to_bool !(outputs.valid) then begin
        let result_z = bits_to_z !(outputs.result) in
        
        Stdio.printf "  Completed in %d cycles\n" cycle_count;
        Stdio.printf "  result = %s\n" (Z.to_string result_z);
        
        let verified = Z.equal result_z expected_z in
        
        if verified then
          Stdio.printf "  Verification: PASS ✓\n"
        else
          Stdio.printf "  Verification: FAIL ✗ (diff: %s)\n" 
            (Z.to_string Z.(result_z - expected_z));
        
        Stdio.printf "\n";
        verified
      end else begin
        Cyclesim.cycle sim;
        wait (cycle_count + 1)
      end
    in
    wait 0
  in

  let test_with_default_modulus name a_z b_z is_sub =
    test_case name a_z b_z test_modulus_z is_sub
  in

  let results = [
    test_with_default_modulus "Simple addition"
      (Z.of_int 100) (Z.of_int 200) false;
    
    test_with_default_modulus "Addition with modular wrap"
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671600")
      (Z.of_int 100) false;
    
    test_with_default_modulus "Simple subtraction"
      (Z.of_int 500) (Z.of_int 300) true;
    
    test_with_default_modulus "Subtraction requiring modular correction"
      (Z.of_int 100) (Z.of_int 200) true;
    
    test_with_default_modulus "Subtraction from modulus-1"
      Z.(test_modulus_z - one) (Z.of_int 10) true;
    
    test_with_default_modulus "Add zero"
      (Z.of_int 12345) Z.zero false;
    
    test_with_default_modulus "Subtract zero"
      (Z.of_int 12345) Z.zero true;
    
    test_case "Different modulus: small prime 997"
      (Z.of_int 500) (Z.of_int 600) (Z.of_int 997) false;
    
    test_case "BN254 subtraction wrap"
      (Z.of_int 10)
      (Z.of_int 20)
      (Z.of_string "21888242871839275222246405745257275088548364400416034343698204186575808495617")
      true;
  ] in

  let passed = List.count results ~f:Fn.id in
  let total = List.length results in

  Stdio.printf "=== Test Summary ===\n";
  Stdio.printf "Passed: %d/%d\n" passed total;

  if passed = total then
    Stdio.printf "\n✓ All tests passed!\n"
  else
    Stdio.printf "\n✗ Some tests failed\n"

let () = test ()