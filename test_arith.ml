open Base
open Hardcaml

let () =
  let width = 256 in
  
  let a_z = Z.of_int 100 in
  let b_z = Z.of_int 200 in
  let modulus_z = Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663" in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((width + 3) / 4) ~char:'0' in
    Bits.of_hex ~width padded
  in
  let bits_to_z bits = Z.of_string_base 2 (Bits.to_bstr bits) in
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Arith.I)(Arith.O) in
  let sim = Sim.create (Arith.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let reset () =
    inputs.clear := Bits.vdd;
    inputs.start_add := Bits.gnd;
    inputs.start_sub := Bits.gnd;
    inputs.start_mul := Bits.gnd;
    inputs.start_inv := Bits.gnd;
    inputs.a := Bits.zero width;
    inputs.b := Bits.zero width;
    inputs.modulus := Bits.zero width;
    inputs.num_bits := Bits.zero 9;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in
  
  let wait_for_valid valid_ref =
    let max_cycles = 500 in
    let rec wait n =
      if n >= max_cycles then false
      else if Bits.to_bool !valid_ref then true
      else begin Cyclesim.cycle sim; wait (n + 1) end
    in
    wait 0
  in
  
  (* Test addition *)
  Stdio.printf "=== Testing Modular Addition ===\n";
  reset ();
  inputs.a := z_to_bits a_z;
  inputs.b := z_to_bits b_z;
  inputs.modulus := z_to_bits modulus_z;
  inputs.start_add := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start_add := Bits.gnd;
  if wait_for_valid outputs.add_valid then begin
    let result = bits_to_z !(outputs.add_result) in
    let expected = Z.((a_z + b_z) mod modulus_z) in
    Stdio.printf "  %s + %s mod n = %s\n" (Z.to_string a_z) (Z.to_string b_z) (Z.to_string result);
    Stdio.printf "  Expected: %s\n" (Z.to_string expected);
    Stdio.printf "  %s\n\n" (if Z.equal result expected then "PASS ✓" else "FAIL ✗")
  end else Stdio.printf "  TIMEOUT\n\n";
  
  (* Test subtraction *)
  Stdio.printf "=== Testing Modular Subtraction ===\n";
  reset ();
  inputs.a := z_to_bits a_z;
  inputs.b := z_to_bits b_z;
  inputs.modulus := z_to_bits modulus_z;
  inputs.start_sub := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start_sub := Bits.gnd;
  if wait_for_valid outputs.sub_valid then begin
    let result = bits_to_z !(outputs.sub_result) in
    let expected = Z.(erem (a_z - b_z) modulus_z) in
    Stdio.printf "  %s - %s mod n = %s\n" (Z.to_string a_z) (Z.to_string b_z) (Z.to_string result);
    Stdio.printf "  Expected: %s\n" (Z.to_string expected);
    Stdio.printf "  %s\n\n" (if Z.equal result expected then "PASS ✓" else "FAIL ✗")
  end else Stdio.printf "  TIMEOUT\n\n";
  
  (* Test multiplication *)
  Stdio.printf "=== Testing Modular Multiplication ===\n";
  reset ();
  let num_bits = if Z.equal b_z Z.zero then 0 else Z.numbits b_z in
  inputs.a := z_to_bits a_z;
  inputs.b := z_to_bits b_z;
  inputs.modulus := z_to_bits modulus_z;
  inputs.num_bits := Bits.of_int ~width:9 num_bits;
  inputs.start_mul := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start_mul := Bits.gnd;
  if wait_for_valid outputs.mul_valid then begin
    let result = bits_to_z !(outputs.mul_result) in
    let expected = Z.((a_z * b_z) mod modulus_z) in
    Stdio.printf "  %s * %s mod n = %s\n" (Z.to_string a_z) (Z.to_string b_z) (Z.to_string result);
    Stdio.printf "  Expected: %s\n" (Z.to_string expected);
    Stdio.printf "  %s\n\n" (if Z.equal result expected then "PASS ✓" else "FAIL ✗")
  end else Stdio.printf "  TIMEOUT\n\n";
  
  (* Test inversion *)
  Stdio.printf "=== Testing Modular Inversion ===\n";
  reset ();
  inputs.a := z_to_bits a_z;
  inputs.modulus := z_to_bits modulus_z;
  inputs.start_inv := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start_inv := Bits.gnd;
  if wait_for_valid outputs.inv_valid then begin
    let result = bits_to_z !(outputs.inv_result) in
    let exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  %s^(-1) mod n = %s\n" (Z.to_string a_z) (Z.to_string result);
    Stdio.printf "  Inverse exists: %b\n" exists;
    if exists then begin
      let product = Z.((a_z * result) mod modulus_z) in
      Stdio.printf "  Verification: %s * %s mod n = %s\n" 
        (Z.to_string a_z) (Z.to_string result) (Z.to_string product);
      Stdio.printf "  %s\n" (if Z.equal product Z.one then "PASS ✓" else "FAIL ✗")
    end
  end else Stdio.printf "  TIMEOUT\n"