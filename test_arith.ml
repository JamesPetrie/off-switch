open Base
open Hardcaml

let () =
  let width = 256 in
  
  let a_z = Z.of_int 100 in
  let b_z = Z.of_int 203 in
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
  
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Cyclesim.cycle sim;
  
  inputs.a := z_to_bits a_z;
  inputs.b := z_to_bits b_z;
  inputs.modulus := z_to_bits modulus_z;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  while not (Bits.to_bool !(outputs.valid)) do
    Cyclesim.cycle sim
  done;
  
  let result_z = bits_to_z !(outputs.result) in
  Stdio.printf "Result: %s\n" (Z.to_string result_z)
