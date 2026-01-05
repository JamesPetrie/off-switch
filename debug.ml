open Base
open Hardcaml

let () =
  Stdio.printf "=== Arith Timing Debug Test ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Arith.I)(Arith.O) in
  let sim = Sim.create (Arith.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in
  
  let bits_to_z bits = Z.of_string_base 2 (Bits.to_bstr bits) in
  
  let g_x = Z.of_string_base 16 "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" in
  let g_y = Z.of_string_base 16 "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8" in
  
  (* Reset *)
  inputs.clear := Bits.vdd;
  inputs.start := Bits.gnd;
  inputs.op := Bits.zero 2;
  inputs.prime_sel := Bits.gnd;
  inputs.addr_a := Bits.zero 5;
  inputs.addr_b := Bits.zero 5;
  inputs.addr_out := Bits.zero 5;
  inputs.reg_read_data_a := Bits.zero 256;
  inputs.reg_read_data_b := Bits.zero 256;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Cyclesim.cycle sim;
  
  (* First multiplication: G_x * G_x *)
  Stdio.printf "=== Operation 1: G_x * G_x ===\n";
  inputs.start := Bits.vdd;
  inputs.op := Bits.of_int ~width:2 2;
  inputs.reg_read_data_a := z_to_bits g_x;
  inputs.reg_read_data_b := z_to_bits g_x;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  let rec wait_op1 cycle =
    let done_ = Bits.to_bool !(outputs.done_) in
    if done_ then begin
      let result = bits_to_z !(outputs.reg_write_data) in
      Stdio.printf "  [cycle %d] Op1 done! result=%s...\n" cycle (String.prefix (Z.to_string result) 30);
      cycle
    end else if cycle > 500 then begin
      Stdio.printf "  TIMEOUT on Op1\n";
      -1
    end else begin
      Cyclesim.cycle sim;
      wait_op1 (cycle + 1)
    end
  in
  let cycle_after_op1 = wait_op1 0 in
  
  if cycle_after_op1 < 0 then ignore (exit 1);
  
  (* Now simulate point_add's behavior: wait one cycle, then start *)
  Stdio.printf "\n=== Simulating point_add timing ===\n";
  Stdio.printf "  [after done] Waiting one cycle (like point_add does)...\n";
  
  (* This cycle: done_ is high, we see it, but don't start yet *)
  let busy0 = Bits.to_bool !(outputs.busy) in
  let done0 = Bits.to_bool !(outputs.done_) in
  Stdio.printf "  [cycle 0 after done] busy=%b done=%b start=0\n" busy0 done0;
  Cyclesim.cycle sim;
  
  (* Next cycle: now we start *)
  let busy1 = Bits.to_bool !(outputs.busy) in
  let done1 = Bits.to_bool !(outputs.done_) in
  Stdio.printf "  [cycle 1 after done] busy=%b done=%b - NOW sending start=1\n" busy1 done1;
  
  inputs.start := Bits.vdd;
  inputs.op := Bits.of_int ~width:2 2;
  inputs.reg_read_data_a := z_to_bits g_y;
  inputs.reg_read_data_b := z_to_bits g_y;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  (* Watch what happens *)
  Stdio.printf "\n=== Operation 2: G_y * G_y ===\n";
  for i = 0 to 20 do
    let busy = Bits.to_bool !(outputs.busy) in
    let done_ = Bits.to_bool !(outputs.done_) in
    Stdio.printf "  [op2 cycle %d] busy=%b done=%b\n" i busy done_;
    Cyclesim.cycle sim
  done;
  
  let rec wait_op2 cycle =
    let done_ = Bits.to_bool !(outputs.done_) in
    if done_ then begin
      let result = bits_to_z !(outputs.reg_write_data) in
      Stdio.printf "  [cycle %d] Op2 done! result=%s...\n" cycle (String.prefix (Z.to_string result) 30);
      cycle
    end else if cycle > 500 then begin
      Stdio.printf "  TIMEOUT on Op2\n";
      -1
    end else begin
      Cyclesim.cycle sim;
      wait_op2 (cycle + 1)
    end
  in
  let _ = wait_op2 21 in
  
  Stdio.printf "\nTest complete\n"