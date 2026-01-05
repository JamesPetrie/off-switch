open Base
open Hardcaml

let () =
  Stdio.printf "=== Scalar Multiplication Debug Test ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Point_mul.I)(Point_mul.O_debug) in
  let sim = Sim.create (Point_mul.create_debug scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let state_name s =
    match Bits.to_int s with
    | 0 -> "Idle"
    | 1 -> "Init"
    | 2 -> "Wait_init"
    | 3 -> "Run_step"
    | 4 -> "Done"
    | _ -> "Unknown"
  in
  
  let op_name o =
    match Bits.to_int o with
    | 0 -> "add"
    | 1 -> "sub"
    | 2 -> "mul"
    | _ -> "???"
  in
  
  let reg_name r =
    match Bits.to_int r with
    | 0 -> "t0" | 1 -> "t1" | 2 -> "t2" | 3 -> "t3" | 4 -> "t4" | 5 -> "t5"
    | 6 -> "x3" | 7 -> "y3" | 8 -> "z3"
    | 9 -> "x1" | 10 -> "y1" | 11 -> "z1"
    | 12 -> "x2" | 13 -> "y2" | 14 -> "z2"
    | 15 -> "param_a" | 16 -> "param_b3"
    | n -> Printf.sprintf "r%d" n
  in
  
  let param_a = Z.zero in
  let param_b3 = Z.of_int 21 in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in
  
  (* Reset *)
  inputs.clear := Bits.vdd;
  inputs.start := Bits.gnd;
  inputs.scalar := Bits.zero 256;
  inputs.param_a := Bits.zero 256;
  inputs.param_b3 := Bits.zero 256;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Cyclesim.cycle sim;
  
  Stdio.printf "After reset:\n";
  Stdio.printf "  state = %s\n" (state_name !(outputs.dbg_state));
  
  (* Start with scalar = 1 *)
  Stdio.printf "\nStarting with scalar = 1:\n";
  inputs.scalar := z_to_bits Z.one;
  inputs.param_a := z_to_bits param_a;
  inputs.param_b3 := z_to_bits param_b3;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  (* Run cycles and observe *)
  for i = 1 to 50 do
    Cyclesim.cycle sim;
    
    let state = state_name !(outputs.dbg_state) in
    let step_val = Bits.to_int !(outputs.dbg_step) in
    let arith_done = Bits.to_int !(outputs.dbg_arith_done) in
    let arith_busy = Bits.to_int !(outputs.dbg_arith_busy) in
    let op_started = Bits.to_int !(outputs.dbg_op_started) in
    
    (* Print on transitions or every few cycles *)
    let prev_step = if i > 1 then step_val else -1 in
    if i <= 15 || arith_done = 1 || (i mod 10 = 0 && prev_step = step_val) then begin
      Stdio.printf "\nCycle %d:\n" i;
      Stdio.printf "  state = %s, step = %d, op_started = %d\n" state step_val op_started;
      Stdio.printf "  instr: %s <- %s %s %s\n" 
        (reg_name !(outputs.dbg_dst))
        (reg_name !(outputs.dbg_src1))
        (op_name !(outputs.dbg_op))
        (reg_name !(outputs.dbg_src2));
      Stdio.printf "  operand_a_lo = 0x%08lx, operand_b_lo = 0x%08lx\n"
        (Bits.to_int32 !(outputs.dbg_operand_a_lo))
        (Bits.to_int32 !(outputs.dbg_operand_b_lo));
      Stdio.printf "  arith_busy = %d, arith_done = %d\n" arith_busy arith_done;
    end;
    
    if Bits.to_bool !(outputs.done_) then begin
      Stdio.printf "\n=== DONE at cycle %d ===\n" i;
      exit 0
    end
  done;
  
  Stdio.printf "\n=== Still running after 50 cycles ===\n";
  Stdio.printf "Final state:\n";
  Stdio.printf "  state = %s\n" (state_name !(outputs.dbg_state));
  Stdio.printf "  step = %d\n" (Bits.to_int !(outputs.dbg_step));
  Stdio.printf "  instr: %s <- %s %s %s\n" 
    (reg_name !(outputs.dbg_dst))
    (reg_name !(outputs.dbg_src1))
    (op_name !(outputs.dbg_op))
    (reg_name !(outputs.dbg_src2));
  Stdio.printf "  operand_a_lo = 0x%08lx, operand_b_lo = 0x%08lx\n"
    (Bits.to_int32 !(outputs.dbg_operand_a_lo))
    (Bits.to_int32 !(outputs.dbg_operand_b_lo));
  Stdio.printf "  arith_busy = %d\n" (Bits.to_int !(outputs.dbg_arith_busy));
  Stdio.printf "  arith_done = %d\n" (Bits.to_int !(outputs.dbg_arith_done))