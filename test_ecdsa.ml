open Base
open Hardcaml

let () =
  Stdio.printf "=== ECDSA Connectivity Test ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Ecdsa.EcdsaVerify.I)(Ecdsa.EcdsaVerify.O) in
  let sim = Sim.create (Ecdsa.EcdsaVerify.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let width = Ecdsa.Config.width in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:(width / 4) ~char:'0' in
    Bits.of_hex ~width padded
  in
  
  let test_e = Z.of_int 12345 in
  let test_r = Z.of_int 67890 in
  let test_s = Z.of_int 11111 in
  let test_q_x = Z.of_int 22222 in
  let test_q_y = Z.of_int 33333 in
  let test_qpg_x = Z.of_int 44444 in
  let test_qpg_y = Z.of_int 55555 in
  
  Stdio.printf "1. Resetting system...\n";
  inputs.clear := Bits.vdd;
  inputs.start := Bits.gnd;
  inputs.e := Bits.zero width;
  inputs.r := Bits.zero width;
  inputs.s := Bits.zero width;
  inputs.q_x := Bits.zero width;
  inputs.q_y := Bits.zero width;
  inputs.qplusg_x := Bits.zero width;
  inputs.qplusg_y := Bits.zero width;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Cyclesim.cycle sim;
  
  let ctrl_state () = Bits.to_int !(outputs.dbg_ctrl_state) in
  let arith_state () = Bits.to_int !(outputs.dbg_arith_state) in
  let is_busy () = Bits.to_bool !(outputs.busy) in
  let load_enable () = Bits.to_bool !(outputs.dbg_load_enable) in
  let load_addr () = Bits.to_int !(outputs.dbg_load_addr) in
  let arith_start () = Bits.to_bool !(outputs.dbg_arith_start) in
  let arith_done () = Bits.to_bool !(outputs.dbg_arith_done) in
  let arith_op () = Bits.to_int !(outputs.dbg_arith_op) in
  let bit_idx () = Bits.to_int !(outputs.dbg_bit_idx) in
  
  let op_name op = match op with
    | 0 -> "ADD"
    | 1 -> "SUB"
    | 2 -> "MUL"
    | 3 -> "INV"
    | _ -> "???"
  in
  
  let ctrl_state_name st = match st with
    | 0 -> "Idle"
    | 1 -> "Load_inputs"
    | 2 -> "Init_constants"
    | 3 -> "Compute_w"
    | 4 -> "Wait_w"
    | 5 -> "Compute_u1"
    | 6 -> "Wait_u1"
    | 7 -> "Compute_u2"
    | 8 -> "Wait_u2"
    | 9 -> "Loop_init"
    | _ -> Printf.sprintf "State_%d" st
  in
  
  Stdio.printf "   Initial state: %s, arith=%d, busy=%b\n" 
    (ctrl_state_name (ctrl_state ())) (arith_state ()) (is_busy ());
  
  Stdio.printf "\n2. Starting ECDSA verification...\n";
  inputs.e := z_to_bits test_e;
  inputs.r := z_to_bits test_r;
  inputs.s := z_to_bits test_s;
  inputs.q_x := z_to_bits test_q_x;
  inputs.q_y := z_to_bits test_q_y;
  inputs.qplusg_x := z_to_bits test_qpg_x;
  inputs.qplusg_y := z_to_bits test_qpg_y;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  Stdio.printf "   After start: %s, busy=%b\n" (ctrl_state_name (ctrl_state ())) (is_busy ());
  
  (* Track all events in order *)
  Stdio.printf "\n3. Running and tracking all events...\n";
  
  let input_count = ref 0 in
  let const_count = ref 0 in
  let in_const_phase = ref false in
  let arith_ops = ref [] in
  let waiting_for_done = ref false in
  let current_op = ref (-1) in
  let current_op_state = ref "" in
  
  let max_cycles = 5000 in
  let cycle_count = ref 0 in
  
  while !cycle_count < max_cycles && is_busy () do
    let st = ctrl_state () in
    let st_name = ctrl_state_name st in
    
    (* Track constant phase *)
    if st = 2 then in_const_phase := true;
    
    (* Track loads *)
    if load_enable () then begin
      let addr = load_addr () in
      if !in_const_phase then Int.incr const_count
      else Int.incr input_count;
      Stdio.printf "   [cycle %d] [%s] Load addr=%d\n" !cycle_count st_name addr
    end;
    
    (* Track arith start *)
    if arith_start () && not !waiting_for_done then begin
      current_op := arith_op ();
      current_op_state := st_name;
      waiting_for_done := true;
      Stdio.printf "   [cycle %d] [%s] Arith START: %s\n" !cycle_count st_name (op_name !current_op)
    end;
    
    (* Track arith done *)
    if arith_done () && !waiting_for_done then begin
      arith_ops := (!current_op_state, !current_op) :: !arith_ops;
      waiting_for_done := false;
      Stdio.printf "   [cycle %d] [%s] Arith DONE\n" !cycle_count st_name
    end;
    
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  
  Stdio.printf "\n   Completed in %d cycles\n" !cycle_count;
  Stdio.printf "   Final state: %s, bit_idx=%d\n" (ctrl_state_name (ctrl_state ())) (bit_idx ());
  
  (* Analyze results *)
  Stdio.printf "\n=== Summary ===\n";
  Stdio.printf "- Input registers loaded: %d (expect 7)\n" !input_count;
  Stdio.printf "- Constant registers loaded: %d (expect 6)\n" !const_count;
  
  let arith_ops_list = List.rev !arith_ops in
  Stdio.printf "- Arithmetic operations observed: %d\n" (List.length arith_ops_list);
  
  List.iteri arith_ops_list ~f:(fun i (state, op) ->
    Stdio.printf "    %d. [%s] %s\n" (i + 1) state (op_name op)
  );
  
  (* Check first 3 operations *)
  let check_op idx expected_op =
    match List.nth arith_ops_list idx with
    | Some (_, op) -> op = expected_op
    | None -> false
  in
  
  let op1_ok = check_op 0 3 in  (* INV *)
  let op2_ok = check_op 1 2 in  (* MUL *)
  let op3_ok = check_op 2 2 in  (* MUL *)
  
  Stdio.printf "\n=== Verification ===\n";
  Stdio.printf "- Input loading: %s\n" (if !input_count = 7 then "PASS ✓" else "FAIL ✗");
  Stdio.printf "- Constant loading: %s\n" (if !const_count = 6 then "PASS ✓" else "FAIL ✗");
  Stdio.printf "- Op 1 is INV (w = s^-1): %s\n" (if op1_ok then "PASS ✓" else "FAIL ✗");
  Stdio.printf "- Op 2 is MUL (u1 = e*w): %s\n" (if op2_ok then "PASS ✓" else "FAIL ✗");
  Stdio.printf "- Op 3 is MUL (u2 = r*w): %s\n" (if op3_ok then "PASS ✓" else "FAIL ✗");
  
  let all_passed = !input_count = 7 && !const_count = 6 && op1_ok && op2_ok && op3_ok in
  Stdio.printf "\n%s\n" 
    (if all_passed then "All connectivity tests PASSED ✓" else "Some tests FAILED ✗")