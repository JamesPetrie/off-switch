open Base
open Hardcaml

let () =
  Stdio.printf "=== Detailed State Machine Trace ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Ecdsa.EcdsaVerify.I)(Ecdsa.EcdsaVerify.O) in
  let sim = Sim.create (Ecdsa.EcdsaVerify.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  let width = Ecdsa.Config.width in
  let prime_p = Ecdsa.Config.prime_p_z in
  let g_x = Ecdsa.Config.g_x_z in
  let g_y = Ecdsa.Config.g_y_z in
  
  let mod_sub a b p = Z.(erem (a - b) p) in
  let mod_mul a b p = Z.((a * b) mod p) in
  let mod_inv a p = Z.(invert a p) in
  
  let sw_point_double (px, py) =
    if Z.equal py Z.zero then (Z.zero, Z.zero)
    else
      let lambda = mod_mul 
        (mod_mul (Z.of_int 3) (mod_mul px px prime_p) prime_p)
        (mod_inv (mod_mul (Z.of_int 2) py prime_p) prime_p)
        prime_p
      in
      let xr = mod_sub (mod_mul lambda lambda prime_p) (mod_mul (Z.of_int 2) px prime_p) prime_p in
      let yr = mod_sub (mod_mul lambda (mod_sub px xr prime_p) prime_p) py prime_p in
      (xr, yr)
  in
  
  let sw_point_add (px, py) (qx, qy) =
    if Z.equal px qx then
      if Z.equal py qy then sw_point_double (px, py)
      else (Z.zero, Z.zero)
    else
      let lambda = mod_mul (mod_sub qy py prime_p) (mod_inv (mod_sub qx px prime_p) prime_p) prime_p in
      let xr = mod_sub (mod_sub (mod_mul lambda lambda prime_p) px prime_p) qx prime_p in
      let yr = mod_sub (mod_mul lambda (mod_sub px xr prime_p) prime_p) py prime_p in
      (xr, yr)
  in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:(width / 4) ~char:'0' in
    Bits.of_hex ~width padded
  in
  
  let bits_to_z bits =
    Z.of_string_base 2 (Bits.to_bstr bits)
  in
  
  let (q_x, q_y) = sw_point_double (g_x, g_y) in
  let (qpg_x, qpg_y) = sw_point_add (q_x, q_y) (g_x, g_y) in
  
  let g5 = 
    let g2 = sw_point_double (g_x, g_y) in
    let g4 = sw_point_double g2 in
    sw_point_add (g_x, g_y) g4
  in
  
  Stdio.printf "Q = 2G, Q+G = 3G\n";
  Stdio.printf "Test: e=3, r=1, s=1 -> u1=3, u2=1 -> expect 5G\n\n";
  
  let test_e = Z.of_int 3 in
  let test_r = Z.of_int 1 in  
  let test_s = Z.of_int 1 in
  
  (* Reset *)
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
  let bit_idx () = Bits.to_int !(outputs.dbg_bit_idx) in
  let is_done () = Bits.to_bool !(outputs.done_) in
  let acc_x () = bits_to_z !(outputs.dbg_acc_x) in
  let acc_y () = bits_to_z !(outputs.dbg_acc_y) in
  let acc_is_inf () = Bits.to_bool !(outputs.dbg_acc_is_infinity) in
  let u1_bit () = Bits.to_bool !(outputs.dbg_u1_bit) in
  let u2_bit () = Bits.to_bool !(outputs.dbg_u2_bit) in
  let double_count () = Bits.to_int !(outputs.dbg_double_count) in
  let add_count () = Bits.to_int !(outputs.dbg_add_count) in
  let copy_count () = Bits.to_int !(outputs.dbg_copy_count) in
  let _u1_val () = bits_to_z !(outputs.dbg_acc_x) in (* We need u1/u2 debug outputs *)
  
  let state_name st = match st with
    | 0 -> "Idle" | 1 -> "Load_inputs" | 2 -> "Init_constants"
    | 3 -> "Validate_r_nonzero" | 4 -> "Validate_r_less_than_n"
    | 5 -> "Validate_s_nonzero" | 6 -> "Validate_s_less_than_n"
    | 7 -> "Validation_failed"
    | 8 -> "Compute_w" | 9 -> "Wait_w" | 10 -> "Check_w_exists"
    | 11 -> "Compute_u1" | 12 -> "Wait_u1"
    | 13 -> "Compute_u2" | 14 -> "Wait_u2"
    | 15 -> "Loop_init" | 16 -> "Loop_double_start"
    | 48 -> "Loop_check_bits"
    | 90 -> "Loop_next" | 91 -> "Final_check" | 92 -> "Done"
    | n -> Printf.sprintf "State_%d" n
  in
  
  (* Set inputs *)
  inputs.e := z_to_bits test_e;
  inputs.r := z_to_bits test_r;
  inputs.s := z_to_bits test_s;
  inputs.q_x := z_to_bits q_x;
  inputs.q_y := z_to_bits q_y;
  inputs.qplusg_x := z_to_bits qpg_x;
  inputs.qplusg_y := z_to_bits qpg_y;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  let max_cycles = 5000 in
  let cycle_count = ref 0 in
  let last_state = ref (-1) in
  let last_bit_idx = ref 256 in
  
  Stdio.printf "=== State Transitions ===\n\n";
  
  while !cycle_count < max_cycles && not (is_done ()) do
    let st = ctrl_state () in
    let bi = bit_idx () in
    
    (* Print on state changes *)
    if st <> !last_state then begin
      Stdio.printf "[cycle %d] %s (bit_idx=%d, acc_inf=%b, d=%d a=%d c=%d)\n"
        !cycle_count (state_name st) bi (acc_is_inf ()) 
        (double_count ()) (add_count ()) (copy_count ());
      
      (* Extra info for Loop_check_bits *)
      if st = 48 then
        Stdio.printf "          u1[%d]=%d u2[%d]=%d\n" bi (if u1_bit () then 1 else 0) bi (if u2_bit () then 1 else 0);
      
      (* Print acc value when entering Loop_next or Final_check *)
      if (st = 90 || st = 91) && not (acc_is_inf ()) then
        Stdio.printf "          acc = (%s...)\n" (String.prefix (Z.to_string (acc_x ())) 30);
      
      last_state := st
    end;
    
    (* Also print when bit_idx changes during Loop states *)
    if bi <> !last_bit_idx && (st >= 15 && st <= 90) then begin
      if !last_bit_idx <> 256 then
        Stdio.printf "  -> bit_idx changed: %d -> %d\n" !last_bit_idx bi;
      last_bit_idx := bi
    end;
    
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  
  Stdio.printf "\n=== Final Results ===\n\n";
  Stdio.printf "Cycles: %d\n" !cycle_count;
  Stdio.printf "Final state: %s\n" (state_name (ctrl_state ()));
  Stdio.printf "acc_is_infinity: %b\n" (acc_is_inf ());
  Stdio.printf "acc_x = %s\n" (Z.to_string (acc_x ()));
  Stdio.printf "acc_y = %s\n\n" (Z.to_string (acc_y ()));
  
  Stdio.printf "Operation counts:\n";
  Stdio.printf "  Doubles: %d\n" (double_count ());
  Stdio.printf "  Additions: %d\n" (add_count ());
  Stdio.printf "  Copies: %d\n\n" (copy_count ());
  
  Stdio.printf "Expected 5G:\n";
  Stdio.printf "  x = %s\n" (Z.to_string (fst g5));
  Stdio.printf "  y = %s\n\n" (Z.to_string (snd g5));
  
  let matches = Z.equal (acc_x ()) (fst g5) && Z.equal (acc_y ()) (snd g5) in
  Stdio.printf "Hardware matches expected: %b\n" matches