open Base
open Hardcaml
open Ecdsa

let () =
  Stdio.printf "=== Point Doubling Debug with tmp5 and lambda_sq ===\n\n";
  
  let g_x = Test_utils.g_x in
  let g_y = Test_utils.g_y in
  let prime_p = Test_utils.prime_p in
  
  let x_sq = Test_utils.mod_mul g_x g_x prime_p in
  let two_x_sq = Test_utils.mod_add x_sq x_sq prime_p in
  let three_x_sq = Test_utils.mod_add two_x_sq x_sq prime_p in
  let two_y = Test_utils.mod_add g_y g_y prime_p in
  let two_y_inv = Test_utils.mod_inv two_y prime_p in
  let lambda = Test_utils.mod_mul three_x_sq two_y_inv prime_p in
  let lambda_sq = Test_utils.mod_mul lambda lambda prime_p in
  let two_x = Test_utils.mod_add g_x g_x prime_p in
  let x_r = Test_utils.mod_sub lambda_sq two_x prime_p in
  
  Stdio.printf "Expected values:\n";
  Stdio.printf "  lambda^2 = %s\n" (Z.to_string lambda_sq);
  Stdio.printf "  2*x      = %s\n" (Z.to_string two_x);
  Stdio.printf "  x_r      = %s\n\n" (Z.to_string x_r);
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(EcdsaVerify.I)(EcdsaVerify.O) in
  let sim = Sim.create (EcdsaVerify.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  (* Reset *)
  inputs.clear := Bits.vdd;
  inputs.start := Bits.gnd;
  inputs.test_mode := Bits.of_int ~width:2 Config.TestMode.point_double;
  inputs.load_enable := Bits.gnd;
  inputs.load_addr := Bits.zero Config.reg_addr_width;
  inputs.load_data := Bits.zero Config.width;
  inputs.e := Bits.zero Config.width;
  inputs.r := Bits.zero Config.width;
  inputs.s := Bits.zero Config.width;
  inputs.q_x := Bits.zero Config.width;
  inputs.q_y := Bits.zero Config.width;
  inputs.qplusg_x := Bits.zero Config.width;
  inputs.qplusg_y := Bits.zero Config.width;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Cyclesim.cycle sim;
  
  let load_reg addr value =
    inputs.load_enable := Bits.vdd;
    inputs.load_addr := Bits.of_int ~width:Config.reg_addr_width addr;
    inputs.load_data := Test_utils.z_to_bits value;
    Cyclesim.cycle sim;
    inputs.load_enable := Bits.gnd
  in
  
  load_reg Config.Reg.zero Z.zero;
  load_reg Config.Reg.acc_x g_x;
  load_reg Config.Reg.acc_y g_y;
  
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  let max_cycles = 5000 in
  let cycle_count = ref 0 in
  let last_state = ref (-1) in
  
  while !cycle_count < max_cycles && not (Bits.to_bool !(outputs.done_)) do
    let state = Bits.to_int !(outputs.dbg_state) in
    
    if state <> !last_state then begin
      (* After Pdbl_lsq (entering state 36 = Pdbl_2x) *)
      if state = 36 then begin
        let hw_lambda_sq = Test_utils.bits_to_z !(outputs.dbg_lambda_sq) in
        Stdio.printf "[After Pdbl_lsq] lambda_sq:\n";
        Stdio.printf "  hw:  %s\n" (Z.to_string hw_lambda_sq);
        Stdio.printf "  exp: %s\n" (Z.to_string lambda_sq);
        Stdio.printf "  match: %b\n\n" (Z.equal hw_lambda_sq lambda_sq);
      end;
      
      (* After Pdbl_2x (entering state 38 = Pdbl_xr) *)
      if state = 38 then begin
        let hw_tmp5 = Test_utils.bits_to_z !(outputs.dbg_tmp5) in
        let hw_lambda_sq = Test_utils.bits_to_z !(outputs.dbg_lambda_sq) in
        Stdio.printf "[After Pdbl_2x] Inputs to Pdbl_xr (lambda_sq - tmp5):\n";
        Stdio.printf "  tmp5 (2*x):  hw=%s\n" (Z.to_string hw_tmp5);
        Stdio.printf "               exp=%s\n" (Z.to_string two_x);
        Stdio.printf "               match: %b\n" (Z.equal hw_tmp5 two_x);
        Stdio.printf "  lambda_sq:   hw=%s\n" (Z.to_string hw_lambda_sq);
        Stdio.printf "               exp=%s\n" (Z.to_string lambda_sq);
        Stdio.printf "               match: %b\n\n" (Z.equal hw_lambda_sq lambda_sq);
      end;
      
      (* After Pdbl_xr (entering state 40 = Pdbl_diff) *)
      if state = 40 then begin
        let hw_tmp1 = Test_utils.bits_to_z !(outputs.dbg_tmp1) in
        Stdio.printf "[After Pdbl_xr] tmp1 (x_r):\n";
        Stdio.printf "  hw:  %s\n" (Z.to_string hw_tmp1);
        Stdio.printf "  exp: %s\n" (Z.to_string x_r);
        Stdio.printf "  match: %b\n\n" (Z.equal hw_tmp1 x_r);
      end;
      
      last_state := state
    end;
    
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  
  Stdio.printf "=== Done in %d cycles ===\n" !cycle_count