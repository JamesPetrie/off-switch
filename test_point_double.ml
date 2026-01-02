open Base
open Hardcaml
open Ecdsa

let () =
  Stdio.printf "=== Point Doubling Unit Test ===\n\n";
  
  (* Compute expected results using software reference *)
  let g = (Test_utils.g_x, Test_utils.g_y) in
  let g2 = Test_utils.point_double g in
  let g4 = Option.bind g2 ~f:Test_utils.point_double in
  let g8 = Option.bind g4 ~f:Test_utils.point_double in
  
  Stdio.printf "Expected values (software reference):\n";
  Test_utils.print_point "G  " (Some g);
  Test_utils.print_point "2G " g2;
  Test_utils.print_point "4G " g4;
  Test_utils.print_point "8G " g8;
  Stdio.printf "\n";
  
  (* Create simulator *)
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(EcdsaVerify.I)(EcdsaVerify.O) in
  let sim = Sim.create (EcdsaVerify.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  (* Helper: set all inputs to default values *)
  let reset_inputs () =
    inputs.clear := Bits.gnd;
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
    inputs.qplusg_y := Bits.zero Config.width
  in
  
  (* Helper: load a value into a register *)
  let load_reg addr value =
    inputs.load_enable := Bits.vdd;
    inputs.load_addr := Bits.of_int ~width:Config.reg_addr_width addr;
    inputs.load_data := Test_utils.z_to_bits value;
    Cyclesim.cycle sim;
    inputs.load_enable := Bits.gnd
  in
  
  (* Helper: run point doubling test and return result *)
  let run_point_double name (px, py) expected =
    Stdio.printf "Test: %s\n" name;
    
    (* Reset *)
    reset_inputs ();
    inputs.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;
    
    (* Load constants required by the controller *)
    load_reg Config.Reg.zero Z.zero;
    load_reg Config.Reg.one Z.one;
    load_reg Config.Reg.two (Z.of_int 2);
    load_reg Config.Reg.three (Z.of_int 3);
    
    (* Load input point into acc_x, acc_y *)
    load_reg Config.Reg.acc_x px;
    load_reg Config.Reg.acc_y py;
    
    (* Start point doubling in test mode *)
    inputs.test_mode := Bits.of_int ~width:2 Config.TestMode.point_double;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    
    (* Wait for completion *)
    let max_cycles = 50000 in
    let cycle_count = ref 0 in
    while !cycle_count < max_cycles && not (Bits.to_bool !(outputs.done_)) do
      Cyclesim.cycle sim;
      Int.incr cycle_count
    done;
    
    if !cycle_count >= max_cycles then begin
      Stdio.printf "  TIMEOUT after %d cycles\n\n" max_cycles;
      false
    end else begin
      let result_x = Test_utils.bits_to_z !(outputs.dbg_acc_x) in
      let result_y = Test_utils.bits_to_z !(outputs.dbg_acc_y) in
      let is_infinity = Bits.to_bool !(outputs.dbg_acc_is_infinity) in
      
      Stdio.printf "  Completed in %d cycles\n" !cycle_count;
      
      let hw_result = if is_infinity then None else Some (result_x, result_y) in
      
      Test_utils.print_point "  Result  " hw_result;
      Test_utils.print_point "  Expected" expected;
      
      let pass = Test_utils.points_equal hw_result expected in
      if pass then
        Stdio.printf "  PASS\n\n"
      else
        Stdio.printf "  FAIL\n\n";
      pass
    end
  in
  
  (* Run tests *)
  let results = [
    run_point_double "2G = double(G)" g g2;
    (match g2 with
     | Some pt -> run_point_double "4G = double(2G)" pt g4
     | None -> Stdio.printf "Skipping 4G test (2G is infinity)\n\n"; true);
    (match g4 with
     | Some pt -> run_point_double "8G = double(4G)" pt g8
     | None -> Stdio.printf "Skipping 8G test (4G is infinity)\n\n"; true);
  ] in
  
  (* Summary *)
  let passed = List.count results ~f:Fn.id in
  let total = List.length results in
  Stdio.printf "=== Summary: %d/%d tests passed ===\n" passed total;
  
  if passed = total then
    Stdio.printf "SUCCESS: All point doubling tests passed!\n"
  else
    Stdio.printf "FAILURE: Some tests failed\n"
