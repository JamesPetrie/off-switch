open Base
open Hardcaml
open Ecdsa

let () =
  Stdio.printf "=== Point Doubling Unit Test ===\n\n";
  
  (* Compute expected results *)
  let g = (Test_utils.g_x, Test_utils.g_y) in
  let g2 = Test_utils.point_double g in
  let g4 = Option.bind g2 ~f:Test_utils.point_double in
  let g8 = Option.bind g4 ~f:Test_utils.point_double in
  
  Stdio.printf "Expected values:\n";
  Stdio.printf "G   = (%s..., %s...)\n" 
    (String.prefix (Z.to_string (fst g)) 30)
    (String.prefix (Z.to_string (snd g)) 30);
  (match g2 with
  | Some (x, y) -> 
    Stdio.printf "2G  = (%s..., %s...)\n"
      (String.prefix (Z.to_string x) 30)
      (String.prefix (Z.to_string y) 30)
  | None -> Stdio.printf "2G  = infinity\n");
  (match g4 with
  | Some (x, y) ->
    Stdio.printf "4G  = (%s..., %s...)\n"
      (String.prefix (Z.to_string x) 30)
      (String.prefix (Z.to_string y) 30)
  | None -> Stdio.printf "4G  = infinity\n");
  (match g8 with
  | Some (x, y) ->
    Stdio.printf "8G  = (%s..., %s...)\n"
      (String.prefix (Z.to_string x) 30)
      (String.prefix (Z.to_string y) 30)
  | None -> Stdio.printf "8G  = infinity\n");
  Stdio.printf "\n";
  
  (* Create simulator *)
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(EcdsaVerify.I)(EcdsaVerify.O) in
  let sim = Sim.create (EcdsaVerify.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  (* Helper to load a register *)
  let load_reg addr value =
    inputs.load_enable := Bits.vdd;
    inputs.load_addr := Bits.of_int ~width:Config.reg_addr_width addr;
    inputs.load_data := Test_utils.z_to_bits value;
    Cyclesim.cycle sim;
    inputs.load_enable := Bits.gnd
  in
  
  (* Helper to run point doubling and return result *)
  let run_point_double (px, py) =
    (* Reset *)
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.test_mode := Bits.of_int ~width:2 Config.TestMode.point_double;
    inputs.load_enable := Bits.gnd;
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
    
    (* Load input point into acc_x, acc_y *)
    load_reg Config.Reg.acc_x px;
    load_reg Config.Reg.acc_y py;
    
    (* Also need to load constants *)
    load_reg Config.Reg.zero Z.zero;
    load_reg Config.Reg.one Z.one;
    load_reg Config.Reg.two (Z.of_int 2);
    load_reg Config.Reg.three (Z.of_int 3);
    
    (* Start point doubling *)
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    
    (* Wait for completion *)
    let max_cycles = 10000 in
    let cycle_count = ref 0 in
    while !cycle_count < max_cycles && not (Bits.to_bool !(outputs.done_)) do
      Cyclesim.cycle sim;
      Int.incr cycle_count
    done;
    
    let result_x = Test_utils.bits_to_z !(outputs.dbg_acc_x) in
    let result_y = Test_utils.bits_to_z !(outputs.dbg_acc_y) in
    let is_infinity = Bits.to_bool !(outputs.dbg_acc_is_infinity) in
    
    Stdio.printf "  Completed in %d cycles\n" !cycle_count;
    
    if is_infinity then None
    else Some (result_x, result_y)
  in
  
  (* Test 1: Double G to get 2G *)
  Stdio.printf "Test 1: 2G = double(G)\n";
  let result = run_point_double g in
  (match result, g2 with
  | Some (rx, ry), Some (ex, ey) ->
    let x_match = Z.equal rx ex in
    let y_match = Z.equal ry ey in
    Stdio.printf "  Result:   (%s..., %s...)\n"
      (String.prefix (Z.to_string rx) 30)
      (String.prefix (Z.to_string ry) 30);
    Stdio.printf "  Expected: (%s..., %s...)\n"
      (String.prefix (Z.to_string ex) 30)
      (String.prefix (Z.to_string ey) 30);
    Stdio.printf "  X match: %b, Y match: %b\n" x_match y_match;
    if x_match && y_match then Stdio.printf "  PASS\n\n" else Stdio.printf "  FAIL\n\n"
  | None, None ->
    Stdio.printf "  Both infinity - PASS\n\n"
  | _ ->
    Stdio.printf "  Mismatch (one infinity, one not) - FAIL\n\n");
  
  (* Test 2: Double 2G to get 4G *)
  Stdio.printf "Test 2: 4G = double(2G)\n";
  (match g2 with
  | Some pt ->
    let result = run_point_double pt in
    (match result, g4 with
    | Some (rx, ry), Some (ex, ey) ->
      let x_match = Z.equal rx ex in
      let y_match = Z.equal ry ey in
      Stdio.printf "  Result:   (%s..., %s...)\n"
        (String.prefix (Z.to_string rx) 30)
        (String.prefix (Z.to_string ry) 30);
      Stdio.printf "  Expected: (%s..., %s...)\n"
        (String.prefix (Z.to_string ex) 30)
        (String.prefix (Z.to_string ey) 30);
      Stdio.printf "  X match: %b, Y match: %b\n" x_match y_match;
      if x_match && y_match then Stdio.printf "  PASS\n\n" else Stdio.printf "  FAIL\n\n"
    | None, None ->
      Stdio.printf "  Both infinity - PASS\n\n"
    | _ ->
      Stdio.printf "  Mismatch - FAIL\n\n")
  | None ->
    Stdio.printf "  Skipped (2G is infinity)\n\n");
  
  (* Test 3: Double 4G to get 8G *)
  Stdio.printf "Test 3: 8G = double(4G)\n";
  (match g4 with
  | Some pt ->
    let result = run_point_double pt in
    (match result, g8 with
    | Some (rx, ry), Some (ex, ey) ->
      let x_match = Z.equal rx ex in
      let y_match = Z.equal ry ey in
      Stdio.printf "  Result:   (%s..., %s...)\n"
        (String.prefix (Z.to_string rx) 30)
        (String.prefix (Z.to_string ry) 30);
      Stdio.printf "  Expected: (%s..., %s...)\n"
        (String.prefix (Z.to_string ex) 30)
        (String.prefix (Z.to_string ey) 30);
      Stdio.printf "  X match: %b, Y match: %b\n" x_match y_match;
      if x_match && y_match then Stdio.printf "  PASS\n\n" else Stdio.printf "  FAIL\n\n"
    | None, None ->
      Stdio.printf "  Both infinity - PASS\n\n"
    | _ ->
      Stdio.printf "  Mismatch - FAIL\n\n")
  | None ->
    Stdio.printf "  Skipped (4G is infinity)\n\n");
  
  Stdio.printf "=== Point Doubling Tests Complete ===\n"
