open Base
open Hardcaml

let () =
  Stdio.printf "=== Point Operations Test ===\n\n";
  
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
  
  (* secp256k1 parameters *)
  let prime_p = Ecdsa.Config.prime_p_z in
  let g_x = Ecdsa.Config.g_x_z in
  let g_y = Ecdsa.Config.g_y_z in
  
  (* Modular arithmetic helpers *)
  let mod_sub a b p = Z.(erem (a - b) p) in
  let mod_mul a b p = Z.((a * b) mod p) in
  let mod_inv a p = Z.(invert a p) in
  
  (* Point addition: P + Q *)
  let point_add (px, py) (qx, qy) =
    if Z.equal px qx then
      if Z.equal py qy then
        None
      else
        Some (Z.zero, Z.zero)
    else
      let lambda = mod_mul (mod_sub qy py prime_p) (mod_inv (mod_sub qx px prime_p) prime_p) prime_p in
      let xr = mod_sub (mod_sub (mod_mul lambda lambda prime_p) px prime_p) qx prime_p in
      let yr = mod_sub (mod_mul lambda (mod_sub px xr prime_p) prime_p) py prime_p in
      Some (xr, yr)
  in
  
  (* Point doubling: 2P *)
  let point_double (px, py) =
    if Z.equal py Z.zero then
      (Z.zero, Z.zero)
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
  
  (* Scalar multiplication: k * P *)
  let scalar_mult k (px, py) =
    let rec loop acc bit_idx =
      if bit_idx < 0 then acc
      else
        let acc' = match acc with
          | None -> None
          | Some p -> Some (point_double p)
        in
        let bit_set = Z.testbit k bit_idx in
        let acc'' = 
          if bit_set then
            match acc' with
            | None -> Some (px, py)
            | Some a -> point_add a (px, py)
          else
            acc'
        in
        loop acc'' (bit_idx - 1)
    in
    loop None 255
  in
  
  Stdio.printf "1. Computing reference values in software...\n\n";
  
  let (g2x, g2y) = point_double (g_x, g_y) in
  Stdio.printf "   G = (%s, %s)\n" (Z.to_string g_x) (Z.to_string g_y);
  Stdio.printf "   2G = (%s, %s)\n" (Z.to_string g2x) (Z.to_string g2y);
  
  let qpg = point_add (g2x, g2y) (g_x, g_y) in
  let (qpg_x, qpg_y) = match qpg with Some p -> p | None -> (Z.zero, Z.zero) in
  Stdio.printf "   Q + G = (%s, %s)\n\n" (Z.to_string qpg_x) (Z.to_string qpg_y);
  
  let test_e = Z.of_int 3 in
  let test_r = Z.of_int 5 in
  let test_s = Z.one in
  let test_q_x = g2x in
  let test_q_y = g2y in
  
  let expected_result = scalar_mult (Z.of_int 13) (g_x, g_y) in
  let (exp_x, exp_y) = match expected_result with Some p -> p | None -> (Z.zero, Z.zero) in
  Stdio.printf "   Expected result (13G):\n";
  Stdio.printf "   x = %s\n" (Z.to_string exp_x);
  Stdio.printf "   y = %s\n\n" (Z.to_string exp_y);
  
  let u1_g = scalar_mult (Z.of_int 3) (g_x, g_y) in
  let u2_q = scalar_mult (Z.of_int 5) (g2x, g2y) in
  Stdio.printf "   Verification:\n";
  (match u1_g with 
   | Some (x, y) -> Stdio.printf "   3*G = (%s, %s)\n" (Z.to_string x) (Z.to_string y)
   | None -> Stdio.printf "   3*G = infinity\n");
  (match u2_q with
   | Some (x, y) -> Stdio.printf "   5*Q = (%s, %s)\n" (Z.to_string x) (Z.to_string y)
   | None -> Stdio.printf "   5*Q = infinity\n");
  
  let combined = match (u1_g, u2_q) with
    | (Some p1, Some p2) -> point_add p1 p2
    | (Some p, None) | (None, Some p) -> Some p
    | (None, None) -> None
  in
  (match combined with
   | Some (x, y) -> Stdio.printf "   3*G + 5*Q = (%s, %s)\n\n" (Z.to_string x) (Z.to_string y)
   | None -> Stdio.printf "   3*G + 5*Q = infinity\n\n");
  
  Stdio.printf "2. Running hardware simulation...\n\n";
  
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
  
  inputs.e := z_to_bits test_e;
  inputs.r := z_to_bits test_r;
  inputs.s := z_to_bits test_s;
  inputs.q_x := z_to_bits test_q_x;
  inputs.q_y := z_to_bits test_q_y;
  inputs.qplusg_x := z_to_bits qpg_x;
  inputs.qplusg_y := z_to_bits qpg_y;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  let ctrl_state () = Bits.to_int !(outputs.dbg_ctrl_state) in
  let bit_idx () = Bits.to_int !(outputs.dbg_bit_idx) in
  let is_done () = Bits.to_bool !(outputs.done_) in
  let valid_sig () = Bits.to_bool !(outputs.valid_signature) in
  
  let max_cycles = 500000 in
  let cycle_count = ref 0 in
  let last_bit_idx = ref 256 in
  
  while !cycle_count < max_cycles && not (is_done ()) do
    let bi = bit_idx () in
    if bi <> !last_bit_idx && bi < 10 then begin
      Stdio.printf "   [cycle %d] bit_idx=%d, state=%d\n" !cycle_count bi (ctrl_state ());
      last_bit_idx := bi
    end;
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  
  Stdio.printf "\n3. Results:\n";
  Stdio.printf "   Completed in %d cycles\n" !cycle_count;
  Stdio.printf "   Final state: %d\n" (ctrl_state ());
  Stdio.printf "   Done: %b\n" (is_done ());
  Stdio.printf "   Valid signature: %b\n\n" (valid_sig ());
  
  if is_done () then
    Stdio.printf "   Hardware computation completed successfully!\n"
  else
    Stdio.printf "   ERROR: Computation timed out\n"