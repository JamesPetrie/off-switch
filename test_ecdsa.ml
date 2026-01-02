open Base
open Hardcaml

let () =
  Stdio.printf "=== ECDSA Register Loading Debug Test ===\n\n";
  
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
  
  let bits_to_z bits =
    Z.of_string_base 2 (Bits.to_bstr bits)
  in
  
  let prime_p = Ecdsa.Config.prime_p_z in
  let order_n = Ecdsa.Config.order_n_z in
  let g_x = Ecdsa.Config.g_x_z in
  let g_y = Ecdsa.Config.g_y_z in
  
  let mod_sub a b p = Z.(erem (a - b) p) in
  let mod_mul a b p = Z.((a * b) mod p) in
  let mod_inv a p = Z.(invert a p) in
  
  let point_add (px, py) (qx, qy) =
    if Z.equal px qx then
      if Z.equal py qy then None
      else Some (Z.zero, Z.zero)
    else
      let lambda = mod_mul (mod_sub qy py prime_p) (mod_inv (mod_sub qx px prime_p) prime_p) prime_p in
      let xr = mod_sub (mod_sub (mod_mul lambda lambda prime_p) px prime_p) qx prime_p in
      let yr = mod_sub (mod_mul lambda (mod_sub px xr prime_p) prime_p) py prime_p in
      Some (xr, yr)
  in
  
  let point_double (px, py) =
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
          else acc'
        in
        loop acc'' (bit_idx - 1)
    in
    loop None 255
  in
  
  Stdio.printf "=== Generating Valid ECDSA Signature ===\n\n";
  
  let private_key = Z.of_int 12345 in
  let k = Z.of_int 67890 in
  let message_hash = Z.of_int 11111 in
  
  Stdio.printf "Private key d = %s\n" (Z.to_string private_key);
  Stdio.printf "Nonce k = %s\n" (Z.to_string k);
  Stdio.printf "Message hash e = %s\n\n" (Z.to_string message_hash);
  
  let q_point = scalar_mult private_key (g_x, g_y) in
  let (q_x, q_y) = match q_point with 
    | Some p -> p 
    | None -> failwith "Invalid public key"
  in
  Stdio.printf "Public key Q:\n";
  Stdio.printf "  Q.x = %s\n" (Z.to_string q_x);
  Stdio.printf "  Q.y = %s\n\n" (Z.to_string q_y);
  
  let r_point = scalar_mult k (g_x, g_y) in
  let (r_x, _r_y) = match r_point with 
    | Some p -> p 
    | None -> failwith "Invalid R point"
  in
  
  let r = Z.(erem r_x order_n) in
  Stdio.printf "Signature component r = %s\n" (Z.to_string r);
  
  let k_inv = mod_inv k order_n in
  let s = mod_mul k_inv Z.(erem (message_hash + mod_mul r private_key order_n) order_n) order_n in
  Stdio.printf "Signature component s = %s\n\n" (Z.to_string s);
  
  let qpg = point_add (q_x, q_y) (g_x, g_y) in
  let (qpg_x, qpg_y) = match qpg with 
    | Some p -> p 
    | None -> failwith "Invalid Q+G"
  in
  Stdio.printf "Precomputed Q + G:\n";
  Stdio.printf "  (Q+G).x = %s\n" (Z.to_string qpg_x);
  Stdio.printf "  (Q+G).y = %s\n\n" (Z.to_string qpg_y);
  
  Stdio.printf "=== Expected Register Values ===\n";
  Stdio.printf "G.x = %s\n" (Z.to_string g_x);
  Stdio.printf "G.y = %s\n\n" (Z.to_string g_y);
  
  (* Helper functions to read outputs *)
  let ctrl_state () = Bits.to_int !(outputs.dbg_ctrl_state) in
  let qpg_x_reg () = bits_to_z !(outputs.dbg_qpg_x) in
  let qpg_y_reg () = bits_to_z !(outputs.dbg_qpg_y) in
  let q_x_reg () = bits_to_z !(outputs.dbg_q_x) in
  let q_y_reg () = bits_to_z !(outputs.dbg_q_y) in
  let g_x_reg () = bits_to_z !(outputs.dbg_g_x) in
  let g_y_reg () = bits_to_z !(outputs.dbg_g_y) in
  let add_x_reg () = bits_to_z !(outputs.dbg_add_x) in
  let add_y_reg () = bits_to_z !(outputs.dbg_add_y) in
  let acc_x_reg () = bits_to_z !(outputs.dbg_acc_x) in
  let acc_y_reg () = bits_to_z !(outputs.dbg_acc_y) in
  let load_enable () = Bits.to_bool !(outputs.dbg_load_enable) in
  let load_addr () = Bits.to_int !(outputs.dbg_load_addr) in
  let arith_done () = Bits.to_bool !(outputs.dbg_arith_done) in
  let _operand_a () = bits_to_z !(outputs.dbg_arith_operand_a) in
  let _operand_b () = bits_to_z !(outputs.dbg_arith_operand_b) in
  
  let state_name st = match st with
    | 0 -> "Idle"
    | 1 -> "Load_inputs"
    | 2 -> "Init_constants"
    | 3 -> "Validate_r_nonzero"
    | 4 -> "Validate_r_less_than_n"
    | 5 -> "Validate_s_nonzero"
    | 6 -> "Validate_s_less_than_n"
    | 7 -> "Validation_failed"
    | 8 -> "Compute_w"
    | 57 -> "Load_add_qpg_x"
    | 58 -> "Load_add_qpg_x_wait"
    | 59 -> "Load_add_qpg_y"
    | 60 -> "Load_add_qpg_y_wait"
    | 61 -> "Copy_to_acc_x"
    | 62 -> "Copy_to_acc_x_wait"
    | 63 -> "Copy_to_acc_y"
    | 64 -> "Copy_to_acc_y_wait"
    | n -> Printf.sprintf "State_%d" n
  in
  
  Stdio.printf "=== Starting Simulation ===\n\n";
  
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
  
  (* Set inputs and start *)
  inputs.e := z_to_bits message_hash;
  inputs.r := z_to_bits r;
  inputs.s := z_to_bits s;
  inputs.q_x := z_to_bits q_x;
  inputs.q_y := z_to_bits q_y;
  inputs.qplusg_x := z_to_bits qpg_x;
  inputs.qplusg_y := z_to_bits qpg_y;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  (* Run until we reach Validate_r_nonzero (state 3), which is right after Init_constants *)
  let cycle_count = ref 0 in
  while ctrl_state () < 3 && !cycle_count < 100 do
    let st = ctrl_state () in
    if load_enable () then
      Stdio.printf "  [cycle %d] %s: load_enable=true, load_addr=%d\n" 
        !cycle_count (state_name st) (load_addr ());
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  
  Stdio.printf "\n=== Register Values After Loading (at Validate_r_nonzero) ===\n\n";
  
  Stdio.printf "Q.x register:\n";
  Stdio.printf "  Hardware: %s\n" (Z.to_string (q_x_reg ()));
  Stdio.printf "  Expected: %s\n" (Z.to_string q_x);
  Stdio.printf "  Match: %b\n\n" (Z.equal (q_x_reg ()) q_x);
  
  Stdio.printf "Q.y register:\n";
  Stdio.printf "  Hardware: %s\n" (Z.to_string (q_y_reg ()));
  Stdio.printf "  Expected: %s\n" (Z.to_string q_y);
  Stdio.printf "  Match: %b\n\n" (Z.equal (q_y_reg ()) q_y);
  
  Stdio.printf "Q+G.x register:\n";
  Stdio.printf "  Hardware: %s\n" (Z.to_string (qpg_x_reg ()));
  Stdio.printf "  Expected: %s\n" (Z.to_string qpg_x);
  Stdio.printf "  Match: %b\n\n" (Z.equal (qpg_x_reg ()) qpg_x);
  
  Stdio.printf "Q+G.y register:\n";
  Stdio.printf "  Hardware: %s\n" (Z.to_string (qpg_y_reg ()));
  Stdio.printf "  Expected: %s\n" (Z.to_string qpg_y);
  Stdio.printf "  Match: %b\n\n" (Z.equal (qpg_y_reg ()) qpg_y);
  
  Stdio.printf "G.x register:\n";
  Stdio.printf "  Hardware: %s\n" (Z.to_string (g_x_reg ()));
  Stdio.printf "  Expected: %s\n" (Z.to_string g_x);
  Stdio.printf "  Match: %b\n\n" (Z.equal (g_x_reg ()) g_x);
  
  Stdio.printf "G.y register:\n";
  Stdio.printf "  Hardware: %s\n" (Z.to_string (g_y_reg ()));
  Stdio.printf "  Expected: %s\n" (Z.to_string g_y);
  Stdio.printf "  Match: %b\n\n" (Z.equal (g_y_reg ()) g_y);
  
  (* Now run until we get to Load_add_qpg_x (state 57) *)
  Stdio.printf "=== Running to Load_add_qpg operations ===\n\n";
  
  while ctrl_state () < 57 && !cycle_count < 2000 do
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  
  Stdio.printf "Reached state %s at cycle %d\n\n" (state_name (ctrl_state ())) !cycle_count;
  
(* Trace through the Load_add_qpg_x and Load_add_qpg_y operations in detail *)
  Stdio.printf "=== Cycle-by-cycle trace ===\n\n";
  
  let arith_state () = Bits.to_int !(outputs.dbg_arith_state) in
  let arith_state_name st = match st with
    | 0 -> "Idle"
    | 1 -> "Load" 
    | 2 -> "Capture"
    | 3 -> "Compute"
    | 4 -> "Write"
    | 5 -> "Ready"
    | _ -> "?"
  in

  for _ = 1 to 50 do
    let st = ctrl_state () in
    let ast = arith_state () in
    Stdio.printf "[cycle %d] ctrl=%s arith=%s done=%b add_x=%s add_y=%s\n"
      !cycle_count
      (state_name st)
      (arith_state_name ast)
      (arith_done ())
      (if Z.equal (add_x_reg ()) qpg_x then "qpg_x" else if Z.equal (add_x_reg ()) Z.zero then "0" else "other")
      (if Z.equal (add_y_reg ()) qpg_y then "qpg_y" 
       else if Z.equal (add_y_reg ()) qpg_x then "qpg_x!" 
       else if Z.equal (add_y_reg ()) Z.zero then "0"
       else "other");
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;

  
  Stdio.printf "\n=== Final Values After Copy Operations ===\n\n";
  Stdio.printf "add_x = %s\n" (Z.to_string (add_x_reg ()));
  Stdio.printf "add_y = %s\n" (Z.to_string (add_y_reg ()));
  Stdio.printf "acc_x = %s\n" (Z.to_string (acc_x_reg ()));
  Stdio.printf "acc_y = %s\n\n" (Z.to_string (acc_y_reg ()));
  
  Stdio.printf "Expected:\n";
  Stdio.printf "add_x = %s\n" (Z.to_string qpg_x);
  Stdio.printf "add_y = %s\n" (Z.to_string qpg_y);
  Stdio.printf "acc_x = %s\n" (Z.to_string qpg_x);
  Stdio.printf "acc_y = %s\n\n" (Z.to_string qpg_y);
  
  (* Summary *)
  Stdio.printf "=== Summary ===\n";
  if Z.equal (acc_x_reg ()) qpg_x && Z.equal (acc_y_reg ()) qpg_y then
    Stdio.printf "SUCCESS: acc correctly contains Q+G\n"
  else begin
    Stdio.printf "FAILURE: acc does not contain Q+G\n";
    if Z.equal (acc_x_reg ()) qpg_x then
      Stdio.printf "  acc_x is correct\n"
    else
      Stdio.printf "  acc_x is WRONG\n";
    if Z.equal (acc_y_reg ()) qpg_y then
      Stdio.printf "  acc_y is correct\n"
    else if Z.equal (acc_y_reg ()) qpg_x then
      Stdio.printf "  acc_y contains qpg_x (WRONG - contains x instead of y!)\n"
    else if Z.equal (acc_y_reg ()) (acc_x_reg ()) then
      Stdio.printf "  acc_y equals acc_x (WRONG - y got same value as x!)\n"
    else
      Stdio.printf "  acc_y is WRONG (unknown value)\n"
  end