open Base
open Hardcaml

let () =
  Stdio.printf "=== ECDSA Full Verification Test ===\n\n";
  
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
  
  Stdio.printf "=== Software Verification ===\n\n";
  let w = mod_inv s order_n in
  let u1 = mod_mul message_hash w order_n in
  let u2 = mod_mul r w order_n in
  Stdio.printf "w = s^(-1) = %s\n" (Z.to_string w);
  Stdio.printf "u1 = e*w = %s\n" (Z.to_string u1);
  Stdio.printf "u2 = r*w = %s\n\n" (Z.to_string u2);
  
  (* Show first few bits *)
  Stdio.printf "First bits of u1 (from bit 255): ";
  for i = 255 downto 250 do
    Stdio.printf "%d" (if Z.testbit u1 i then 1 else 0)
  done;
  Stdio.printf "...\n";
  
  Stdio.printf "First bits of u2 (from bit 255): ";
  for i = 255 downto 250 do
    Stdio.printf "%d" (if Z.testbit u2 i then 1 else 0)
  done;
  Stdio.printf "...\n\n";
  
  let u1_g = scalar_mult u1 (g_x, g_y) in
  let u2_q = scalar_mult u2 (q_x, q_y) in
  let result_point = match (u1_g, u2_q) with
    | (Some p1, Some p2) -> point_add p1 p2
    | (Some p, None) | (None, Some p) -> Some p
    | (None, None) -> None
  in
  let (result_x, result_y) = match result_point with
    | Some p -> p
    | None -> (Z.zero, Z.zero)
  in
  let result_x_mod_n = Z.(erem result_x order_n) in
  Stdio.printf "Expected result point:\n";
  Stdio.printf "  x = %s\n" (Z.to_string result_x);
  Stdio.printf "  y = %s\n" (Z.to_string result_y);
  Stdio.printf "Software verification: %s\n\n" 
    (if Z.equal result_x_mod_n r then "VALID ✓" else "INVALID ✗");
  
  Stdio.printf "=== Hardware Verification ===\n\n";
  
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
  let valid_sig () = Bits.to_bool !(outputs.valid_signature) in
  let acc_x () = bits_to_z !(outputs.dbg_acc_x) in
  let acc_y () = bits_to_z !(outputs.dbg_acc_y) in
  let add_y () = bits_to_z !(outputs.dbg_add_y) in
  
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
  
  Stdio.printf "Starting hardware verification...\n\n";
  
  (* State name lookup *)
  let state_name st = match st with
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
    | 10 -> "Loop_check_bits"
    | 11 -> "Load_add_g_x"
    | 12 -> "Load_add_g_x_wait"
    | 13 -> "Load_add_g_y"
    | 14 -> "Load_add_g_y_wait"
    | 23 -> "Copy_to_acc_x"
    | 24 -> "Copy_to_acc_x_wait"
    | 25 -> "Copy_to_acc_y"
    | 26 -> "Copy_to_acc_y_wait"
    | 81 -> "Loop_next"
    | 82 -> "Final_check"
    | 83 -> "Done"
    | n -> Printf.sprintf "State_%d" n
  in
  
  let max_cycles = 10000000 in
  let cycle_count = ref 0 in
  let last_state = ref (-1) in
  let last_bit_idx = ref (-1) in
  let last_acc_x = ref Z.zero in
  let shown_first_acc = ref false in
  
 while !cycle_count < max_cycles && not (is_done ()) do
    let st = ctrl_state () in
    let bi = bit_idx () in
    let ax = acc_x () in
    
    (* Track state 22 -> 23 transition (after loading qpg_y, before copying to acc) *)
    if !last_state = 22 && st = 23 then begin
      Stdio.printf "\n*** After Load_add_qpg_y_wait (state 22), entering Copy_to_acc_x (state 23) ***\n";
      Stdio.printf "    add_y = %s\n" (Z.to_string (add_y ()));
      Stdio.printf "    Expected qpg_y = %s\n\n" (Z.to_string qpg_y)
    end;

     (* Update last_state for next iteration *)
    last_state := st;
    
    (* Show detailed trace for first 500 cycles *)
    if !cycle_count < 500 then begin
      if st <> !last_state || bi <> !last_bit_idx then begin
        Stdio.printf "  [cycle %d] %s, bit_idx=%d\n" !cycle_count (state_name st) bi;
        last_state := st;
        last_bit_idx := bi
      end
    end else if !cycle_count = 500 then begin
      Stdio.printf "\n=== Continuing... ===\n"
    end;
    
   
    
    (* Show when acc_x changes (first time it becomes non-zero) *)
    if not !shown_first_acc && not (Z.equal ax Z.zero) then begin
      Stdio.printf "\n*** acc_x first became non-zero at cycle %d, state=%s, bit_idx=%d ***\n"
        !cycle_count (state_name st) bi;
      Stdio.printf "    acc_x = %s\n" (Z.to_string ax);
      Stdio.printf "    acc_y = %s\n" (Z.to_string (acc_y ()));
      Stdio.printf "    add_y = %s\n\n" (Z.to_string (add_y ()));
      shown_first_acc := true;
      last_acc_x := ax
    end;
    
    (* Show when acc_x becomes zero again *)
    if !shown_first_acc && Z.equal ax Z.zero && not (Z.equal !last_acc_x Z.zero) then begin
      Stdio.printf "\n*** acc_x became ZERO at cycle %d, state=%s, bit_idx=%d ***\n\n"
        !cycle_count (state_name st) bi
    end;
    last_acc_x := ax;
    
    (* Progress every 50000 cycles *)
    if !cycle_count > 500 && !cycle_count % 50000 = 0 then begin
      Stdio.printf "  [cycle %d] %s, bit_idx=%d, acc_x=%s...\n" 
        !cycle_count (state_name st) bi
        (if Z.equal ax Z.zero then "0" else "non-zero")
    end;
    
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  
  Stdio.printf "\n=== Results ===\n";
  Stdio.printf "Completed in %d cycles\n" !cycle_count;
  Stdio.printf "Final state: %s\n" (state_name (ctrl_state ()));
  Stdio.printf "Final bit_idx: %d\n" (bit_idx ());
  Stdio.printf "Done: %b\n" (is_done ());
  Stdio.printf "Valid signature: %b\n\n" (valid_sig ());
  
  Stdio.printf "=== Final Register Values ===\n";
  Stdio.printf "Hardware acc_x = %s\n" (Z.to_string (acc_x ()));
  Stdio.printf "Hardware acc_y = %s\n" (Z.to_string (acc_y ()));
  Stdio.printf "\nExpected acc_x = %s\n" (Z.to_string result_x);
  Stdio.printf "Expected acc_y = %s\n\n" (Z.to_string result_y);
  
  if is_done () then begin
    if valid_sig () then
      Stdio.printf "✓ Hardware correctly verified the signature!\n"
    else
      Stdio.printf "✗ Hardware rejected the signature (expected valid)\n"
  end else
    Stdio.printf "✗ Timeout - verification did not complete\n"