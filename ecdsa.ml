open Base
open Hardcaml
open Signal

module Config = struct
  let width = 256
  let reg_addr_width = 5
  let num_steps = 40
  
  let t0 = 0
  let t1 = 1
  let t2 = 2
  let t3 = 3
  let t4 = 4
  let t5 = 5
  let x3 = 6
  let y3 = 7
  let z3 = 8
  let x1 = 9
  let y1 = 10
  let z1 = 11
  let x2 = 12
  let y2 = 13
  let z2 = 14
  let param_a = 15
  let param_b3 = 16
  
  let num_regs = 17
  
  (* Generator point G *)
  let generator_x = Z.of_string "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
  let generator_y = Z.of_string "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
  let generator_z = Z.one
  
  (* Public key Q = 2G *)
  let q_x = Z.of_string "0xc6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5"
  let q_y = Z.of_string "12158399299693830322967808612713398636155367887041628176798871954788371653930"
  let q_z = Z.one
  
  (* Precomputed G + Q = 3G *)
  let gpq_x = Z.of_string "0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9"
  let gpq_y = Z.of_string "0x388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672"
  let gpq_z = Z.one
  
  (* Point at infinity *)
  let infinity_x = Z.zero
  let infinity_y = Z.one
  let infinity_z = Z.zero
end

module Op = struct
  let add = 0
  let sub = 1
  let mul = 2
  let inv = 3
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; z : 'a [@bits Config.width]       (* message hash *)
    ; r : 'a [@bits Config.width]       (* signature r *)
    ; s : 'a [@bits Config.width]       (* signature s *)
    ; param_a : 'a [@bits Config.width]
    ; param_b3 : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { busy : 'a
    ; done_ : 'a
    ; x : 'a [@bits Config.width]
    ; y : 'a [@bits Config.width]
    ; z_out : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
    | Prep_op
    | Loop
    | Load
    | Run_add
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

type instr = { op : int; src1 : int; src2 : int; dst : int }

let program = [|
{ op = Op.mul; src1 = Config.x1; src2 = Config.x2; dst = Config.t0 };
  { op = Op.mul; src1 = Config.y1; src2 = Config.y2; dst = Config.t1 };
  { op = Op.mul; src1 = Config.z1; src2 = Config.z2; dst = Config.t2 };
  { op = Op.add; src1 = Config.x1; src2 = Config.y1; dst = Config.t3 };
  { op = Op.add; src1 = Config.x2; src2 = Config.y2; dst = Config.t4 };
  { op = Op.mul; src1 = Config.t3; src2 = Config.t4; dst = Config.t3 };
  { op = Op.add; src1 = Config.t0; src2 = Config.t1; dst = Config.t4 };
  { op = Op.sub; src1 = Config.t3; src2 = Config.t4; dst = Config.t3 };
  { op = Op.add; src1 = Config.x1; src2 = Config.z1; dst = Config.t4 };
  { op = Op.add; src1 = Config.x2; src2 = Config.z2; dst = Config.t5 };
  { op = Op.mul; src1 = Config.t4; src2 = Config.t5; dst = Config.t4 };
  { op = Op.add; src1 = Config.t0; src2 = Config.t2; dst = Config.t5 };
  { op = Op.sub; src1 = Config.t4; src2 = Config.t5; dst = Config.t4 };
  { op = Op.add; src1 = Config.y1; src2 = Config.z1; dst = Config.t5 };
  { op = Op.add; src1 = Config.y2; src2 = Config.z2; dst = Config.x3 };
  { op = Op.mul; src1 = Config.t5; src2 = Config.x3; dst = Config.t5 };
  { op = Op.add; src1 = Config.t1; src2 = Config.t2; dst = Config.x3 };
  { op = Op.sub; src1 = Config.t5; src2 = Config.x3; dst = Config.t5 };
  { op = Op.mul; src1 = Config.param_a; src2 = Config.t4; dst = Config.z3 };
  { op = Op.mul; src1 = Config.param_b3; src2 = Config.t2; dst = Config.x3 };
  { op = Op.add; src1 = Config.x3; src2 = Config.z3; dst = Config.z3 };
  { op = Op.sub; src1 = Config.t1; src2 = Config.z3; dst = Config.x3 };
  { op = Op.add; src1 = Config.t1; src2 = Config.z3; dst = Config.z3 };
  { op = Op.mul; src1 = Config.x3; src2 = Config.z3; dst = Config.y3 };
  { op = Op.add; src1 = Config.t0; src2 = Config.t0; dst = Config.t1 };
  { op = Op.add; src1 = Config.t1; src2 = Config.t0; dst = Config.t1 };
  { op = Op.mul; src1 = Config.param_a; src2 = Config.t2; dst = Config.t2 };
  { op = Op.mul; src1 = Config.param_b3; src2 = Config.t4; dst = Config.t4 };
  { op = Op.add; src1 = Config.t1; src2 = Config.t2; dst = Config.t1 };
  { op = Op.sub; src1 = Config.t0; src2 = Config.t2; dst = Config.t2 };
  { op = Op.mul; src1 = Config.param_a; src2 = Config.t2; dst = Config.t2 };
  { op = Op.add; src1 = Config.t4; src2 = Config.t2; dst = Config.t4 };
  { op = Op.mul; src1 = Config.t1; src2 = Config.t4; dst = Config.t0 };
  { op = Op.add; src1 = Config.y3; src2 = Config.t0; dst = Config.y1 };
  { op = Op.mul; src1 = Config.t5; src2 = Config.t4; dst = Config.t0 };
  { op = Op.mul; src1 = Config.t3; src2 = Config.x3; dst = Config.x3 };
  { op = Op.sub; src1 = Config.x3; src2 = Config.t0; dst = Config.x1 };
  { op = Op.mul; src1 = Config.t3; src2 = Config.t1; dst = Config.t0 };
  { op = Op.mul; src1 = Config.t5; src2 = Config.z3; dst = Config.z3 };
  { op = Op.add; src1 = Config.z3; src2 = Config.t0; dst = Config.z1 };
|]

(* Helper to extract a bit from a signal using a signal index *)
let bit_select_dynamic signal index_signal =
  let width = Signal.width signal in
  let bits = List.init width ~f:(fun i -> bit signal i) in
  mux index_signal bits

let create scope (i : _ I.t) =
  let open Always in
  let width = Config.width in
  let addr_width = Config.reg_addr_width in
  
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in
  
  let reg_file = Array.init Config.num_regs ~f:(fun _ -> Variable.reg spec ~width) in
  
  (* Hardcoded point constants *)
  let const_gx = of_z ~width Config.generator_x in
  let const_gy = of_z ~width Config.generator_y in
  let const_gz = of_z ~width Config.generator_z in
  
  let const_qx = of_z ~width Config.q_x in
  let const_qy = of_z ~width Config.q_y in
  let const_qz = of_z ~width Config.q_z in
  
  let const_gpqx = of_z ~width Config.gpq_x in
  let const_gpqy = of_z ~width Config.gpq_y in
  let const_gpqz = of_z ~width Config.gpq_z in
  
  (* Input registers *)
  let z_reg = Variable.reg spec ~width in
  let r_reg = Variable.reg spec ~width in
  let s_reg = Variable.reg spec ~width in
  
  (* Prep phase registers *)
  let w_reg = Variable.reg spec ~width in      (* s^(-1) mod n *)
  let u1_reg = Variable.reg spec ~width in     (* z * w mod n *)
  let u2_reg = Variable.reg spec ~width in     (* r * w mod n *)
  let prep_step = Variable.reg spec ~width:2 in
  let prep_op_started = Variable.reg spec ~width:1 in
  
  (* Main loop registers *)
  let bit_pos = Variable.reg spec ~width:8 in
  let doubling = Variable.reg spec ~width:1 in
  let last_step = Variable.reg spec ~width:1 in
  let step = Variable.reg spec ~width:6 in
  let load_idx = Variable.reg spec ~width:2 in
  let op_started = Variable.reg spec ~width:1 in
  
  (* Latched second operand registers *)
  let x2_latched = Variable.reg spec ~width in
  let y2_latched = Variable.reg spec ~width in
  let z2_latched = Variable.reg spec ~width in
  
  let out_x = Variable.reg spec ~width in
  let out_y = Variable.reg spec ~width in
  let out_z = Variable.reg spec ~width in
  let done_flag = Variable.reg spec ~width:1 in
  
  let current_bit_u = bit_select_dynamic u1_reg.value bit_pos.value in
  let current_bit_v = bit_select_dynamic u2_reg.value bit_pos.value in
  
  (* Check if P is point at infinity (z1 = 0) *)
  let p_is_infinity = reg_file.(Config.z1).value ==:. 0 in
  
  (* Instruction decode for point addition *)
  let default_instr = of_int ~width:addr_width 0 in
  let decode field =
    mux step.value 
      (Array.to_list (Array.map program ~f:(fun instr -> 
        of_int ~width:addr_width (field instr))))
    |> fun s -> mux2 (step.value >=:. Config.num_steps) default_instr s
  in
  let decode_op =
    mux step.value
      (Array.to_list (Array.map program ~f:(fun instr ->
        of_int ~width:2 instr.op)))
    |> fun s -> mux2 (step.value >=:. Config.num_steps) (zero 2) s
  in
  
  let current_dst = decode (fun instr -> instr.dst) in
  let current_src1 = decode (fun instr -> instr.src1) in
  let current_src2 = decode (fun instr -> instr.src2) in
  let current_op_from_program = decode_op in
  
  (* Arith control signals *)
  let arith_start = Variable.wire ~default:gnd in
  let arith_op = Variable.wire ~default:(zero 2) in
  let arith_prime_sel = Variable.wire ~default:gnd in
  let arith_a = Variable.wire ~default:(zero width) in
  let arith_b = Variable.wire ~default:(zero width) in
  
  (* Use latched values for x2/y2/z2 in point addition *)
  let reg_read idx =
    let base_values = Array.to_list (Array.map reg_file ~f:(fun r -> r.value)) in
    let base = mux idx base_values in
    mux2 (idx ==:. Config.x2) x2_latched.value
      (mux2 (idx ==:. Config.y2) y2_latched.value
        (mux2 (idx ==:. Config.z2) z2_latched.value base))
  in
  
  let arith_read_data_a = 
    mux2 (sm.is Prep_op) arith_a.value (reg_read current_src1)
  in
  let arith_read_data_b = 
    mux2 (sm.is Prep_op) arith_b.value (reg_read current_src2)
  in
  
  let arith_op_selected =
    mux2 (sm.is Prep_op) arith_op.value current_op_from_program
  in
  
  let arith_prime_sel_selected =
    mux2 (sm.is Prep_op) arith_prime_sel.value gnd
  in
  
  let arith_out = Arith.create (Scope.sub_scope scope "arith")
    { Arith.I.
      clock = i.clock
    ; clear = i.clear
    ; start = arith_start.value
    ; op = arith_op_selected
    ; prime_sel = arith_prime_sel_selected
    ; addr_a = current_src1
    ; addr_b = current_src2
    ; addr_out = current_dst
    ; reg_read_data_a = arith_read_data_a
    ; reg_read_data_b = arith_read_data_b
    }
  in
  
  compile [
    done_flag <-- gnd;
    
    sm.switch [
      State.Idle, [
        when_ i.start [
          z_reg <-- i.z;
          r_reg <-- i.r;
          s_reg <-- i.s;
          prep_step <--. 0;
          prep_op_started <-- gnd;
          reg_file.(Config.param_a) <-- i.param_a;
          reg_file.(Config.param_b3) <-- i.param_b3;
          sm.set_next Prep_op;
        ];
      ];
      
      State.Prep_op, [
        (* Set up arith inputs based on prep_step *)
        arith_prime_sel <-- vdd;  (* All prep ops use mod n *)
        
        if_ (prep_step.value ==:. 0) [
          (* w = s^(-1) mod n *)
          arith_op <--. Op.inv;
          arith_a <-- s_reg.value;
          arith_b <-- zero width;  (* unused for inv *)
        ] @@ elif (prep_step.value ==:. 1) [
          (* u1 = z * w mod n *)
          arith_op <--. Op.mul;
          arith_a <-- z_reg.value;
          arith_b <-- w_reg.value;
        ] [
          (* u2 = r * w mod n *)
          arith_op <--. Op.mul;
          arith_a <-- r_reg.value;
          arith_b <-- w_reg.value;
        ];
        
        if_ (~:(prep_op_started.value)) [
          arith_start <-- vdd;
          prep_op_started <-- vdd;
        ] [
          when_ arith_out.done_ [
            (* Store result *)
            if_ (prep_step.value ==:. 0) [
              w_reg <-- arith_out.reg_write_data;
            ] @@ elif (prep_step.value ==:. 1) [
              u1_reg <-- arith_out.reg_write_data;
            ] [
              u2_reg <-- arith_out.reg_write_data;
            ];
            
            if_ (prep_step.value ==:. 2) [
              (* Initialize for main loop *)
              bit_pos <--. 255;
              doubling <-- vdd;
              last_step <-- gnd;
              step <-- zero 6;
              op_started <-- gnd;
              reg_file.(Config.x1) <-- of_z ~width Config.infinity_x;
              reg_file.(Config.y1) <-- of_z ~width Config.infinity_y;
              reg_file.(Config.z1) <-- of_z ~width Config.infinity_z;
              sm.set_next Loop;
            ] [
              prep_step <-- prep_step.value +:. 1;
              prep_op_started <-- gnd;
            ];
          ];
        ];
      ];
      
      State.Loop, [
        if_ last_step.value [
          sm.set_next Done;
        ] @@ elif doubling.value [
          (* Doubling phase *)
          doubling <-- gnd;
          if_ p_is_infinity [
            sm.set_next Loop;
          ] [
            load_idx <--. 0;
            sm.set_next Load;
          ];
        ] [
          (* Adding phase *)
          doubling <-- vdd;
          load_idx <-- (current_bit_v @: current_bit_u);
          last_step <-- (bit_pos.value ==:. 0);
          bit_pos <-- bit_pos.value -:. 1;
          
          if_ ((~: current_bit_u) &: (~: current_bit_v)) [
            sm.set_next Loop;
          ] [
            sm.set_next Load;
          ];
        ];
      ];
      
      State.Load, [
        if_ (load_idx.value ==:. 0) [
          x2_latched <-- reg_file.(Config.x1).value;
          y2_latched <-- reg_file.(Config.y1).value;
          z2_latched <-- reg_file.(Config.z1).value;
        ] @@ elif (load_idx.value ==:. 1) [
          x2_latched <-- const_gx;
          y2_latched <-- const_gy;
          z2_latched <-- const_gz;
        ] @@ elif (load_idx.value ==:. 2) [
          x2_latched <-- const_qx;
          y2_latched <-- const_qy;
          z2_latched <-- const_qz;
        ] [
          x2_latched <-- const_gpqx;
          y2_latched <-- const_gpqy;
          z2_latched <-- const_gpqz;
        ];
        step <-- zero 6;
        sm.set_next Run_add;
      ];
      
      State.Run_add, [
        if_ (~:(op_started.value)) [
          arith_start <-- vdd;
          op_started <-- vdd;
        ] [
          when_ arith_out.done_ [
            proc (Array.to_list (Array.mapi reg_file ~f:(fun idx reg ->
              when_ (current_dst ==:. idx) [
                reg <-- arith_out.reg_write_data;
              ])));
            
            if_ (step.value ==:. Config.num_steps - 1) [
              op_started <-- gnd;
              sm.set_next Loop;
            ] [
              step <-- step.value +:. 1;
              op_started <-- gnd;
            ];
          ];
        ];
      ];
      
      State.Done, [
        out_x <-- reg_file.(Config.x1).value;
        out_y <-- reg_file.(Config.y1).value;
        out_z <-- reg_file.(Config.z1).value;
        done_flag <-- vdd;
        sm.set_next Idle;
      ];
    ];
  ];
  
  { O.
    busy = ~:(sm.is Idle)
  ; done_ = done_flag.value
  ; x = out_x.value
  ; y = out_y.value
  ; z_out = out_z.value
  }