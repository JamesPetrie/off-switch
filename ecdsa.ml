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
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; scalar_u : 'a [@bits Config.width]  (* scalar for G *)
    ; scalar_v : 'a [@bits Config.width]  (* scalar for Q *)
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
    ; z : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
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
  
  let scalar_u_reg = Variable.reg spec ~width in
  let scalar_v_reg = Variable.reg spec ~width in
  let bit_pos = Variable.reg spec ~width:8 in
  let doubling = Variable.reg spec ~width:1 in
  let last_step = Variable.reg spec ~width:1 in
  let step = Variable.reg spec ~width:6 in
  let load_idx = Variable.reg spec ~width:2 in  (* 0=P, 1=G, 2=Q, 3=G+Q *)
  let op_started = Variable.reg spec ~width:1 in
  
  (* Latched second operand registers *)
  let x2_latched = Variable.reg spec ~width in
  let y2_latched = Variable.reg spec ~width in
  let z2_latched = Variable.reg spec ~width in
  
  let out_x = Variable.reg spec ~width in
  let out_y = Variable.reg spec ~width in
  let out_z = Variable.reg spec ~width in
  let done_flag = Variable.reg spec ~width:1 in
  
  let current_bit_u = bit_select_dynamic scalar_u_reg.value bit_pos.value in
  let current_bit_v = bit_select_dynamic scalar_v_reg.value bit_pos.value in
  
  (* Check if P is point at infinity (z1 = 0) *)
  let p_is_infinity = reg_file.(Config.z1).value ==:. 0 in
  
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
  let current_op = decode_op in
  
  let arith_start = Variable.wire ~default:gnd in
  
  (* Use latched values for x2/y2/z2 *)
  let reg_read idx =
    let base_values = Array.to_list (Array.map reg_file ~f:(fun r -> r.value)) in
    let base = mux idx base_values in
    mux2 (idx ==:. Config.x2) x2_latched.value
      (mux2 (idx ==:. Config.y2) y2_latched.value
        (mux2 (idx ==:. Config.z2) z2_latched.value base))
  in
  
  let arith_read_data_a = reg_read current_src1 in
  let arith_read_data_b = reg_read current_src2 in
  
  let arith_out = Arith.create (Scope.sub_scope scope "arith")
    { Arith.I.
      clock = i.clock
    ; clear = i.clear
    ; start = arith_start.value
    ; op = current_op
    ; prime_sel = gnd
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
          scalar_u_reg <-- i.scalar_u;
          scalar_v_reg <-- i.scalar_v;
          bit_pos <--. 255;
          doubling <-- vdd;
          last_step <-- gnd;
          step <-- zero 6;
          op_started <-- gnd;
          reg_file.(Config.x1) <-- of_z ~width Config.infinity_x;
          reg_file.(Config.y1) <-- of_z ~width Config.infinity_y;
          reg_file.(Config.z1) <-- of_z ~width Config.infinity_z;
          reg_file.(Config.param_a) <-- i.param_a;
          reg_file.(Config.param_b3) <-- i.param_b3;
          sm.set_next Loop;
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
            load_idx <--. 0;  (* Load P *)
            sm.set_next Load;
          ];
        ] [
          (* Adding phase *)
          doubling <-- vdd;
          load_idx <-- (current_bit_v @: current_bit_u);  (* {v[i], u[i]} as 2-bit value *)
          last_step <-- (bit_pos.value ==:. 0);
          bit_pos <-- bit_pos.value -:. 1;
          
          if_ ((~: current_bit_u) &: (~: current_bit_v)) [
            (* load_idx = 0, skip add *)
            sm.set_next Loop;
          ] [
            sm.set_next Load;
          ];
        ];
      ];
      
      State.Load, [
        (* Latch second operand based on load_idx *)
        (* 0=P, 1=G, 2=Q, 3=G+Q *)
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
          (* load_idx = 3 *)
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
  ; z = out_z.value
  }