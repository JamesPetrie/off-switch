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
  
  let generator_x = Z.of_string "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
  let generator_y = Z.of_string "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
  let generator_z = Z.one
  
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
    ; scalar : 'a [@bits Config.width]
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

(* Debug output module with extra signals *)
module O_debug = struct
  type 'a t =
    { busy : 'a
    ; done_ : 'a
    ; x : 'a [@bits Config.width]
    ; y : 'a [@bits Config.width]
    ; z : 'a [@bits Config.width]
    ; dbg_state : 'a [@bits 3]
    ; dbg_bit_pos : 'a [@bits 8]
    ; dbg_j : 'a
    ; dbg_step : 'a [@bits 6]
    ; dbg_op_started : 'a
    ; dbg_current_bit : 'a
    ; dbg_arith_done : 'a
    ; dbg_arith_busy : 'a
    ; dbg_src1 : 'a [@bits 5]
    ; dbg_src2 : 'a [@bits 5]
    ; dbg_dst : 'a [@bits 5]
    ; dbg_op : 'a [@bits 2]
    ; dbg_operand_a_lo : 'a [@bits 32]
    ; dbg_operand_b_lo : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
    | Init
    | Wait_init
    | Run_step
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

let create_impl scope (i : _ I.t) ~include_debug =
  let open Always in
  let width = Config.width in
  let addr_width = Config.reg_addr_width in
  
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in
  
  let reg_file = Array.init Config.num_regs ~f:(fun _ -> Variable.reg spec ~width) in
  
  let const_gx = of_z ~width Config.generator_x in
  let const_gy = of_z ~width Config.generator_y in
  let const_gz = of_z ~width Config.generator_z in
  
  let scalar_reg = Variable.reg spec ~width in
  let bit_pos = Variable.reg spec ~width:8 in
  let j = Variable.reg spec ~width:1 in
  let step = Variable.reg spec ~width:6 in
  let op_started = Variable.reg spec ~width:1 in
  
  let out_x = Variable.reg spec ~width in
  let out_y = Variable.reg spec ~width in
  let out_z = Variable.reg spec ~width in
  let done_flag = Variable.reg spec ~width:1 in
  
  let current_bit = bit_select_dynamic scalar_reg.value bit_pos.value in
  
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
  
  let x2_val = mux2 j.value const_gx reg_file.(Config.x1).value in
  let y2_val = mux2 j.value const_gy reg_file.(Config.y1).value in
  let z2_val = mux2 j.value const_gz reg_file.(Config.z1).value in
  
  let arith_start = Variable.wire ~default:gnd in
  
  let reg_read idx =
    let base_values = Array.to_list (Array.map reg_file ~f:(fun r -> r.value)) in
    let base = mux idx base_values in
    mux2 (idx ==:. Config.x2) x2_val
      (mux2 (idx ==:. Config.y2) y2_val
        (mux2 (idx ==:. Config.z2) z2_val base))
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
          scalar_reg <-- i.scalar;
          bit_pos <--. 255;
          j <-- gnd;
          step <-- zero 6;
          op_started <-- gnd;
          sm.set_next Init;
        ];
      ];
      
      State.Init, [
        reg_file.(Config.x1) <-- of_z ~width Config.infinity_x;
        reg_file.(Config.y1) <-- of_z ~width Config.infinity_y;
        reg_file.(Config.z1) <-- of_z ~width Config.infinity_z;
        reg_file.(Config.param_a) <-- i.param_a;
        reg_file.(Config.param_b3) <-- i.param_b3;
        sm.set_next Wait_init;
      ];
      
      State.Wait_init, [
        sm.set_next Run_step;
      ];
      
      State.Run_step, [
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
              step <-- zero 6;
              op_started <-- gnd;
              
              if_ (j.value ==:. 0) [
                if_ current_bit [
                  j <-- vdd;
                ] [
                  if_ (bit_pos.value ==:. 0) [
                    sm.set_next Done;
                  ] [
                    bit_pos <-- bit_pos.value -:. 1;
                  ];
                ];
              ] [
                j <-- gnd;
                if_ (bit_pos.value ==:. 0) [
                  sm.set_next Done;
                ] [
                  bit_pos <-- bit_pos.value -:. 1;
                ];
              ];
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
  
  let base_outputs = 
    { O.
      busy = ~:(sm.is Idle)
    ; done_ = done_flag.value
    ; x = out_x.value
    ; y = out_y.value
    ; z = out_z.value
    }
  in
  
  if include_debug then
    let state_bits = 
      mux2 (sm.is Idle) (of_int ~width:3 0)
        (mux2 (sm.is Init) (of_int ~width:3 1)
          (mux2 (sm.is Wait_init) (of_int ~width:3 2)
            (mux2 (sm.is Run_step) (of_int ~width:3 3)
              (of_int ~width:3 4))))
    in
    `Debug { O_debug.
      busy = base_outputs.busy
    ; done_ = base_outputs.done_
    ; x = base_outputs.x
    ; y = base_outputs.y
    ; z = base_outputs.z
    ; dbg_state = state_bits
    ; dbg_bit_pos = bit_pos.value
    ; dbg_j = j.value
    ; dbg_step = step.value
    ; dbg_op_started = op_started.value
    ; dbg_current_bit = current_bit
    ; dbg_arith_done = arith_out.done_
    ; dbg_arith_busy = arith_out.busy
    ; dbg_src1 = current_src1
    ; dbg_src2 = current_src2
    ; dbg_dst = current_dst
    ; dbg_op = current_op
    ; dbg_operand_a_lo = sel_bottom arith_read_data_a 32
    ; dbg_operand_b_lo = sel_bottom arith_read_data_b 32
    }
  else
    `Normal base_outputs

let create scope i =
  match create_impl scope i ~include_debug:false with
  | `Normal o -> o
  | `Debug _ -> assert false

let create_debug scope i =
  match create_impl scope i ~include_debug:true with
  | `Debug o -> o
  | `Normal _ -> assert false