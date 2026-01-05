open Base
open Hardcaml
open Signal

(* Point addition on short Weierstrass curves using projective coordinates.
   
   Implements complete addition formula for E/Fq: y² = x³ + ax + b
   Input:  P = (X1:Y1:Z1), Q = (X2:Y2:Z2) in projective coordinates
   Output: P + Q = (X3:Y3:Z3)
   
   Uses Arith module for field operations (add, sub, mul) mod prime_p.
   Executes 40-step algorithm with step counter and combinational decode.
*)

module Config = struct
  let width = 256
  let reg_addr_width = 5
  let num_steps = 40
  
  (* Register allocation *)
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
    ; x1 : 'a [@bits Config.width]
    ; y1 : 'a [@bits Config.width]
    ; z1 : 'a [@bits Config.width]
    ; x2 : 'a [@bits Config.width]
    ; y2 : 'a [@bits Config.width]
    ; z2 : 'a [@bits Config.width]
    ; param_a : 'a [@bits Config.width]
    ; param_b3 : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { busy : 'a
    ; done_ : 'a
    ; x3 : 'a [@bits Config.width]
    ; y3 : 'a [@bits Config.width]
    ; z3 : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
    | Load_inputs
    | Wait_load      (* NEW: wait for register writes to take effect *)
    | Run_step
    | Output
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

(* Instruction encoding *)
type instr = { op : int; src1 : int; src2 : int; dst : int }

let program = [|
  { op = Op.mul; src1 = Config.x1; src2 = Config.x2; dst = Config.t0 };       (* 0:  t0 <- X1 * X2 *)
  { op = Op.mul; src1 = Config.y1; src2 = Config.y2; dst = Config.t1 };       (* 1:  t1 <- Y1 * Y2 *)
  { op = Op.mul; src1 = Config.z1; src2 = Config.z2; dst = Config.t2 };       (* 2:  t2 <- Z1 * Z2 *)
  { op = Op.add; src1 = Config.x1; src2 = Config.y1; dst = Config.t3 };       (* 3:  t3 <- X1 + Y1 *)
  { op = Op.add; src1 = Config.x2; src2 = Config.y2; dst = Config.t4 };       (* 4:  t4 <- X2 + Y2 *)
  { op = Op.mul; src1 = Config.t3; src2 = Config.t4; dst = Config.t3 };       (* 5:  t3 <- t3 * t4 *)
  { op = Op.add; src1 = Config.t0; src2 = Config.t1; dst = Config.t4 };       (* 6:  t4 <- t0 + t1 *)
  { op = Op.sub; src1 = Config.t3; src2 = Config.t4; dst = Config.t3 };       (* 7:  t3 <- t3 - t4 *)
  { op = Op.add; src1 = Config.x1; src2 = Config.z1; dst = Config.t4 };       (* 8:  t4 <- X1 + Z1 *)
  { op = Op.add; src1 = Config.x2; src2 = Config.z2; dst = Config.t5 };       (* 9:  t5 <- X2 + Z2 *)
  { op = Op.mul; src1 = Config.t4; src2 = Config.t5; dst = Config.t4 };       (* 10: t4 <- t4 * t5 *)
  { op = Op.add; src1 = Config.t0; src2 = Config.t2; dst = Config.t5 };       (* 11: t5 <- t0 + t2 *)
  { op = Op.sub; src1 = Config.t4; src2 = Config.t5; dst = Config.t4 };       (* 12: t4 <- t4 - t5 *)
  { op = Op.add; src1 = Config.y1; src2 = Config.z1; dst = Config.t5 };       (* 13: t5 <- Y1 + Z1 *)
  { op = Op.add; src1 = Config.y2; src2 = Config.z2; dst = Config.x3 };       (* 14: X3 <- Y2 + Z2 *)
  { op = Op.mul; src1 = Config.t5; src2 = Config.x3; dst = Config.t5 };       (* 15: t5 <- t5 * X3 *)
  { op = Op.add; src1 = Config.t1; src2 = Config.t2; dst = Config.x3 };       (* 16: X3 <- t1 + t2 *)
  { op = Op.sub; src1 = Config.t5; src2 = Config.x3; dst = Config.t5 };       (* 17: t5 <- t5 - X3 *)
  { op = Op.mul; src1 = Config.param_a; src2 = Config.t4; dst = Config.z3 };  (* 18: Z3 <- a * t4 *)
  { op = Op.mul; src1 = Config.param_b3; src2 = Config.t2; dst = Config.x3 }; (* 19: X3 <- b3 * t2 *)
  { op = Op.add; src1 = Config.x3; src2 = Config.z3; dst = Config.z3 };       (* 20: Z3 <- X3 + Z3 *)
  { op = Op.sub; src1 = Config.t1; src2 = Config.z3; dst = Config.x3 };       (* 21: X3 <- t1 - Z3 *)
  { op = Op.add; src1 = Config.t1; src2 = Config.z3; dst = Config.z3 };       (* 22: Z3 <- t1 + Z3 *)
  { op = Op.mul; src1 = Config.x3; src2 = Config.z3; dst = Config.y3 };       (* 23: Y3 <- X3 * Z3 *)
  { op = Op.add; src1 = Config.t0; src2 = Config.t0; dst = Config.t1 };       (* 24: t1 <- t0 + t0 *)
  { op = Op.add; src1 = Config.t1; src2 = Config.t0; dst = Config.t1 };       (* 25: t1 <- t1 + t0 *)
  { op = Op.mul; src1 = Config.param_a; src2 = Config.t2; dst = Config.t2 };  (* 26: t2 <- a * t2 *)
  { op = Op.mul; src1 = Config.param_b3; src2 = Config.t4; dst = Config.t4 }; (* 27: t4 <- b3 * t4 *)
  { op = Op.add; src1 = Config.t1; src2 = Config.t2; dst = Config.t1 };       (* 28: t1 <- t1 + t2 *)
  { op = Op.sub; src1 = Config.t0; src2 = Config.t2; dst = Config.t2 };       (* 29: t2 <- t0 - t2 *)
  { op = Op.mul; src1 = Config.param_a; src2 = Config.t2; dst = Config.t2 };  (* 30: t2 <- a * t2 *)
  { op = Op.add; src1 = Config.t4; src2 = Config.t2; dst = Config.t4 };       (* 31: t4 <- t4 + t2 *)
  { op = Op.mul; src1 = Config.t1; src2 = Config.t4; dst = Config.t0 };       (* 32: t0 <- t1 * t4 *)
  { op = Op.add; src1 = Config.y3; src2 = Config.t0; dst = Config.y3 };       (* 33: Y3 <- Y3 + t0 *)
  { op = Op.mul; src1 = Config.t5; src2 = Config.t4; dst = Config.t0 };       (* 34: t0 <- t5 * t4 *)
  { op = Op.mul; src1 = Config.t3; src2 = Config.x3; dst = Config.x3 };       (* 35: X3 <- t3 * X3 *)
  { op = Op.sub; src1 = Config.x3; src2 = Config.t0; dst = Config.x3 };       (* 36: X3 <- X3 - t0 *)
  { op = Op.mul; src1 = Config.t3; src2 = Config.t1; dst = Config.t0 };       (* 37: t0 <- t3 * t1 *)
  { op = Op.mul; src1 = Config.t5; src2 = Config.z3; dst = Config.z3 };       (* 38: Z3 <- t5 * Z3 *)
  { op = Op.add; src1 = Config.z3; src2 = Config.t0; dst = Config.z3 };       (* 39: Z3 <- Z3 + t0 *)
|]

let create scope (i : _ I.t) =
  let open Always in
  let ( -- ) = Scope.naming scope in
  let width = Config.width in
  let addr_width = Config.reg_addr_width in
  
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in
  
  (* Internal register file *)
  let reg_file = Array.init 17 ~f:(fun _ -> Variable.reg spec ~width) in
  
  (* Step counter *)
  let step = Variable.reg spec ~width:6 in
  
  (* Flag: has operation been started this step? *)
  let op_started = Variable.reg spec ~width:1 in
  
  (* Output registers *)
  let out_x3 = Variable.reg spec ~width in
  let out_y3 = Variable.reg spec ~width in
  let out_z3 = Variable.reg spec ~width in
  let done_flag = Variable.reg spec ~width:1 in
  
  (* Build instruction decode ROM *)
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
  
  (* Arith interface - directly from decode *)
  let arith_start = Variable.wire ~default:gnd in
  
  let arith_read_data_a = 
    mux current_src1 (Array.to_list (Array.map reg_file ~f:(fun r -> r.value)))
  in
  let arith_read_data_b =
    mux current_src2 (Array.to_list (Array.map reg_file ~f:(fun r -> r.value)))
  in
  
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
          step <-- zero 6;
          op_started <-- gnd;
          sm.set_next Load_inputs;
        ];
      ];
      
State.Load_inputs, [
  reg_file.(Config.x1) <-- i.x1;
  reg_file.(Config.y1) <-- i.y1;
  reg_file.(Config.z1) <-- i.z1;
  reg_file.(Config.x2) <-- i.x2;
  reg_file.(Config.y2) <-- i.y2;
  reg_file.(Config.z2) <-- i.z2;
  reg_file.(Config.param_a) <-- i.param_a;
  reg_file.(Config.param_b3) <-- i.param_b3;
  op_started <-- gnd;
  sm.set_next Wait_load;  (* Changed from Run_step *)
];

State.Wait_load, [
  (* Wait one cycle for register writes to take effect *)
  sm.set_next Run_step;
];
      
State.Run_step, [
  if_ (~:(op_started.value)) [
    (* First cycle of step: start operation *)
    arith_start <-- vdd;
    op_started <-- vdd;
  ] [
    (* Wait for operation to complete *)
    when_ arith_out.done_ [
      (* Write result to destination register *)
      proc (Array.to_list (Array.mapi reg_file ~f:(fun idx reg ->
        when_ (current_dst ==:. idx) [
          reg <-- arith_out.reg_write_data;
        ])));
      
      (* Move to next step or finish *)
      if_ (step.value ==:. Config.num_steps - 1) [
        sm.set_next Output;
      ] [
        step <-- step.value +:. 1;
        op_started <-- gnd;
      ];
    ];
  ];
];
      
      State.Output, [
        out_x3 <-- reg_file.(Config.x3).value;
        out_y3 <-- reg_file.(Config.y3).value;
        out_z3 <-- reg_file.(Config.z3).value;
        sm.set_next Done;
      ];
      
      State.Done, [
        done_flag <-- vdd;
        sm.set_next Idle;
      ];
    ];
  ];
  
  { O.
    busy = ~:(sm.is Idle) -- "busy"
  ; done_ = done_flag.value -- "done"
  ; x3 = out_x3.value -- "x3"
  ; y3 = out_y3.value -- "y3"
  ; z3 = out_z3.value -- "z3"
  }