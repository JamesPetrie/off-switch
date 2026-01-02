open Base
open Hardcaml
open Signal

module Config = struct
  let width = 256
  let num_registers = 32
  let reg_addr_width = 5
  
  let prime_p_z = Z.of_string 
    "115792089237316195423570985008687907853269984665640564039457584007908834671663"
  let order_n_z = Z.of_string
    "115792089237316195423570985008687907852837564279074904382605163141518161494337"
  
  let g_x_z = Z.of_string
    "55066263022277343669578718895168534326250603453777594175500187360389116729240"
  let g_y_z = Z.of_string
    "32670510020758816978083085130507043184471273380659243275938904335757337482424"
  
  let z_to_constant z =
    let hex_str = Z.format "%x" z in
    let padded = String.make ((width / 4) - String.length hex_str) '0' ^ hex_str in
    Constant.of_hex_string ~width ~signedness:Unsigned padded
    
  let prime_p () = Signal.of_constant (z_to_constant prime_p_z)
  let order_n () = Signal.of_constant (z_to_constant order_n_z)
  let g_x () = Signal.of_constant (z_to_constant g_x_z)
  let g_y () = Signal.of_constant (z_to_constant g_y_z)
end

module Reg = struct
  let e = 0
  let r = 1
  let s = 2
  let q_x = 3
  let q_y = 4
  let qpg_x = 5
  let qpg_y = 6
  let g_x = 7
  let g_y = 8
  let zero = 9
  let one = 10
  let two = 11
  let three = 12
  let w = 13
  let u1 = 14
  let u2 = 15
  let acc_x = 16
  let acc_y = 17
  let tmp1 = 18
  let tmp2 = 19
  let tmp3 = 20
  let tmp4 = 21
  let tmp5 = 22
  let lambda = 23
  let lambda_sq = 24
  let p_x = 25
  let p_y = 26
  let add_x = 27
  let add_y = 28
end

module ArithOp = struct
  let add = 0
  let sub = 1
  let mul = 2
  let inv = 3
end

module RegisterFile = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_enable : 'a
      ; write_addr : 'a [@bits Config.reg_addr_width]
      ; write_data : 'a [@bits Config.width]
      ; read_addr_a : 'a [@bits Config.reg_addr_width]
      ; read_addr_b : 'a [@bits Config.reg_addr_width]
      ; load_enable : 'a
      ; load_addr : 'a [@bits Config.reg_addr_width]
      ; load_data : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { read_data_a : 'a [@bits Config.width]
      ; read_data_b : 'a [@bits Config.width]
      ; reg_u1 : 'a [@bits Config.width]
      ; reg_u2 : 'a [@bits Config.width]
      ; reg_acc_x : 'a [@bits Config.width]
      ; reg_acc_y : 'a [@bits Config.width]
      ; reg_r : 'a [@bits Config.width]
      ; reg_s : 'a [@bits Config.width]
      ; reg_tmp1 : 'a [@bits Config.width]
      ; reg_tmp2 : 'a [@bits Config.width]
      ; reg_tmp3 : 'a [@bits Config.width]
      ; reg_tmp4 : 'a [@bits Config.width]
      ; reg_tmp5 : 'a [@bits Config.width]
      ; reg_lambda : 'a [@bits Config.width]
      ; reg_p_x : 'a [@bits Config.width]
      ; reg_p_y : 'a [@bits Config.width]
      ; reg_add_x : 'a [@bits Config.width]
      ; reg_add_y : 'a [@bits Config.width]
      ; reg_q_x : 'a [@bits Config.width]
      ; reg_q_y : 'a [@bits Config.width]
      ; reg_qpg_x : 'a [@bits Config.width]
      ; reg_qpg_y : 'a [@bits Config.width]
      ; reg_g_x : 'a [@bits Config.width]
      ; reg_g_y : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    
    let reset_value idx =
      if idx = Reg.zero then zero Config.width
      else if idx = Reg.one then one Config.width
      else if idx = Reg.two then of_int ~width:Config.width 2
      else if idx = Reg.three then of_int ~width:Config.width 3
      else if idx = Reg.g_x then Config.g_x ()
      else if idx = Reg.g_y then Config.g_y ()
      else zero Config.width
    in
    
    let eff_write_enable = i.write_enable |: i.load_enable in
    let eff_write_addr = mux2 i.write_enable i.write_addr i.load_addr in
    let eff_write_data = mux2 i.write_enable i.write_data i.load_data in
    
    let registers = Array.init Config.num_registers ~f:(fun idx ->
      let r = Always.Variable.reg spec ~width:Config.width in
      let init_val = reset_value idx in
      Always.(compile [
        if_ i.clear [
          r <-- init_val;
        ] [
          when_ (eff_write_enable &: (eff_write_addr ==:. idx)) [
            r <-- eff_write_data;
          ];
        ];
      ]);
      r.value
    ) in
    
    let read_data_a = mux i.read_addr_a (Array.to_list registers) in
    let read_data_b = mux i.read_addr_b (Array.to_list registers) in
    
    { O.
      read_data_a = read_data_a -- "read_data_a"
    ; read_data_b = read_data_b -- "read_data_b"
    ; reg_u1 = registers.(Reg.u1) -- "reg_u1"
    ; reg_u2 = registers.(Reg.u2) -- "reg_u2"
    ; reg_acc_x = registers.(Reg.acc_x) -- "reg_acc_x"
    ; reg_acc_y = registers.(Reg.acc_y) -- "reg_acc_y"
    ; reg_r = registers.(Reg.r) -- "reg_r"
    ; reg_s = registers.(Reg.s) -- "reg_s"
    ; reg_tmp1 = registers.(Reg.tmp1) -- "reg_tmp1"
    ; reg_tmp2 = registers.(Reg.tmp2) -- "reg_tmp2"
    ; reg_tmp3 = registers.(Reg.tmp3) -- "reg_tmp3"
    ; reg_tmp4 = registers.(Reg.tmp4) -- "reg_tmp4"
    ; reg_tmp5 = registers.(Reg.tmp5) -- "reg_tmp5"
    ; reg_lambda = registers.(Reg.lambda) -- "reg_lambda"
    ; reg_p_x = registers.(Reg.p_x) -- "reg_p_x"
    ; reg_p_y = registers.(Reg.p_y) -- "reg_p_y"
    ; reg_add_x = registers.(Reg.add_x) -- "reg_add_x"
    ; reg_add_y = registers.(Reg.add_y) -- "reg_add_y"
    ; reg_q_x = registers.(Reg.q_x) -- "reg_q_x"
    ; reg_q_y = registers.(Reg.q_y) -- "reg_q_y"
    ; reg_qpg_x = registers.(Reg.qpg_x) -- "reg_qpg_x"
    ; reg_qpg_y = registers.(Reg.qpg_y) -- "reg_qpg_y"
    ; reg_g_x = registers.(Reg.g_x) -- "reg_g_x"
    ; reg_g_y = registers.(Reg.g_y) -- "reg_g_y"
    }
end

module ArithUnit = struct
  module State = struct
    type t =
      | Idle
      | Load
      | Capture
      | Compute
      | Write
      | Ready
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; op : 'a [@bits 2]
      ; prime_sel : 'a
      ; addr_a : 'a [@bits Config.reg_addr_width]
      ; addr_b : 'a [@bits Config.reg_addr_width]
      ; addr_out : 'a [@bits Config.reg_addr_width]
      ; reg_read_data_a : 'a [@bits Config.width]
      ; reg_read_data_b : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { busy : 'a
      ; done_ : 'a
      ; reg_write_enable : 'a
      ; reg_write_addr : 'a [@bits Config.reg_addr_width]
      ; reg_write_data : 'a [@bits Config.width]
      ; reg_read_addr_a : 'a [@bits Config.reg_addr_width]
      ; reg_read_addr_b : 'a [@bits Config.reg_addr_width]
      ; inv_exists : 'a
      ; dbg_state : 'a [@bits 3]
      ; dbg_operand_a : 'a [@bits Config.width]
      ; dbg_operand_b : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    let op_reg = Variable.reg spec ~width:2 in
    let prime_sel_reg = Variable.reg spec ~width:1 in
    let addr_a_reg = Variable.reg spec ~width:Config.reg_addr_width in
    let addr_b_reg = Variable.reg spec ~width:Config.reg_addr_width in
    let addr_out_reg = Variable.reg spec ~width:Config.reg_addr_width in
    
    (* Separate register for write address - captured at start, stable through write *)
    let write_addr_reg = Variable.reg spec ~width:Config.reg_addr_width in
    
    let operand_a = Variable.reg spec ~width:Config.width in
    let operand_b = Variable.reg spec ~width:Config.width in
    
    let start_add = Variable.reg spec ~width:1 in
    let start_sub = Variable.reg spec ~width:1 in
    let start_mul = Variable.reg spec ~width:1 in
    let start_inv = Variable.reg spec ~width:1 in
    
    let result_reg = Variable.reg spec ~width:Config.width in
    let inv_exists_reg = Variable.reg spec ~width:1 in
    
    let reg_write_enable = Variable.wire ~default:gnd in
    let done_flag = Variable.reg spec ~width:1 in
    
    let selected_prime = mux2 prime_sel_reg.value (Config.order_n ()) (Config.prime_p ()) in
    
    let mod_add_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_add")
      { Mod_add.ModAdd.I.
        clock = i.clock
      ; clear = i.clear
      ; start = start_add.value
      ; a = operand_a.value
      ; b = operand_b.value
      ; modulus = selected_prime
      ; subtract = gnd
      }
    in
    
    let mod_sub_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_sub")
      { Mod_add.ModAdd.I.
        clock = i.clock
      ; clear = i.clear
      ; start = start_sub.value
      ; a = operand_a.value
      ; b = operand_b.value
      ; modulus = selected_prime
      ; subtract = vdd
      }
    in
    
    let mod_mul_out = Mod_mul.ModMul.create (Scope.sub_scope scope "mod_mul")
      { Mod_mul.ModMul.I.
        clock = i.clock
      ; clear = i.clear
      ; start = start_mul.value
      ; x = operand_a.value
      ; y = operand_b.value
      ; modulus = selected_prime
      ; num_bits = of_int ~width:9 Config.width
      }
    in
    
    let mod_inv_out = Mod_inv.ModInv.create (Scope.sub_scope scope "mod_inv")
      { Mod_inv.ModInv.I.
        clock = i.clock
      ; clear = i.clear
      ; start = start_inv.value
      ; x = operand_a.value
      ; modulus = selected_prime
      }
    in
    
    let op_result = 
      mux op_reg.value [
        mod_add_out.result;
        mod_sub_out.result;
        mod_mul_out.result;
        mod_inv_out.result;
      ]
    in
    
    let op_valid =
      mux op_reg.value [
        mod_add_out.valid;
        mod_sub_out.valid;
        mod_mul_out.valid;
        mod_inv_out.valid;
      ]
    in
    
    let state_bits = mux (sm.current) [
      of_int ~width:3 0;  (* Idle *)
      of_int ~width:3 1;  (* Load *)
      of_int ~width:3 2;  (* Capture *)
      of_int ~width:3 3;  (* Compute *)
      of_int ~width:3 4;  (* Write *)
      of_int ~width:3 5;  (* Ready *)
    ] in
    
    compile [
      start_add <-- gnd;
      start_sub <-- gnd;
      start_mul <-- gnd;
      start_inv <-- gnd;
      reg_write_enable <-- gnd;
      done_flag <-- gnd;
      
      sm.switch [
        State.Idle, [
          when_ i.start [
            op_reg <-- i.op;
            prime_sel_reg <-- i.prime_sel;
            addr_a_reg <-- i.addr_a;
            addr_b_reg <-- i.addr_b;
            addr_out_reg <-- i.addr_out;
            (* Capture write address separately - this won't change until next operation *)
            write_addr_reg <-- i.addr_out;
            sm.set_next Load;
          ];
        ];
        
        State.Load, [
          sm.set_next Capture;
        ];
        
        State.Capture, [
          operand_a <-- i.reg_read_data_a;
          operand_b <-- i.reg_read_data_b;
          
          switch op_reg.value [
            of_int ~width:2 ArithOp.add, [ start_add <-- vdd ];
            of_int ~width:2 ArithOp.sub, [ start_sub <-- vdd ];
            of_int ~width:2 ArithOp.mul, [ start_mul <-- vdd ];
            of_int ~width:2 ArithOp.inv, [ start_inv <-- vdd ];
          ];
          
          sm.set_next Compute;
        ];
        
        State.Compute, [
          when_ op_valid [
            result_reg <-- op_result;
            inv_exists_reg <-- mod_inv_out.exists;
            sm.set_next Write;
          ];
        ];
        
        State.Write, [
          reg_write_enable <-- vdd;
          sm.set_next Ready;
        ];
        
        State.Ready, [
          done_flag <-- vdd;
          when_ i.start [
            op_reg <-- i.op;
            prime_sel_reg <-- i.prime_sel;
            addr_a_reg <-- i.addr_a;
            addr_b_reg <-- i.addr_b;
            addr_out_reg <-- i.addr_out;
            (* Capture new write address for the new operation *)
            write_addr_reg <-- i.addr_out;
            sm.set_next Load;
          ];
          when_ (~:(i.start)) [
            sm.set_next Idle;
          ];
        ];
      ];
    ];
    
    let is_idle_or_ready = (sm.is Idle) |: (sm.is Ready) in
    
    { O.
      busy = ~:is_idle_or_ready -- "busy"
    ; done_ = done_flag.value -- "done"
    ; reg_write_enable = reg_write_enable.value -- "reg_write_enable"
    (* Use the stable write_addr_reg instead of addr_out_reg *)
    ; reg_write_addr = write_addr_reg.value -- "reg_write_addr"
    ; reg_write_data = result_reg.value -- "reg_write_data"
    ; reg_read_addr_a = addr_a_reg.value -- "reg_read_addr_a"
    ; reg_read_addr_b = addr_b_reg.value -- "reg_read_addr_b"
    ; inv_exists = inv_exists_reg.value -- "inv_exists"
    ; dbg_state = state_bits -- "dbg_state"
    ; dbg_operand_a = operand_a.value -- "dbg_operand_a"
    ; dbg_operand_b = operand_b.value -- "dbg_operand_b"
    }
end
module EcdsaController = struct
  module State = struct
    type t =
      | Idle
      | Load_inputs
      | Init_constants
      | Validate_r_nonzero
      | Validate_r_less_than_n
      | Validate_s_nonzero
      | Validate_s_less_than_n
      | Validation_failed
      | Compute_w
      | Wait_w
      | Check_w_exists
      | Compute_u1
      | Wait_u1
      | Compute_u2
      | Wait_u2
      | Loop_init
      | Loop_double_start
      | Pdbl_save_px
      | Pdbl_save_px_wait
      | Pdbl_save_py
      | Pdbl_save_py_wait
      | Pdbl_xsq
      | Pdbl_xsq_wait
      | Pdbl_3xsq
      | Pdbl_3xsq_wait
      | Pdbl_2y
      | Pdbl_2y_wait
      | Pdbl_inv
      | Pdbl_inv_wait
      | Pdbl_inv_check
      | Pdbl_lambda
      | Pdbl_lambda_wait
      | Pdbl_lsq
      | Pdbl_lsq_wait
      | Pdbl_2x
      | Pdbl_2x_wait
      | Pdbl_xr
      | Pdbl_xr_wait
      | Pdbl_diff
      | Pdbl_diff_wait
      | Pdbl_ldiff
      | Pdbl_ldiff_wait
      | Pdbl_yr
      | Pdbl_yr_wait
      | Pdbl_copy_x
      | Pdbl_copy_x_wait
      | Pdbl_copy_y
      | Pdbl_copy_y_wait
      | Loop_check_bits
      | Load_add_g_x
      | Load_add_g_x_wait
      | Load_add_g_y
      | Load_add_g_y_wait
      | Load_add_q_x
      | Load_add_q_x_wait
      | Load_add_q_y
      | Load_add_q_y_wait
      | Load_add_qpg_x
      | Load_add_qpg_x_wait
      | Load_add_qpg_y
      | Load_add_qpg_y_wait
      | Copy_to_acc_x
      | Copy_to_acc_x_wait
      | Copy_to_acc_y
      | Copy_to_acc_y_wait
      | Padd_save_px
      | Padd_save_px_wait
      | Padd_save_py
      | Padd_save_py_wait
      | Padd_num
      | Padd_num_wait
      | Padd_denom
      | Padd_denom_wait
      | Padd_inv
      | Padd_inv_wait
      | Padd_inv_check
      | Padd_lambda
      | Padd_lambda_wait
      | Padd_lsq
      | Padd_lsq_wait
      | Padd_xr1
      | Padd_xr1_wait
      | Padd_xr
      | Padd_xr_wait
      | Padd_diff
      | Padd_diff_wait
      | Padd_ldiff
      | Padd_ldiff_wait
      | Padd_yr
      | Padd_yr_wait
      | Loop_next
      | Final_check
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; arith_done : 'a
      ; arith_inv_exists : 'a
      ; u1_value : 'a [@bits Config.width]
      ; u2_value : 'a [@bits Config.width]
      ; acc_x_value : 'a [@bits Config.width]
      ; acc_y_value : 'a [@bits Config.width]
      ; r_value : 'a [@bits Config.width]
      ; s_value : 'a [@bits Config.width]
      ; input_e : 'a [@bits Config.width]
      ; input_r : 'a [@bits Config.width]
      ; input_s : 'a [@bits Config.width]
      ; input_q_x : 'a [@bits Config.width]
      ; input_q_y : 'a [@bits Config.width]
      ; input_qpg_x : 'a [@bits Config.width]
      ; input_qpg_y : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { busy : 'a
      ; done_ : 'a
      ; valid_signature : 'a
      ; error_invalid_r : 'a
      ; error_invalid_s : 'a
      ; arith_start : 'a
      ; arith_op : 'a [@bits 2]
      ; arith_prime_sel : 'a
      ; arith_addr_a : 'a [@bits Config.reg_addr_width]
      ; arith_addr_b : 'a [@bits Config.reg_addr_width]
      ; arith_addr_out : 'a [@bits Config.reg_addr_width]
      ; load_enable : 'a
      ; load_addr : 'a [@bits Config.reg_addr_width]
      ; load_data : 'a [@bits Config.width]
      ; dbg_state : 'a [@bits 7]
      ; dbg_bit_idx : 'a [@bits 9]
      ; dbg_acc_is_infinity : 'a
      ; dbg_double_count : 'a [@bits 10]
      ; dbg_add_count : 'a [@bits 10]
      ; dbg_copy_count : 'a [@bits 10]
      ; dbg_selected_point : 'a [@bits 2]
      ; dbg_u1_bit : 'a
      ; dbg_u2_bit : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    let bit_idx = Variable.reg spec ~width:9 in
    let acc_is_infinity = Variable.reg spec ~width:1 in
    let load_step = Variable.reg spec ~width:4 in
    
    let error_invalid_r = Variable.reg spec ~width:1 in
    let error_invalid_s = Variable.reg spec ~width:1 in
    
    let double_count = Variable.reg spec ~width:10 in
    let add_count = Variable.reg spec ~width:10 in
    let copy_count = Variable.reg spec ~width:10 in
    
    let selected_point = Variable.reg spec ~width:2 in
    
    let arith_start = Variable.wire ~default:gnd in
    let arith_op = Variable.wire ~default:(zero 2) in
    let arith_prime_sel = Variable.wire ~default:gnd in
    let arith_addr_a = Variable.wire ~default:(zero Config.reg_addr_width) in
    let arith_addr_b = Variable.wire ~default:(zero Config.reg_addr_width) in
    let arith_addr_out = Variable.wire ~default:(zero Config.reg_addr_width) in
    
    let load_enable = Variable.wire ~default:gnd in
    let load_addr = Variable.wire ~default:(zero Config.reg_addr_width) in
    let load_data = Variable.wire ~default:(zero Config.width) in
    
    let valid_sig = Variable.reg spec ~width:1 in
    let done_reg = Variable.reg spec ~width:1 in
    
    let start_arith op prime_sel addr_a addr_b addr_out =
      [ arith_start <-- vdd
      ; arith_op <-- of_int ~width:2 op
      ; arith_prime_sel <-- (if prime_sel then vdd else gnd)
      ; arith_addr_a <-- of_int ~width:Config.reg_addr_width addr_a
      ; arith_addr_b <-- of_int ~width:Config.reg_addr_width addr_b
      ; arith_addr_out <-- of_int ~width:Config.reg_addr_width addr_out
      ]
    in
    
    let copy_reg src dst =
      start_arith ArithOp.add false src Reg.zero dst
    in
    
    let load_const addr data =
      [ load_enable <-- vdd
      ; load_addr <-- of_int ~width:Config.reg_addr_width addr
      ; load_data <-- data
      ]
    in
    
    let get_bit value idx =
      let bits = Array.init Config.width ~f:(fun i -> bit value i) in
      mux idx (Array.to_list bits)
    in
    let u1_bit = get_bit i.u1_value bit_idx.value in
    let u2_bit = get_bit i.u2_value bit_idx.value in
    
    let order_n = Config.order_n () in
    
    let state_encoding = List.mapi State.all ~f:(fun idx _ -> 
      of_int ~width:7 idx
    ) in
    let state_bits = mux sm.current state_encoding in
    
    compile [
      done_reg <-- gnd;
      
      sm.switch [
        State.Idle, [
          valid_sig <-- gnd;
          error_invalid_r <-- gnd;
          error_invalid_s <-- gnd;
          double_count <-- zero 10;
          add_count <-- zero 10;
          copy_count <-- zero 10;
          selected_point <-- zero 2;
          when_ i.start [
            load_step <-- zero 4;
            sm.set_next Load_inputs;
          ];
        ];
        
        State.Load_inputs, [
          switch load_step.value [
            of_int ~width:4 0, load_const Reg.e i.input_e @
              [ load_step <-- of_int ~width:4 1 ];
            of_int ~width:4 1, load_const Reg.r i.input_r @
              [ load_step <-- of_int ~width:4 2 ];
            of_int ~width:4 2, load_const Reg.s i.input_s @
              [ load_step <-- of_int ~width:4 3 ];
            of_int ~width:4 3, load_const Reg.q_x i.input_q_x @
              [ load_step <-- of_int ~width:4 4 ];
            of_int ~width:4 4, load_const Reg.q_y i.input_q_y @
              [ load_step <-- of_int ~width:4 5 ];
            of_int ~width:4 5, load_const Reg.qpg_x i.input_qpg_x @
              [ load_step <-- of_int ~width:4 6 ];
            of_int ~width:4 6, load_const Reg.qpg_y i.input_qpg_y @
              [ load_step <-- zero 4; sm.set_next Init_constants ];
          ];
        ];
        
        State.Init_constants, [
          switch load_step.value [
            of_int ~width:4 0, load_const Reg.g_x (Config.g_x ()) @
              [ load_step <-- of_int ~width:4 1 ];
            of_int ~width:4 1, load_const Reg.g_y (Config.g_y ()) @
              [ load_step <-- of_int ~width:4 2 ];
            of_int ~width:4 2, load_const Reg.zero (zero Config.width) @
              [ load_step <-- of_int ~width:4 3 ];
            of_int ~width:4 3, load_const Reg.one (of_int ~width:Config.width 1) @
              [ load_step <-- of_int ~width:4 4 ];
            of_int ~width:4 4, load_const Reg.two (of_int ~width:Config.width 2) @
              [ load_step <-- of_int ~width:4 5 ];
            of_int ~width:4 5, load_const Reg.three (of_int ~width:Config.width 3) @
              [ sm.set_next Validate_r_nonzero ];
          ];
        ];
        
        State.Validate_r_nonzero, [
          if_ (i.r_value ==: zero Config.width) [
            error_invalid_r <-- vdd;
            sm.set_next Validation_failed;
          ] [
            sm.set_next Validate_r_less_than_n;
          ];
        ];
        
        State.Validate_r_less_than_n, [
          if_ (i.r_value >=: order_n) [
            error_invalid_r <-- vdd;
            sm.set_next Validation_failed;
          ] [
            sm.set_next Validate_s_nonzero;
          ];
        ];
        
        State.Validate_s_nonzero, [
          if_ (i.s_value ==: zero Config.width) [
            error_invalid_s <-- vdd;
            sm.set_next Validation_failed;
          ] [
            sm.set_next Validate_s_less_than_n;
          ];
        ];
        
        State.Validate_s_less_than_n, [
          if_ (i.s_value >=: order_n) [
            error_invalid_s <-- vdd;
            sm.set_next Validation_failed;
          ] [
            sm.set_next Compute_w;
          ];
        ];
        
        State.Validation_failed, [
          valid_sig <-- gnd;
          done_reg <-- vdd;
          sm.set_next Done;
        ];
        
        State.Compute_w,
          start_arith ArithOp.inv true Reg.s Reg.zero Reg.w @
          [ sm.set_next Wait_w ];
        
        State.Wait_w, [
          when_ i.arith_done [ sm.set_next Check_w_exists ];
        ];
        
        State.Check_w_exists, [
          if_ (~:(i.arith_inv_exists)) [
            error_invalid_s <-- vdd;
            sm.set_next Validation_failed;
          ] [
            sm.set_next Compute_u1;
          ];
        ];
        
        State.Compute_u1,
          start_arith ArithOp.mul true Reg.e Reg.w Reg.u1 @
          [ sm.set_next Wait_u1 ];
        
        State.Wait_u1, [
          when_ i.arith_done [ sm.set_next Compute_u2 ];
        ];
        
        State.Compute_u2,
          start_arith ArithOp.mul true Reg.r Reg.w Reg.u2 @
          [ sm.set_next Wait_u2 ];
        
        State.Wait_u2, [
          when_ i.arith_done [ sm.set_next Loop_init ];
        ];
        
        State.Loop_init, [
          bit_idx <-- of_int ~width:9 255;
          acc_is_infinity <-- vdd;
          sm.set_next Loop_double_start;
        ];
        
        State.Loop_double_start, [
          if_ acc_is_infinity.value [
            sm.set_next Loop_check_bits;
          ] [
            double_count <-- double_count.value +:. 1;
            sm.set_next Pdbl_save_px;
          ];
        ];
        
        State.Pdbl_save_px,
          copy_reg Reg.acc_x Reg.p_x @
          [ sm.set_next Pdbl_save_px_wait ];
        
        State.Pdbl_save_px_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_save_py ];
        ];
        
        State.Pdbl_save_py,
          copy_reg Reg.acc_y Reg.p_y @
          [ sm.set_next Pdbl_save_py_wait ];
        
        State.Pdbl_save_py_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_xsq ];
        ];
        
        State.Pdbl_xsq,
          start_arith ArithOp.mul false Reg.p_x Reg.p_x Reg.tmp1 @
          [ sm.set_next Pdbl_xsq_wait ];
        
        State.Pdbl_xsq_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_3xsq ];
        ];
        
        State.Pdbl_3xsq,
          start_arith ArithOp.mul false Reg.tmp1 Reg.three Reg.tmp2 @
          [ sm.set_next Pdbl_3xsq_wait ];
        
        State.Pdbl_3xsq_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_2y ];
        ];
        
        State.Pdbl_2y,
          start_arith ArithOp.mul false Reg.p_y Reg.two Reg.tmp3 @
          [ sm.set_next Pdbl_2y_wait ];
        
        State.Pdbl_2y_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_inv ];
        ];
        
        State.Pdbl_inv,
          start_arith ArithOp.inv false Reg.tmp3 Reg.zero Reg.tmp4 @
          [ sm.set_next Pdbl_inv_wait ];
        
        State.Pdbl_inv_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_inv_check ];
        ];
        
        State.Pdbl_inv_check, [
          if_ (~:(i.arith_inv_exists)) [
            acc_is_infinity <-- vdd;
            sm.set_next Loop_check_bits;
          ] [
            sm.set_next Pdbl_lambda;
          ];
        ];
        
        State.Pdbl_lambda,
          start_arith ArithOp.mul false Reg.tmp2 Reg.tmp4 Reg.lambda @
          [ sm.set_next Pdbl_lambda_wait ];
        
        State.Pdbl_lambda_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_lsq ];
        ];
        
        State.Pdbl_lsq,
          start_arith ArithOp.mul false Reg.lambda Reg.lambda Reg.lambda_sq @
          [ sm.set_next Pdbl_lsq_wait ];
        
        State.Pdbl_lsq_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_2x ];
        ];
        
        State.Pdbl_2x,
          start_arith ArithOp.mul false Reg.p_x Reg.two Reg.tmp5 @
          [ sm.set_next Pdbl_2x_wait ];
        
        State.Pdbl_2x_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_xr ];
        ];
        
        State.Pdbl_xr,
          start_arith ArithOp.sub false Reg.lambda_sq Reg.tmp5 Reg.tmp1 @
          [ sm.set_next Pdbl_xr_wait ];
        
        State.Pdbl_xr_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_diff ];
        ];
        
        State.Pdbl_diff,
          start_arith ArithOp.sub false Reg.p_x Reg.tmp1 Reg.tmp2 @
          [ sm.set_next Pdbl_diff_wait ];
        
        State.Pdbl_diff_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_ldiff ];
        ];
        
        State.Pdbl_ldiff,
          start_arith ArithOp.mul false Reg.lambda Reg.tmp2 Reg.tmp3 @
          [ sm.set_next Pdbl_ldiff_wait ];
        
        State.Pdbl_ldiff_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_yr ];
        ];
        
        State.Pdbl_yr,
          start_arith ArithOp.sub false Reg.tmp3 Reg.p_y Reg.tmp4 @
          [ sm.set_next Pdbl_yr_wait ];
        
        State.Pdbl_yr_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_copy_x ];
        ];
        
        State.Pdbl_copy_x,
          copy_reg Reg.tmp1 Reg.acc_x @
          [ sm.set_next Pdbl_copy_x_wait ];
        
        State.Pdbl_copy_x_wait, [
          when_ i.arith_done [ sm.set_next Pdbl_copy_y ];
        ];
        
        State.Pdbl_copy_y,
          copy_reg Reg.tmp4 Reg.acc_y @
          [ sm.set_next Pdbl_copy_y_wait ];
        
        State.Pdbl_copy_y_wait, [
          when_ i.arith_done [ sm.set_next Loop_check_bits ];
        ];
        
        State.Loop_check_bits, [
          let bits = u2_bit @: u1_bit in
          if_ (bits ==:. 0b00) [
            selected_point <-- of_int ~width:2 0;
            sm.set_next Loop_next;
          ] @@ elif (bits ==:. 0b01) [
            selected_point <-- of_int ~width:2 1;
            sm.set_next Load_add_g_x;
          ] @@ elif (bits ==:. 0b10) [
            selected_point <-- of_int ~width:2 2;
            sm.set_next Load_add_q_x;
          ] [
            selected_point <-- of_int ~width:2 3;
            sm.set_next Load_add_qpg_x;
          ];
        ];
        
        State.Load_add_g_x,
          copy_reg Reg.g_x Reg.add_x @
          [ sm.set_next Load_add_g_x_wait ];
        
        State.Load_add_g_x_wait, [
          when_ i.arith_done [ sm.set_next Load_add_g_y ];
        ];
        
        State.Load_add_g_y,
          copy_reg Reg.g_y Reg.add_y @
          [ sm.set_next Load_add_g_y_wait ];
        
        State.Load_add_g_y_wait, [
          when_ i.arith_done [
            if_ acc_is_infinity.value [
              copy_count <-- copy_count.value +:. 1;
              sm.set_next Copy_to_acc_x;
            ] [
              add_count <-- add_count.value +:. 1;
              sm.set_next Padd_save_px;
            ];
          ];
        ];
        
        State.Load_add_q_x,
          copy_reg Reg.q_x Reg.add_x @
          [ sm.set_next Load_add_q_x_wait ];
        
        State.Load_add_q_x_wait, [
          when_ i.arith_done [ sm.set_next Load_add_q_y ];
        ];
        
        State.Load_add_q_y,
          copy_reg Reg.q_y Reg.add_y @
          [ sm.set_next Load_add_q_y_wait ];
        
        State.Load_add_q_y_wait, [
          when_ i.arith_done [
            if_ acc_is_infinity.value [
              copy_count <-- copy_count.value +:. 1;
              sm.set_next Copy_to_acc_x;
            ] [
              add_count <-- add_count.value +:. 1;
              sm.set_next Padd_save_px;
            ];
          ];
        ];
        
        State.Load_add_qpg_x,
          copy_reg Reg.qpg_x Reg.add_x @
          [ sm.set_next Load_add_qpg_x_wait ];
        
        State.Load_add_qpg_x_wait, [
          when_ i.arith_done [ sm.set_next Load_add_qpg_y ];
        ];
        
        State.Load_add_qpg_y,
          copy_reg Reg.qpg_y Reg.add_y @
          [ sm.set_next Load_add_qpg_y_wait ];
        
        State.Load_add_qpg_y_wait, [
          when_ i.arith_done [
            if_ acc_is_infinity.value [
              copy_count <-- copy_count.value +:. 1;
              sm.set_next Copy_to_acc_x;
            ] [
              add_count <-- add_count.value +:. 1;
              sm.set_next Padd_save_px;
            ];
          ];
        ];
        
        State.Copy_to_acc_x,
          copy_reg Reg.add_x Reg.acc_x @
          [ sm.set_next Copy_to_acc_x_wait ];
        
        State.Copy_to_acc_x_wait, [
          when_ i.arith_done [ sm.set_next Copy_to_acc_y ];
        ];
        
        State.Copy_to_acc_y,
          copy_reg Reg.add_y Reg.acc_y @
          [ sm.set_next Copy_to_acc_y_wait ];
        
        State.Copy_to_acc_y_wait, [
          when_ i.arith_done [
            acc_is_infinity <-- gnd;
            sm.set_next Loop_next;
          ];
        ];
        
        State.Padd_save_px,
          copy_reg Reg.acc_x Reg.p_x @
          [ sm.set_next Padd_save_px_wait ];
        
        State.Padd_save_px_wait, [
          when_ i.arith_done [ sm.set_next Padd_save_py ];
        ];
        
        State.Padd_save_py,
          copy_reg Reg.acc_y Reg.p_y @
          [ sm.set_next Padd_save_py_wait ];
        
        State.Padd_save_py_wait, [
          when_ i.arith_done [ sm.set_next Padd_num ];
        ];
        
        State.Padd_num,
          start_arith ArithOp.sub false Reg.add_y Reg.p_y Reg.tmp1 @
          [ sm.set_next Padd_num_wait ];
        
        State.Padd_num_wait, [
          when_ i.arith_done [ sm.set_next Padd_denom ];
        ];
        
        State.Padd_denom,
          start_arith ArithOp.sub false Reg.add_x Reg.p_x Reg.tmp2 @
          [ sm.set_next Padd_denom_wait ];
        
        State.Padd_denom_wait, [
          when_ i.arith_done [ sm.set_next Padd_inv ];
        ];
        
        State.Padd_inv,
          start_arith ArithOp.inv false Reg.tmp2 Reg.zero Reg.tmp3 @
          [ sm.set_next Padd_inv_wait ];
        
        State.Padd_inv_wait, [
          when_ i.arith_done [ sm.set_next Padd_inv_check ];
        ];
        
        State.Padd_inv_check, [
          if_ (~:(i.arith_inv_exists)) [
            acc_is_infinity <-- vdd;
            sm.set_next Loop_next;
          ] [
            sm.set_next Padd_lambda;
          ];
        ];
        
        State.Padd_lambda,
          start_arith ArithOp.mul false Reg.tmp1 Reg.tmp3 Reg.lambda @
          [ sm.set_next Padd_lambda_wait ];
        
        State.Padd_lambda_wait, [
          when_ i.arith_done [ sm.set_next Padd_lsq ];
        ];
        
        State.Padd_lsq,
          start_arith ArithOp.mul false Reg.lambda Reg.lambda Reg.lambda_sq @
          [ sm.set_next Padd_lsq_wait ];
        
        State.Padd_lsq_wait, [
          when_ i.arith_done [ sm.set_next Padd_xr1 ];
        ];
        
        State.Padd_xr1,
          start_arith ArithOp.sub false Reg.lambda_sq Reg.p_x Reg.tmp4 @
          [ sm.set_next Padd_xr1_wait ];
        
        State.Padd_xr1_wait, [
          when_ i.arith_done [ sm.set_next Padd_xr ];
        ];
        
        State.Padd_xr,
          start_arith ArithOp.sub false Reg.tmp4 Reg.add_x Reg.acc_x @
          [ sm.set_next Padd_xr_wait ];
        
        State.Padd_xr_wait, [
          when_ i.arith_done [ sm.set_next Padd_diff ];
        ];
        
        State.Padd_diff,
          start_arith ArithOp.sub false Reg.p_x Reg.acc_x Reg.tmp5 @
          [ sm.set_next Padd_diff_wait ];
        
        State.Padd_diff_wait, [
          when_ i.arith_done [ sm.set_next Padd_ldiff ];
        ];
        
        State.Padd_ldiff,
          start_arith ArithOp.mul false Reg.lambda Reg.tmp5 Reg.tmp4 @
          [ sm.set_next Padd_ldiff_wait ];
        
        State.Padd_ldiff_wait, [
          when_ i.arith_done [ sm.set_next Padd_yr ];
        ];
        
        State.Padd_yr,
          start_arith ArithOp.sub false Reg.tmp4 Reg.p_y Reg.acc_y @
          [ sm.set_next Padd_yr_wait ];
        
        State.Padd_yr_wait, [
          when_ i.arith_done [ sm.set_next Loop_next ];
        ];
        
        State.Loop_next, [
          if_ (bit_idx.value ==:. 0) [
            sm.set_next Final_check;
          ] [
            bit_idx <-- bit_idx.value -:. 1;
            sm.set_next Loop_double_start;
          ];
        ];
        
        State.Final_check, [
          if_ acc_is_infinity.value [
            valid_sig <-- gnd;
          ] [
            let x_eq_r = i.acc_x_value ==: i.r_value in
            let x_minus_n = i.acc_x_value -: order_n in
            let x_mod_n_eq_r = (i.acc_x_value >=: order_n) &: (x_minus_n ==: i.r_value) in
            valid_sig <-- (x_eq_r |: x_mod_n_eq_r);
          ];
          done_reg <-- vdd;
          sm.set_next Done;
        ];
        
        State.Done, [
          done_reg <-- vdd;
          when_ i.start [
            done_reg <-- gnd;
            valid_sig <-- gnd;
            error_invalid_r <-- gnd;
            error_invalid_s <-- gnd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];
    
    { O.
      busy = ~:(sm.is Idle) -- "busy"
    ; done_ = done_reg.value -- "done"
    ; valid_signature = valid_sig.value -- "valid_signature"
    ; error_invalid_r = error_invalid_r.value -- "error_invalid_r"
    ; error_invalid_s = error_invalid_s.value -- "error_invalid_s"
    ; arith_start = arith_start.value -- "arith_start"
    ; arith_op = arith_op.value -- "arith_op"
    ; arith_prime_sel = arith_prime_sel.value -- "arith_prime_sel"
    ; arith_addr_a = arith_addr_a.value -- "arith_addr_a"
    ; arith_addr_b = arith_addr_b.value -- "arith_addr_b"
    ; arith_addr_out = arith_addr_out.value -- "arith_addr_out"
    ; load_enable = load_enable.value -- "load_enable"
    ; load_addr = load_addr.value -- "load_addr"
    ; load_data = load_data.value -- "load_data"
    ; dbg_state = state_bits -- "dbg_state"
    ; dbg_bit_idx = bit_idx.value -- "dbg_bit_idx"
    ; dbg_acc_is_infinity = acc_is_infinity.value -- "dbg_acc_is_infinity"
    ; dbg_double_count = double_count.value -- "dbg_double_count"
    ; dbg_add_count = add_count.value -- "dbg_add_count"
    ; dbg_copy_count = copy_count.value -- "dbg_copy_count"
    ; dbg_selected_point = selected_point.value -- "dbg_selected_point"
    ; dbg_u1_bit = u1_bit -- "dbg_u1_bit"
    ; dbg_u2_bit = u2_bit -- "dbg_u2_bit"
    }
end

module EcdsaVerify = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; e : 'a [@bits Config.width]
      ; r : 'a [@bits Config.width]
      ; s : 'a [@bits Config.width]
      ; q_x : 'a [@bits Config.width]
      ; q_y : 'a [@bits Config.width]
      ; qplusg_x : 'a [@bits Config.width]
      ; qplusg_y : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { valid_signature : 'a
      ; done_ : 'a
      ; busy : 'a
      ; error_invalid_r : 'a
      ; error_invalid_s : 'a
      ; dbg_ctrl_state : 'a [@bits 7]
      ; dbg_arith_state : 'a [@bits 3]
      ; dbg_bit_idx : 'a [@bits 9]
      ; dbg_acc_is_infinity : 'a
      ; dbg_double_count : 'a [@bits 10]
      ; dbg_add_count : 'a [@bits 10]
      ; dbg_copy_count : 'a [@bits 10]
      ; dbg_selected_point : 'a [@bits 2]
      ; dbg_u1_bit : 'a
      ; dbg_u2_bit : 'a
      ; dbg_arith_start : 'a
      ; dbg_arith_done : 'a
      ; dbg_arith_op : 'a [@bits 2]
      ; dbg_arith_operand_a : 'a [@bits Config.width]
      ; dbg_arith_operand_b : 'a [@bits Config.width]
      ; dbg_load_enable : 'a
      ; dbg_load_addr : 'a [@bits Config.reg_addr_width]
      ; dbg_acc_x : 'a [@bits Config.width]
      ; dbg_acc_y : 'a [@bits Config.width]
      ; dbg_r : 'a [@bits Config.width]
      ; dbg_tmp1 : 'a [@bits Config.width]
      ; dbg_tmp2 : 'a [@bits Config.width]
      ; dbg_tmp3 : 'a [@bits Config.width]
      ; dbg_tmp4 : 'a [@bits Config.width]
      ; dbg_tmp5 : 'a [@bits Config.width]
      ; dbg_lambda : 'a [@bits Config.width]
      ; dbg_p_x : 'a [@bits Config.width]
      ; dbg_p_y : 'a [@bits Config.width]
      ; dbg_add_x : 'a [@bits Config.width]
      ; dbg_add_y : 'a [@bits Config.width]
      ; dbg_q_x : 'a [@bits Config.width]
      ; dbg_q_y : 'a [@bits Config.width]
      ; dbg_qpg_x : 'a [@bits Config.width]
      ; dbg_qpg_y : 'a [@bits Config.width]
      ; dbg_g_x : 'a [@bits Config.width]
      ; dbg_g_y : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    
    let arith_done_wire = wire 1 in
    let arith_inv_exists_wire = wire 1 in
    let reg_u1_wire = wire Config.width in
    let reg_u2_wire = wire Config.width in
    let reg_acc_x_wire = wire Config.width in
    let reg_acc_y_wire = wire Config.width in
    let reg_r_wire = wire Config.width in
    let reg_s_wire = wire Config.width in
    let reg_read_data_a_wire = wire Config.width in
    let reg_read_data_b_wire = wire Config.width in
    let reg_tmp1_wire = wire Config.width in
    let reg_tmp2_wire = wire Config.width in
    let reg_tmp3_wire = wire Config.width in
    let reg_tmp4_wire = wire Config.width in
    let reg_tmp5_wire = wire Config.width in
    let reg_lambda_wire = wire Config.width in
    let reg_p_x_wire = wire Config.width in
    let reg_p_y_wire = wire Config.width in
    let reg_add_x_wire = wire Config.width in
    let reg_add_y_wire = wire Config.width in
    let reg_q_x_wire = wire Config.width in
    let reg_q_y_wire = wire Config.width in
    let reg_qpg_x_wire = wire Config.width in
    let reg_qpg_y_wire = wire Config.width in
    let reg_g_x_wire = wire Config.width in
    let reg_g_y_wire = wire Config.width in
    
    let ctrl = EcdsaController.create (Scope.sub_scope scope "ctrl")
      { EcdsaController.I.
        clock = i.clock
      ; clear = i.clear
      ; start = i.start
      ; arith_done = arith_done_wire
      ; arith_inv_exists = arith_inv_exists_wire
      ; u1_value = reg_u1_wire
      ; u2_value = reg_u2_wire
      ; acc_x_value = reg_acc_x_wire
      ; acc_y_value = reg_acc_y_wire
      ; r_value = reg_r_wire
      ; s_value = reg_s_wire
      ; input_e = i.e
      ; input_r = i.r
      ; input_s = i.s
      ; input_q_x = i.q_x
      ; input_q_y = i.q_y
      ; input_qpg_x = i.qplusg_x
      ; input_qpg_y = i.qplusg_y
      }
    in
    
    let arith = ArithUnit.create (Scope.sub_scope scope "arith")
      { ArithUnit.I.
        clock = i.clock
      ; clear = i.clear
      ; start = ctrl.arith_start
      ; op = ctrl.arith_op
      ; prime_sel = ctrl.arith_prime_sel
      ; addr_a = ctrl.arith_addr_a
      ; addr_b = ctrl.arith_addr_b
      ; addr_out = ctrl.arith_addr_out
      ; reg_read_data_a = reg_read_data_a_wire
      ; reg_read_data_b = reg_read_data_b_wire
      }
    in
    
    let regfile = RegisterFile.create (Scope.sub_scope scope "regfile")
      { RegisterFile.I.
        clock = i.clock
      ; clear = i.clear
      ; write_enable = arith.reg_write_enable
      ; write_addr = arith.reg_write_addr
      ; write_data = arith.reg_write_data
      ; read_addr_a = arith.reg_read_addr_a
      ; read_addr_b = arith.reg_read_addr_b
      ; load_enable = ctrl.load_enable
      ; load_addr = ctrl.load_addr
      ; load_data = ctrl.load_data
      }
    in
    
    arith_done_wire <== arith.done_;
    arith_inv_exists_wire <== arith.inv_exists;
    reg_u1_wire <== regfile.reg_u1;
    reg_u2_wire <== regfile.reg_u2;
    reg_acc_x_wire <== regfile.reg_acc_x;
    reg_acc_y_wire <== regfile.reg_acc_y;
    reg_r_wire <== regfile.reg_r;
    reg_s_wire <== regfile.reg_s;
    reg_read_data_a_wire <== regfile.read_data_a;
    reg_read_data_b_wire <== regfile.read_data_b;
    reg_tmp1_wire <== regfile.reg_tmp1;
    reg_tmp2_wire <== regfile.reg_tmp2;
    reg_tmp3_wire <== regfile.reg_tmp3;
    reg_tmp4_wire <== regfile.reg_tmp4;
    reg_tmp5_wire <== regfile.reg_tmp5;
    reg_lambda_wire <== regfile.reg_lambda;
    reg_p_x_wire <== regfile.reg_p_x;
    reg_p_y_wire <== regfile.reg_p_y;
    reg_add_x_wire <== regfile.reg_add_x;
    reg_add_y_wire <== regfile.reg_add_y;
    reg_q_x_wire <== regfile.reg_q_x;
    reg_q_y_wire <== regfile.reg_q_y;
    reg_qpg_x_wire <== regfile.reg_qpg_x;
    reg_qpg_y_wire <== regfile.reg_qpg_y;
    reg_g_x_wire <== regfile.reg_g_x;
    reg_g_y_wire <== regfile.reg_g_y;
    
    { O.
      valid_signature = ctrl.valid_signature -- "valid_signature"
    ; done_ = ctrl.done_ -- "done"
    ; busy = ctrl.busy -- "busy"
    ; error_invalid_r = ctrl.error_invalid_r -- "error_invalid_r"
    ; error_invalid_s = ctrl.error_invalid_s -- "error_invalid_s"
    ; dbg_ctrl_state = ctrl.dbg_state -- "dbg_ctrl_state"
    ; dbg_arith_state = arith.dbg_state -- "dbg_arith_state"
    ; dbg_bit_idx = ctrl.dbg_bit_idx -- "dbg_bit_idx"
    ; dbg_acc_is_infinity = ctrl.dbg_acc_is_infinity -- "dbg_acc_is_infinity"
    ; dbg_double_count = ctrl.dbg_double_count -- "dbg_double_count"
    ; dbg_add_count = ctrl.dbg_add_count -- "dbg_add_count"
    ; dbg_copy_count = ctrl.dbg_copy_count -- "dbg_copy_count"
    ; dbg_selected_point = ctrl.dbg_selected_point -- "dbg_selected_point"
    ; dbg_u1_bit = ctrl.dbg_u1_bit -- "dbg_u1_bit"
    ; dbg_u2_bit = ctrl.dbg_u2_bit -- "dbg_u2_bit"
    ; dbg_arith_start = ctrl.arith_start -- "dbg_arith_start"
    ; dbg_arith_done = arith.done_ -- "dbg_arith_done"
    ; dbg_arith_op = ctrl.arith_op -- "dbg_arith_op"
    ; dbg_arith_operand_a = arith.dbg_operand_a -- "dbg_arith_operand_a"
    ; dbg_arith_operand_b = arith.dbg_operand_b -- "dbg_arith_operand_b"
    ; dbg_load_enable = ctrl.load_enable -- "dbg_load_enable"
    ; dbg_load_addr = ctrl.load_addr -- "dbg_load_addr"
    ; dbg_acc_x = regfile.reg_acc_x -- "dbg_acc_x"
    ; dbg_acc_y = regfile.reg_acc_y -- "dbg_acc_y"
    ; dbg_r = regfile.reg_r -- "dbg_r"
    ; dbg_tmp1 = regfile.reg_tmp1 -- "dbg_tmp1"
    ; dbg_tmp2 = regfile.reg_tmp2 -- "dbg_tmp2"
    ; dbg_tmp3 = regfile.reg_tmp3 -- "dbg_tmp3"
    ; dbg_tmp4 = regfile.reg_tmp4 -- "dbg_tmp4"
    ; dbg_tmp5 = regfile.reg_tmp5 -- "dbg_tmp5"
    ; dbg_lambda = regfile.reg_lambda -- "dbg_lambda"
    ; dbg_p_x = regfile.reg_p_x -- "dbg_p_x"
    ; dbg_p_y = regfile.reg_p_y -- "dbg_p_y"
    ; dbg_add_x = regfile.reg_add_x -- "dbg_add_x"
    ; dbg_add_y = regfile.reg_add_y -- "dbg_add_y"
    ; dbg_q_x = regfile.reg_q_x -- "dbg_q_x"
    ; dbg_q_y = regfile.reg_q_y -- "dbg_q_y"
    ; dbg_qpg_x = regfile.reg_qpg_x -- "dbg_qpg_x"
    ; dbg_qpg_y = regfile.reg_qpg_y -- "dbg_qpg_y"
    ; dbg_g_x = regfile.reg_g_x -- "dbg_g_x"
    ; dbg_g_y = regfile.reg_g_y -- "dbg_g_y"
    }
end