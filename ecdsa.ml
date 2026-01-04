open Base
open Hardcaml
open Signal

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
      ; reg_r : 'a [@bits Config.width]
      ; reg_s : 'a [@bits Config.width]
      ; reg_u1 : 'a [@bits Config.width]
      ; reg_u2 : 'a [@bits Config.width]
      ; reg_acc_x : 'a [@bits Config.width]
      ; reg_acc_y : 'a [@bits Config.width]
      ; reg_tmp1 : 'a [@bits Config.width]
      ; reg_tmp5 : 'a [@bits Config.width]
      ; reg_lambda_sq : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    
    let eff_write_enable = i.write_enable |: i.load_enable in
    let eff_write_addr = mux2 i.load_enable i.load_addr i.write_addr in
    let eff_write_data = mux2 i.load_enable i.load_data i.write_data in
    
    let registers = Array.init Config.num_registers ~f:(fun idx ->
      let r = Always.Variable.reg spec ~width:Config.width in
      Always.(compile [
        when_ (eff_write_enable &: (eff_write_addr ==:. idx)) [
          r <-- eff_write_data;
        ];
      ]);
      r.value
    ) in
    
    { O.read_data_a = mux i.read_addr_a (Array.to_list registers) -- "read_data_a"
    ; read_data_b = mux i.read_addr_b (Array.to_list registers) -- "read_data_b"
    ; reg_r = registers.(Config.Reg.r) -- "reg_r"
    ; reg_s = registers.(Config.Reg.s) -- "reg_s"
    ; reg_u1 = registers.(Config.Reg.u1) -- "reg_u1"
    ; reg_u2 = registers.(Config.Reg.u2) -- "reg_u2"
    ; reg_acc_x = registers.(Config.Reg.acc_x) -- "reg_acc_x"
    ; reg_acc_y = registers.(Config.Reg.acc_y) -- "reg_acc_y"
    ; reg_tmp1 = registers.(Config.Reg.tmp1) -- "reg_tmp1"
    ; reg_tmp5 = registers.(Config.Reg.tmp5) -- "reg_tmp5"
    ; reg_lambda_sq = registers.(Config.Reg.lambda_sq) -- "reg_lambda_sq"
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
    let done_flag = Variable.wire ~default:gnd in
    
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
    
    compile [
      start_add <-- gnd;
      start_sub <-- gnd;
      start_mul <-- gnd;
      start_inv <-- gnd;
      
      sm.switch [
        State.Idle, [
          when_ i.start [
            op_reg <-- i.op;
            prime_sel_reg <-- i.prime_sel;
            addr_a_reg <-- i.addr_a;
            addr_b_reg <-- i.addr_b;
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
            of_int ~width:2 Config.ArithOp.add, [ start_add <-- vdd ];
            of_int ~width:2 Config.ArithOp.sub, [ start_sub <-- vdd ];
            of_int ~width:2 Config.ArithOp.mul, [ start_mul <-- vdd ];
            of_int ~width:2 Config.ArithOp.inv, [ start_inv <-- vdd ];
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
    
    { O.busy = ~:is_idle_or_ready -- "busy"
    ; done_ = done_flag.value -- "done"
    ; reg_write_enable = reg_write_enable.value -- "reg_write_enable"
    ; reg_write_addr = write_addr_reg.value -- "reg_write_addr"
    ; reg_write_data = result_reg.value -- "reg_write_data"
    ; reg_read_addr_a = addr_a_reg.value -- "reg_read_addr_a"
    ; reg_read_addr_b = addr_b_reg.value -- "reg_read_addr_b"
    ; inv_exists = inv_exists_reg.value -- "inv_exists"
    }
end

module Controller = struct
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
      | Pdbl_2xsq
      | Pdbl_2xsq_wait
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
      | Test_done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; test_mode : 'a [@bits 2]
      ; arith_done : 'a
      ; arith_inv_exists : 'a
      ; u1_value : 'a [@bits Config.width]
      ; u2_value : 'a [@bits Config.width]
      ; acc_x_value : 'a [@bits Config.width]
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
    let test_mode_reg = Variable.reg spec ~width:2 in
    
    let error_invalid_r = Variable.reg spec ~width:1 in
    let error_invalid_s = Variable.reg spec ~width:1 in
    
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
      start_arith Config.ArithOp.add false src Config.Reg.zero dst
    in
    
    let double_reg src dst =
      start_arith Config.ArithOp.add false src src dst
    in
    
    let load_const addr data =
      [ load_enable <-- vdd
      ; load_addr <-- of_int ~width:Config.reg_addr_width addr
      ; load_data <-- data
      ]
    in
    
    let get_bit value idx =
      let bits = Array.init Config.width ~f:(fun j -> bit value j) in
      mux idx (Array.to_list bits)
    in
    let u1_bit = get_bit i.u1_value bit_idx.value in
    let u2_bit = get_bit i.u2_value bit_idx.value in
    
    let order_n = Config.order_n () in
    
    let state_encoding = List.mapi State.all ~f:(fun idx _ -> 
      of_int ~width:7 idx
    ) in
    let state_bits = mux sm.current state_encoding in
    
    let is_pdbl_test = test_mode_reg.value ==:. Config.TestMode.point_double in
    let is_padd_test = test_mode_reg.value ==:. Config.TestMode.point_add in
    
    compile [
      done_reg <-- gnd;
      
      sm.switch [
        State.Idle, [
          valid_sig <-- gnd;
          error_invalid_r <-- gnd;
          error_invalid_s <-- gnd;
          when_ i.start [
            load_step <-- zero 4;
            test_mode_reg <-- i.test_mode;
            switch i.test_mode [
              of_int ~width:2 Config.TestMode.normal, [ sm.set_next Load_inputs ];
              of_int ~width:2 Config.TestMode.point_double, [
                acc_is_infinity <-- gnd;
                sm.set_next Pdbl_save_px;
              ];
              of_int ~width:2 Config.TestMode.point_add, [
                acc_is_infinity <-- gnd;
                sm.set_next Padd_save_px;
              ];
              of_int ~width:2 Config.TestMode.scalar_mult, [ sm.set_next Loop_init ];
            ];
          ];
        ];
        
        State.Load_inputs, [
          switch load_step.value [
            of_int ~width:4 0, load_const Config.Reg.e i.input_e @ [ load_step <-- of_int ~width:4 1 ];
            of_int ~width:4 1, load_const Config.Reg.r i.input_r @ [ load_step <-- of_int ~width:4 2 ];
            of_int ~width:4 2, load_const Config.Reg.s i.input_s @ [ load_step <-- of_int ~width:4 3 ];
            of_int ~width:4 3, load_const Config.Reg.q_x i.input_q_x @ [ load_step <-- of_int ~width:4 4 ];
            of_int ~width:4 4, load_const Config.Reg.q_y i.input_q_y @ [ load_step <-- of_int ~width:4 5 ];
            of_int ~width:4 5, load_const Config.Reg.qpg_x i.input_qpg_x @ [ load_step <-- of_int ~width:4 6 ];
            of_int ~width:4 6, load_const Config.Reg.qpg_y i.input_qpg_y @ [ load_step <-- zero 4; sm.set_next Init_constants ];
          ];
        ];
        
        State.Init_constants, [
          switch load_step.value [
            of_int ~width:4 0, load_const Config.Reg.g_x (Config.g_x ()) @ [ load_step <-- of_int ~width:4 1 ];
            of_int ~width:4 1, load_const Config.Reg.g_y (Config.g_y ()) @ [ load_step <-- of_int ~width:4 2 ];
            of_int ~width:4 2, load_const Config.Reg.zero (zero Config.width) @ [ load_step <-- of_int ~width:4 3 ];
            of_int ~width:4 3, load_const Config.Reg.one (of_int ~width:Config.width 1) @ [ sm.set_next Validate_r_nonzero ];
          ];
        ];
        
        State.Validate_r_nonzero, [
          if_ (i.r_value ==: zero Config.width) [
            error_invalid_r <-- vdd; sm.set_next Validation_failed;
          ] [ sm.set_next Validate_r_less_than_n ];
        ];
        
        State.Validate_r_less_than_n, [
          if_ (i.r_value >=: order_n) [
            error_invalid_r <-- vdd; sm.set_next Validation_failed;
          ] [ sm.set_next Validate_s_nonzero ];
        ];
        
        State.Validate_s_nonzero, [
          if_ (i.s_value ==: zero Config.width) [
            error_invalid_s <-- vdd; sm.set_next Validation_failed;
          ] [ sm.set_next Validate_s_less_than_n ];
        ];
        
        State.Validate_s_less_than_n, [
          if_ (i.s_value >=: order_n) [
            error_invalid_s <-- vdd; sm.set_next Validation_failed;
          ] [ sm.set_next Compute_w ];
        ];
        
        State.Validation_failed, [ valid_sig <-- gnd; done_reg <-- vdd; sm.set_next Done ];
        
        State.Compute_w, start_arith Config.ArithOp.inv true Config.Reg.s Config.Reg.zero Config.Reg.w @ [ sm.set_next Wait_w ];
        State.Wait_w, [ when_ i.arith_done [ sm.set_next Check_w_exists ] ];
        State.Check_w_exists, [
          if_ (~:(i.arith_inv_exists)) [ error_invalid_s <-- vdd; sm.set_next Validation_failed ]
          [ sm.set_next Compute_u1 ];
        ];
        
        State.Compute_u1, start_arith Config.ArithOp.mul true Config.Reg.e Config.Reg.w Config.Reg.u1 @ [ sm.set_next Wait_u1 ];
        State.Wait_u1, [ when_ i.arith_done [ sm.set_next Compute_u2 ] ];
        State.Compute_u2, start_arith Config.ArithOp.mul true Config.Reg.r Config.Reg.w Config.Reg.u2 @ [ sm.set_next Wait_u2 ];
        State.Wait_u2, [ when_ i.arith_done [ sm.set_next Loop_init ] ];
        
        State.Loop_init, [ bit_idx <-- of_int ~width:9 255; acc_is_infinity <-- vdd; sm.set_next Loop_double_start ];
        State.Loop_double_start, [ if_ acc_is_infinity.value [ sm.set_next Loop_check_bits ] [ sm.set_next Pdbl_save_px ] ];
        
        (* Point doubling *)
        State.Pdbl_save_px, copy_reg Config.Reg.acc_x Config.Reg.p_x @ [ sm.set_next Pdbl_save_px_wait ];
        State.Pdbl_save_px_wait, [ when_ i.arith_done [ sm.set_next Pdbl_save_py ] ];
        State.Pdbl_save_py, copy_reg Config.Reg.acc_y Config.Reg.p_y @ [ sm.set_next Pdbl_save_py_wait ];
        State.Pdbl_save_py_wait, [ when_ i.arith_done [ sm.set_next Pdbl_xsq ] ];
        
        State.Pdbl_xsq, start_arith Config.ArithOp.mul false Config.Reg.p_x Config.Reg.p_x Config.Reg.tmp1 @ [ sm.set_next Pdbl_xsq_wait ];
        State.Pdbl_xsq_wait, [ when_ i.arith_done [ sm.set_next Pdbl_2xsq ] ];
        State.Pdbl_2xsq, double_reg Config.Reg.tmp1 Config.Reg.tmp2 @ [ sm.set_next Pdbl_2xsq_wait ];
        State.Pdbl_2xsq_wait, [ when_ i.arith_done [ sm.set_next Pdbl_3xsq ] ];
        State.Pdbl_3xsq, start_arith Config.ArithOp.add false Config.Reg.tmp2 Config.Reg.tmp1 Config.Reg.tmp2 @ [ sm.set_next Pdbl_3xsq_wait ];
        State.Pdbl_3xsq_wait, [ when_ i.arith_done [ sm.set_next Pdbl_2y ] ];
        State.Pdbl_2y, double_reg Config.Reg.p_y Config.Reg.tmp3 @ [ sm.set_next Pdbl_2y_wait ];
        State.Pdbl_2y_wait, [ when_ i.arith_done [ sm.set_next Pdbl_inv ] ];
        
        State.Pdbl_inv, start_arith Config.ArithOp.inv false Config.Reg.tmp3 Config.Reg.zero Config.Reg.tmp4 @ [ sm.set_next Pdbl_inv_wait ];
        State.Pdbl_inv_wait, [ when_ i.arith_done [ sm.set_next Pdbl_inv_check ] ];
        State.Pdbl_inv_check, [
          if_ (~:(i.arith_inv_exists)) [
            acc_is_infinity <-- vdd;
            if_ is_pdbl_test [ sm.set_next Test_done ] [ sm.set_next Loop_check_bits ];
          ] [ sm.set_next Pdbl_lambda ];
        ];
        
        State.Pdbl_lambda, start_arith Config.ArithOp.mul false Config.Reg.tmp2 Config.Reg.tmp4 Config.Reg.lambda @ [ sm.set_next Pdbl_lambda_wait ];
        State.Pdbl_lambda_wait, [ when_ i.arith_done [ sm.set_next Pdbl_lsq ] ];
        State.Pdbl_lsq, start_arith Config.ArithOp.mul false Config.Reg.lambda Config.Reg.lambda Config.Reg.lambda_sq @ [ sm.set_next Pdbl_lsq_wait ];
        State.Pdbl_lsq_wait, [ when_ i.arith_done [ sm.set_next Pdbl_2x ] ];
        State.Pdbl_2x, double_reg Config.Reg.p_x Config.Reg.tmp5 @ [ sm.set_next Pdbl_2x_wait ];
        State.Pdbl_2x_wait, [ when_ i.arith_done [ sm.set_next Pdbl_xr ] ];
        
        State.Pdbl_xr, start_arith Config.ArithOp.sub false Config.Reg.lambda_sq Config.Reg.tmp5 Config.Reg.tmp1 @ [ sm.set_next Pdbl_xr_wait ];
        State.Pdbl_xr_wait, [ when_ i.arith_done [ sm.set_next Pdbl_diff ] ];
        State.Pdbl_diff, start_arith Config.ArithOp.sub false Config.Reg.p_x Config.Reg.tmp1 Config.Reg.tmp2 @ [ sm.set_next Pdbl_diff_wait ];
        State.Pdbl_diff_wait, [ when_ i.arith_done [ sm.set_next Pdbl_ldiff ] ];
        State.Pdbl_ldiff, start_arith Config.ArithOp.mul false Config.Reg.lambda Config.Reg.tmp2 Config.Reg.tmp3 @ [ sm.set_next Pdbl_ldiff_wait ];
        State.Pdbl_ldiff_wait, [ when_ i.arith_done [ sm.set_next Pdbl_yr ] ];
        State.Pdbl_yr, start_arith Config.ArithOp.sub false Config.Reg.tmp3 Config.Reg.p_y Config.Reg.tmp4 @ [ sm.set_next Pdbl_yr_wait ];
        State.Pdbl_yr_wait, [ when_ i.arith_done [ sm.set_next Pdbl_copy_x ] ];
        
        State.Pdbl_copy_x, copy_reg Config.Reg.tmp1 Config.Reg.acc_x @ [ sm.set_next Pdbl_copy_x_wait ];
        State.Pdbl_copy_x_wait, [ when_ i.arith_done [ sm.set_next Pdbl_copy_y ] ];
        State.Pdbl_copy_y, copy_reg Config.Reg.tmp4 Config.Reg.acc_y @ [ sm.set_next Pdbl_copy_y_wait ];
        State.Pdbl_copy_y_wait, [
          when_ i.arith_done [
            if_ is_pdbl_test [ sm.set_next Test_done ] [ sm.set_next Loop_check_bits ];
          ];
        ];
        
        (* Bit checking and point loading *)
        State.Loop_check_bits, [
          let bits = u2_bit @: u1_bit in
          if_ (bits ==:. 0b00) [ sm.set_next Loop_next ]
          @@ elif (bits ==:. 0b01) [ sm.set_next Load_add_g_x ]
          @@ elif (bits ==:. 0b10) [ sm.set_next Load_add_q_x ]
          [ sm.set_next Load_add_qpg_x ];
        ];
        
        State.Load_add_g_x, copy_reg Config.Reg.g_x Config.Reg.add_x @ [ sm.set_next Load_add_g_x_wait ];
        State.Load_add_g_x_wait, [ when_ i.arith_done [ sm.set_next Load_add_g_y ] ];
        State.Load_add_g_y, copy_reg Config.Reg.g_y Config.Reg.add_y @ [ sm.set_next Load_add_g_y_wait ];
        State.Load_add_g_y_wait, [
          when_ i.arith_done [
            if_ acc_is_infinity.value [ sm.set_next Copy_to_acc_x ] [ sm.set_next Padd_save_px ];
          ];
        ];
        
        State.Load_add_q_x, copy_reg Config.Reg.q_x Config.Reg.add_x @ [ sm.set_next Load_add_q_x_wait ];
        State.Load_add_q_x_wait, [ when_ i.arith_done [ sm.set_next Load_add_q_y ] ];
        State.Load_add_q_y, copy_reg Config.Reg.q_y Config.Reg.add_y @ [ sm.set_next Load_add_q_y_wait ];
        State.Load_add_q_y_wait, [
          when_ i.arith_done [
            if_ acc_is_infinity.value [ sm.set_next Copy_to_acc_x ] [ sm.set_next Padd_save_px ];
          ];
        ];
        
        State.Load_add_qpg_x, copy_reg Config.Reg.qpg_x Config.Reg.add_x @ [ sm.set_next Load_add_qpg_x_wait ];
        State.Load_add_qpg_x_wait, [ when_ i.arith_done [ sm.set_next Load_add_qpg_y ] ];
        State.Load_add_qpg_y, copy_reg Config.Reg.qpg_y Config.Reg.add_y @ [ sm.set_next Load_add_qpg_y_wait ];
        State.Load_add_qpg_y_wait, [
          when_ i.arith_done [
            if_ acc_is_infinity.value [ sm.set_next Copy_to_acc_x ] [ sm.set_next Padd_save_px ];
          ];
        ];
        
        State.Copy_to_acc_x, copy_reg Config.Reg.add_x Config.Reg.acc_x @ [ sm.set_next Copy_to_acc_x_wait ];
        State.Copy_to_acc_x_wait, [ when_ i.arith_done [ sm.set_next Copy_to_acc_y ] ];
        State.Copy_to_acc_y, copy_reg Config.Reg.add_y Config.Reg.acc_y @ [ sm.set_next Copy_to_acc_y_wait ];
        State.Copy_to_acc_y_wait, [ when_ i.arith_done [ acc_is_infinity <-- gnd; sm.set_next Loop_next ] ];
        
        (* Point addition *)
        State.Padd_save_px, copy_reg Config.Reg.acc_x Config.Reg.p_x @ [ sm.set_next Padd_save_px_wait ];
        State.Padd_save_px_wait, [ when_ i.arith_done [ sm.set_next Padd_save_py ] ];
        State.Padd_save_py, copy_reg Config.Reg.acc_y Config.Reg.p_y @ [ sm.set_next Padd_save_py_wait ];
        State.Padd_save_py_wait, [ when_ i.arith_done [ sm.set_next Padd_num ] ];
        
        State.Padd_num, start_arith Config.ArithOp.sub false Config.Reg.add_y Config.Reg.p_y Config.Reg.tmp1 @ [ sm.set_next Padd_num_wait ];
        State.Padd_num_wait, [ when_ i.arith_done [ sm.set_next Padd_denom ] ];
        State.Padd_denom, start_arith Config.ArithOp.sub false Config.Reg.add_x Config.Reg.p_x Config.Reg.tmp2 @ [ sm.set_next Padd_denom_wait ];
        State.Padd_denom_wait, [ when_ i.arith_done [ sm.set_next Padd_inv ] ];
        State.Padd_inv, start_arith Config.ArithOp.inv false Config.Reg.tmp2 Config.Reg.zero Config.Reg.tmp3 @ [ sm.set_next Padd_inv_wait ];
        State.Padd_inv_wait, [ when_ i.arith_done [ sm.set_next Padd_inv_check ] ];
        State.Padd_inv_check, [
          if_ (~:(i.arith_inv_exists)) [
            acc_is_infinity <-- vdd;
            if_ is_padd_test [ sm.set_next Test_done ] [ sm.set_next Loop_next ];
          ] [ sm.set_next Padd_lambda ];
        ];
        
        State.Padd_lambda, start_arith Config.ArithOp.mul false Config.Reg.tmp1 Config.Reg.tmp3 Config.Reg.lambda @ [ sm.set_next Padd_lambda_wait ];
        State.Padd_lambda_wait, [ when_ i.arith_done [ sm.set_next Padd_lsq ] ];
        State.Padd_lsq, start_arith Config.ArithOp.mul false Config.Reg.lambda Config.Reg.lambda Config.Reg.lambda_sq @ [ sm.set_next Padd_lsq_wait ];
        State.Padd_lsq_wait, [ when_ i.arith_done [ sm.set_next Padd_xr1 ] ];
        State.Padd_xr1, start_arith Config.ArithOp.sub false Config.Reg.lambda_sq Config.Reg.p_x Config.Reg.tmp4 @ [ sm.set_next Padd_xr1_wait ];
        State.Padd_xr1_wait, [ when_ i.arith_done [ sm.set_next Padd_xr ] ];
        State.Padd_xr, start_arith Config.ArithOp.sub false Config.Reg.tmp4 Config.Reg.add_x Config.Reg.acc_x @ [ sm.set_next Padd_xr_wait ];
        State.Padd_xr_wait, [ when_ i.arith_done [ sm.set_next Padd_diff ] ];
        State.Padd_diff, start_arith Config.ArithOp.sub false Config.Reg.p_x Config.Reg.acc_x Config.Reg.tmp5 @ [ sm.set_next Padd_diff_wait ];
        State.Padd_diff_wait, [ when_ i.arith_done [ sm.set_next Padd_ldiff ] ];
        State.Padd_ldiff, start_arith Config.ArithOp.mul false Config.Reg.lambda Config.Reg.tmp5 Config.Reg.tmp4 @ [ sm.set_next Padd_ldiff_wait ];
        State.Padd_ldiff_wait, [ when_ i.arith_done [ sm.set_next Padd_yr ] ];
        State.Padd_yr, start_arith Config.ArithOp.sub false Config.Reg.tmp4 Config.Reg.p_y Config.Reg.acc_y @ [ sm.set_next Padd_yr_wait ];
        State.Padd_yr_wait, [
          when_ i.arith_done [
            if_ is_padd_test [ sm.set_next Test_done ] [ sm.set_next Loop_next ];
          ];
        ];
        
        State.Loop_next, [
          if_ (bit_idx.value ==:. 0) [ sm.set_next Final_check ]
          [ bit_idx <-- bit_idx.value -:. 1; sm.set_next Loop_double_start ];
        ];
        
        State.Final_check, [
          if_ acc_is_infinity.value [ valid_sig <-- gnd ]
          [
            let x_eq_r = i.acc_x_value ==: i.r_value in
            let x_minus_n = i.acc_x_value -: order_n in
            let x_mod_n_eq_r = (i.acc_x_value >=: order_n) &: (x_minus_n ==: i.r_value) in
            valid_sig <-- (x_eq_r |: x_mod_n_eq_r);
          ];
          done_reg <-- vdd;
          sm.set_next Done;
        ];
        
        State.Test_done, [ done_reg <-- vdd; sm.set_next Done ];
        
        State.Done, [
          done_reg <-- vdd;
          when_ i.start [
            done_reg <-- gnd; valid_sig <-- gnd;
            error_invalid_r <-- gnd; error_invalid_s <-- gnd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];
    
    { O.busy = ~:(sm.is Idle) -- "busy"
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
    }
end

module EcdsaVerify = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; test_mode : 'a [@bits 2]
      ; e : 'a [@bits Config.width]
      ; r : 'a [@bits Config.width]
      ; s : 'a [@bits Config.width]
      ; q_x : 'a [@bits Config.width]
      ; q_y : 'a [@bits Config.width]
      ; qplusg_x : 'a [@bits Config.width]
      ; qplusg_y : 'a [@bits Config.width]
      ; load_enable : 'a
      ; load_addr : 'a [@bits Config.reg_addr_width]
      ; load_data : 'a [@bits Config.width]
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
      ; dbg_state : 'a [@bits 7]
      ; dbg_bit_idx : 'a [@bits 9]
      ; dbg_acc_is_infinity : 'a
      ; dbg_acc_x : 'a [@bits Config.width]
      ; dbg_acc_y : 'a [@bits Config.width]
      ; dbg_tmp1 : 'a [@bits Config.width]
      ; dbg_tmp5 : 'a [@bits Config.width]
      ; dbg_lambda_sq : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    
    let arith_done_w = wire 1 in
    let arith_inv_exists_w = wire 1 in
    let reg_u1_w = wire Config.width in
    let reg_u2_w = wire Config.width in
    let reg_acc_x_w = wire Config.width in
    let reg_acc_y_w = wire Config.width in
    let reg_r_w = wire Config.width in
    let reg_s_w = wire Config.width in
    let reg_tmp1_w = wire Config.width in
    let reg_tmp5_w = wire Config.width in
    let reg_lambda_sq_w = wire Config.width in
    let reg_read_data_a_w = wire Config.width in
    let reg_read_data_b_w = wire Config.width in
    
    let ctrl = Controller.create (Scope.sub_scope scope "ctrl")
      { Controller.I.
        clock = i.clock
      ; clear = i.clear
      ; start = i.start
      ; test_mode = i.test_mode
      ; arith_done = arith_done_w
      ; arith_inv_exists = arith_inv_exists_w
      ; u1_value = reg_u1_w
      ; u2_value = reg_u2_w
      ; acc_x_value = reg_acc_x_w
      ; r_value = reg_r_w
      ; s_value = reg_s_w
      ; input_e = i.e
      ; input_r = i.r
      ; input_s = i.s
      ; input_q_x = i.q_x
      ; input_q_y = i.q_y
      ; input_qpg_x = i.qplusg_x
      ; input_qpg_y = i.qplusg_y
      }
    in
    
    let combined_load_enable = ctrl.load_enable |: i.load_enable in
    let combined_load_addr = mux2 ctrl.load_enable ctrl.load_addr i.load_addr in
    let combined_load_data = mux2 ctrl.load_enable ctrl.load_data i.load_data in
    
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
      ; reg_read_data_a = reg_read_data_a_w
      ; reg_read_data_b = reg_read_data_b_w
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
      ; load_enable = combined_load_enable
      ; load_addr = combined_load_addr
      ; load_data = combined_load_data
      }
    in
    
    arith_done_w <== arith.done_;
    arith_inv_exists_w <== arith.inv_exists;
    reg_u1_w <== regfile.reg_u1;
    reg_u2_w <== regfile.reg_u2;
    reg_acc_x_w <== regfile.reg_acc_x;
    reg_acc_y_w <== regfile.reg_acc_y;
    reg_r_w <== regfile.reg_r;
    reg_s_w <== regfile.reg_s;
    reg_tmp1_w <== regfile.reg_tmp1;
    reg_tmp5_w <== regfile.reg_tmp5;
    reg_lambda_sq_w <== regfile.reg_lambda_sq;
    reg_read_data_a_w <== regfile.read_data_a;
    reg_read_data_b_w <== regfile.read_data_b;
    
    { O.valid_signature = ctrl.valid_signature -- "valid_signature"
    ; done_ = ctrl.done_ -- "done"
    ; busy = ctrl.busy -- "busy"
    ; error_invalid_r = ctrl.error_invalid_r -- "error_invalid_r"
    ; error_invalid_s = ctrl.error_invalid_s -- "error_invalid_s"
    ; dbg_state = ctrl.dbg_state -- "dbg_state"
    ; dbg_bit_idx = ctrl.dbg_bit_idx -- "dbg_bit_idx"
    ; dbg_acc_is_infinity = ctrl.dbg_acc_is_infinity -- "dbg_acc_is_infinity"
    ; dbg_acc_x = reg_acc_x_w -- "dbg_acc_x"
    ; dbg_acc_y = reg_acc_y_w -- "dbg_acc_y"
    ; dbg_tmp1 = reg_tmp1_w -- "dbg_tmp1"
    ; dbg_tmp5 = reg_tmp5_w -- "dbg_tmp5"
    ; dbg_lambda_sq = reg_lambda_sq_w -- "dbg_lambda_sq"
    }
end