open Hardcaml
open Signal

module Config = struct
  let width = 256
  let num_registers = 32
  let reg_addr_width = 5
  
  (* secp256k1 field prime: p = 2^256 - 2^32 - 977 *)
  let prime_p = Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663"
  (* secp256k1 curve order *)
  let prime_n = Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671437"
  
  let z_to_constant z =
    let hex_str = Z.format "%x" z in
    let padded = String.make ((width / 4) - String.length hex_str) '0' ^ hex_str in
    Constant.of_hex_string ~width ~signedness:Unsigned padded
end

module Op = struct
  let add = 0
  let sub = 1
  let mul = 2
  let inv = 3
end

module State = struct
  type t =
    | Idle
    | Load
    | Capture
    | Compute
    | Write
    | Done
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
  
  (* State machine *)
  let sm = State_machine.create (module State) spec ~enable:vdd in
  
  (* Latched inputs *)
  let op_reg = Variable.reg spec ~width:2 in
  let prime_sel_reg = Variable.reg spec ~width:1 in
  let addr_a_reg = Variable.reg spec ~width:Config.reg_addr_width in
  let addr_b_reg = Variable.reg spec ~width:Config.reg_addr_width in
  let addr_out_reg = Variable.reg spec ~width:Config.reg_addr_width in
  
  (* Captured operands *)
  let operand_a = Variable.reg spec ~width:Config.width in
  let operand_b = Variable.reg spec ~width:Config.width in
  
  (* Operation start signals *)
  let start_add = Variable.reg spec ~width:1 in
  let start_sub = Variable.reg spec ~width:1 in
  let start_mul = Variable.reg spec ~width:1 in
  let start_inv = Variable.reg spec ~width:1 in
  
  (* Result capture *)
  let result_reg = Variable.reg spec ~width:Config.width in
  let inv_exists_reg = Variable.reg spec ~width:1 in
  
  (* Output registers *)
  let reg_write_enable = Variable.reg spec ~width:1 in
  let done_flag = Variable.reg spec ~width:1 in
  
  (* Prime constants *)
  let prime_p_const = Signal.of_constant (Config.z_to_constant Config.prime_p) in
  let prime_n_const = Signal.of_constant (Config.z_to_constant Config.prime_n) in
  let selected_prime = mux2 prime_sel_reg.value prime_n_const prime_p_const in
  
  (* Count significant bits in operand_b for multiplication optimization *)
  (* Simple approach: use full width, or implement leading zero count *)
  let num_bits_for_mul = of_int ~width:9 Config.width in
  
  (* Instantiate arithmetic modules *)
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
    ; num_bits = num_bits_for_mul
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
  
  (* Mux results based on operation *)
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
    (* Default: clear pulse signals *)
    start_add <-- gnd;
    start_sub <-- gnd;
    start_mul <-- gnd;
    start_inv <-- gnd;
    reg_write_enable <-- gnd;
    done_flag <-- gnd;
    
    sm.switch [
      State.Idle, [
        when_ i.start [
          (* Latch all inputs *)
          op_reg <-- i.op;
          prime_sel_reg <-- i.prime_sel;
          addr_a_reg <-- i.addr_a;
          addr_b_reg <-- i.addr_b;
          addr_out_reg <-- i.addr_out;
          sm.set_next Load;
        ];
      ];
      
      State.Load, [
        (* Wait one cycle for register file to provide data *)
        sm.set_next Capture;
      ];
      
      State.Capture, [
        (* Capture operands from register file *)
        operand_a <-- i.reg_read_data_a;
        operand_b <-- i.reg_read_data_b;
        
        (* Start the appropriate operation *)
        switch op_reg.value [
          of_int ~width:2 Op.add, [ start_add <-- vdd ];
          of_int ~width:2 Op.sub, [ start_sub <-- vdd ];
          of_int ~width:2 Op.mul, [ start_mul <-- vdd ];
          of_int ~width:2 Op.inv, [ start_inv <-- vdd ];
        ];
        
        sm.set_next Compute;
      ];
      
      State.Compute, [
        (* Wait for operation to complete *)
        when_ op_valid [
          result_reg <-- op_result;
          inv_exists_reg <-- mod_inv_out.exists;
          sm.set_next Write;
        ];
      ];
      
      State.Write, [
        (* Write result to register file *)
        reg_write_enable <-- vdd;
        sm.set_next Done;
      ];
      
      State.Done, [
        done_flag <-- vdd;
        sm.set_next Idle;
      ];
    ];
  ];
  
  (* Busy when not idle *)
  let busy = ~:(sm.is Idle) in
  
  { O.
    busy = busy -- "busy"
  ; done_ = done_flag.value -- "done"
  ; reg_write_enable = reg_write_enable.value -- "reg_write_enable"
  ; reg_write_addr = addr_out_reg.value -- "reg_write_addr"
  ; reg_write_data = result_reg.value -- "reg_write_data"
  ; reg_read_addr_a = addr_a_reg.value -- "reg_read_addr_a"
  ; reg_read_addr_b = addr_b_reg.value -- "reg_read_addr_b"
  ; inv_exists = inv_exists_reg.value -- "inv_exists"
  }