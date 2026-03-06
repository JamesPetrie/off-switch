open Hardcaml
open Signal


(* Arith - Modular arithmetic unit for secp256k1 field operations

   Performs add, sub, mul, inv modulo either field prime p or curve order n.
   Operands are read from and results written to an external 32×256-bit register file.

   Operations (op input):
     0 = add: f <- a + b  mod m
     1 = sub: f <- a - b  mod m
     2 = mul: f <- a * b  mod m
     3 = inv: f <- a^(-1) mod m  (b ignored)

   Modulus selection (prime_sel): 0 = prime_p, 1 = prime_n

   Protocol:
     1. Set a, b, op, prime_sel; pulse start
     2. Wait for done_ pulse; result written via reg_write_data signal
     3. For inv, check inv_exists to confirm inverse was found

   State machine: Idle -> Compute -> Idle
*)

module Config = struct
  let width = 256
  let num_registers = 32

  (* secp256k1 field prime: p = 2^256 - 2^32 - 977 *)
  let prime_p = Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663"
  (* secp256k1 curve order - CORRECTED *)
  let prime_n = Z.of_string "115792089237316195423570985008687907852837564279074904382605163141518161494337"

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
    | Compute
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid : 'a
    ; op : 'a [@bits 2]
    ; prime_sel : 'a
    ; reg_read_data_a : 'a [@bits Config.width]
    ; reg_read_data_b : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { busy : 'a
    ; ready : 'a
    ; reg_write_data : 'a [@bits Config.width]
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

  (* Captured operands *)
  let operand_a = Variable.reg spec ~width:Config.width in
  let operand_b = Variable.reg spec ~width:Config.width in

  (* Operation start signals *)
  let mod_add_valid = Variable.reg spec ~width:1 in
  let mod_sub_valid = Variable.reg spec ~width:1 in
  let start_mul = Variable.reg spec ~width:1 in
  let start_inv = Variable.reg spec ~width:1 in

  (* Prime constants *)
  let prime_p_const = Signal.of_constant (Config.z_to_constant Config.prime_p) in
  let prime_n_const = Signal.of_constant (Config.z_to_constant Config.prime_n) in
  let selected_prime = mux2 prime_sel_reg.value prime_n_const prime_p_const in

  (* Count significant bits in operand_b for multiplication optimization *)
  (* Simple approach: use full width, or implement leading zero count *)
  let num_bits_for_mul = of_int ~width:9 Config.width in

  (* Instantiate arithmetic modules *)

  (* Forward declaration wires for mod_add inputs, driven below after mod_mul is available *)
  let mod_add_valid_w    = Signal.wire 1 in
  let mod_add_a_w        = Signal.wire Config.width in
  let mod_add_b_w        = Signal.wire Config.width in
  let mod_add_subtract_w = Signal.wire 1 in

  let mod_add_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_add")
    { Mod_add.ModAdd.I.
      clock    = i.clock
    ; clear    = i.clear
    ; valid    = mod_add_valid_w
    ; a        = mod_add_a_w
    ; b        = mod_add_b_w
    ; modulus  = selected_prime
    ; subtract = mod_add_subtract_w
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
    ; mod_add_result = mod_add_out.result
    ; mod_add_ready  = mod_add_out.ready
    }
  in

  let mod_inv_out = Mod_inv.ModInv.create (Scope.sub_scope scope "mod_inv")
    { Mod_inv.ModInv.I.
      clock = i.clock
    ; clear = i.clear
    ; start = start_inv.value
    ; x = operand_a.value
    ; modulus = selected_prime
    ; mod_add_result   = mod_add_out.result
    ; mod_add_ready    = mod_add_out.ready
    ; mod_add_adjusted = mod_add_out.adjusted
    }
  in

  (* Drive mod_add inputs, muxed by operation *)
  assign mod_add_valid_w (mux op_reg.value [
    mod_add_valid.value;        (* add *)
    mod_sub_valid.value;        (* sub *)
    mod_mul_out.mod_add_valid;  (* mul *)
    mod_inv_out.mod_add_valid;  (* inv *)
  ]);
  assign mod_add_a_w (mux op_reg.value [
    operand_a.value;            (* add *)
    operand_a.value;            (* sub *)
    mod_mul_out.mod_add_a;      (* mul *)
    mod_inv_out.mod_add_a;      (* inv *)
  ]);
  assign mod_add_b_w (mux op_reg.value [
    operand_b.value;            (* add *)
    operand_b.value;            (* sub *)
    mod_mul_out.mod_add_b;      (* mul *)
    mod_inv_out.mod_add_b;      (* inv *)
  ]);
  assign mod_add_subtract_w (mux op_reg.value [
    gnd;                          (* add *)
    vdd;                          (* sub *)
    mod_mul_out.mod_add_subtract; (* mul *)
    mod_inv_out.mod_add_subtract; (* inv *)
  ]);

  (* Mux results based on operation *)
  let op_result =
    mux op_reg.value [
      mod_add_out.result;
      mod_add_out.result;
      mod_mul_out.result;
      mod_inv_out.result;
    ]
  in

  let op_ready =
    mux op_reg.value [
      mod_add_out.ready;
      mod_add_out.ready;
      mod_mul_out.valid;
      mod_inv_out.valid;
    ]
  in

  compile [
    (* Default: clear pulse signals *)
    (* TODO move start_mul, start_inv clear to Compute step as well when updated *)
    start_mul <-- gnd;
    start_inv <-- gnd;

    sm.switch [
      State.Idle, [
        when_ i.valid [
          (* Latch all inputs *)
          op_reg <-- i.op;
          prime_sel_reg <-- i.prime_sel;
          operand_a <-- i.reg_read_data_a;
          operand_b <-- i.reg_read_data_b;

          (* Start the required operation *)
          switch i.op [
            of_int ~width:2 Op.add, [ mod_add_valid <-- vdd ];
            of_int ~width:2 Op.sub, [ mod_sub_valid <-- vdd ];
            of_int ~width:2 Op.mul, [ start_mul <-- vdd ];
            of_int ~width:2 Op.inv, [ start_inv <-- vdd ];
          ];

          (* Move to next state *)
          sm.set_next Compute;
        ];
      ];

      State.Compute, [
        (* Simply wait for operation to complete *)
        when_ op_ready [
          (* Clear valid signals *)
          mod_add_valid <-- gnd;
          mod_sub_valid <-- gnd;

          sm.set_next Idle;
        ];
      ];
    ];
  ];

  (* Busy when not idle *)
  let busy = ~:(sm.is Idle) in

  { O.
    busy = busy -- "busy"
  ; ready = op_ready -- "done"
  ; reg_write_data = op_result -- "reg_write_data"
  ; inv_exists = mod_inv_out.exists -- "inv_exists"
  }