open Base
open Hardcaml
open Signal

module Config = struct
  let width = 256
  
  (* secp256k1 parameters *)
  let prime_p_z = Z.of_string 
    "115792089237316195423570985008687907853269984665640564039457584007908834671663"
  let order_n_z = Z.of_string
    "115792089237316195423570985008687907852837564279074904382605163141518161494337"
  let coeff_a_z = Z.zero  (* secp256k1 has a = 0 *)
  
  (* Generator point G *)
  let g_x_z = Z.of_string
    "55066263022277343669578718895168534326250603453777594175500187360389116729240"
  let g_y_z = Z.of_string
    "32670510020758816978083085130507043184471273380659243275938904335757337482424"
  
  let z_to_constant z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((width + 3) / 4) ~char:'0' in
    Constant.of_hex_string ~width ~signedness:Unsigned padded
    
  let prime_p () = Signal.of_constant (z_to_constant prime_p_z)
  let order_n () = Signal.of_constant (z_to_constant order_n_z)
  let coeff_a () = Signal.of_constant (z_to_constant coeff_a_z)
  let g_x () = Signal.of_constant (z_to_constant g_x_z)
  let g_y () = Signal.of_constant (z_to_constant g_y_z)
  
  let n_bitlength = 256
end

(* ============================================================
   Modular Arithmetic Unit - Unified Interface
   ============================================================ *)
module ArithOp = struct
  type t = Add | Sub | Mul | Inv
  [@@deriving sexp_of, compare, enumerate]
  
  let to_int = function Add -> 0 | Sub -> 1 | Mul -> 2 | Inv -> 3
  let num_bits = 2
end

module FieldArith = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; op : 'a [@bits ArithOp.num_bits]
      ; operand_a : 'a [@bits Config.width]
      ; operand_b : 'a [@bits Config.width]
      ; modulus : 'a [@bits Config.width]
      ; num_bits : 'a [@bits 9]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits Config.width]
      ; valid : 'a
      ; busy : 'a
      ; inv_exists : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t = Idle | Running_add | Running_mul | Running_inv
    [@@deriving sexp_of, compare, enumerate]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    let add_start = Variable.wire ~default:gnd in
    let mul_start = Variable.wire ~default:gnd in
    let inv_start = Variable.wire ~default:gnd in
    
    (* Note: ModAdd needs modification to accept modulus as input.
       For now, we'll use it only for mod-p operations.
       For mod-n, we need a separate approach or modified ModAdd. *)
    let mod_add = ModAdd.create 
      (Scope.sub_scope scope "add")
      { ModAdd.I.
        clock = i.clock
      ; clear = i.clear
      ; start = add_start.value
      ; a = i.operand_a
      ; b = i.operand_b
      ; subtract = (i.op ==:. ArithOp.(to_int Sub))
      }
    in
    
    let mod_mul = ModMul.create
      (Scope.sub_scope scope "mul")
      { ModMul.I.
        clock = i.clock
      ; clear = i.clear
      ; start = mul_start.value
      ; x = i.operand_a
      ; y = i.operand_b
      ; modulus = i.modulus
      ; num_bits = i.num_bits
      }
    in
    
    let mod_inv = ModInv.create
      (Scope.sub_scope scope "inv")
      { ModInv.I.
        clock = i.clock
      ; clear = i.clear
      ; start = inv_start.value
      ; x = i.operand_a
      ; modulus = i.modulus
      }
    in
    
    let result = Variable.reg spec ~width:Config.width in
    let valid = Variable.reg spec ~width:1 in
    let inv_exists = Variable.reg spec ~width:1 in
    
    let op_is_add = i.op ==:. ArithOp.(to_int Add) in
    let op_is_sub = i.op ==:. ArithOp.(to_int Sub) in
    let op_is_mul = i.op ==:. ArithOp.(to_int Mul) in
    let op_is_inv = i.op ==:. ArithOp.(to_int Inv) in
    
    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          when_ i.start [
            if_ (op_is_add |: op_is_sub) [
              add_start <-- vdd;
              sm.set_next Running_add;
            ] @@ elif op_is_mul [
              mul_start <-- vdd;
              sm.set_next Running_mul;
            ] @@ elif op_is_inv [
              inv_start <-- vdd;
              sm.set_next Running_inv;
            ] [];
          ];
        ];
        
        State.Running_add, [
          when_ mod_add.valid [
            result <-- mod_add.result;
            valid <-- vdd;
            sm.set_next Idle;
          ];
        ];
        
        State.Running_mul, [
          when_ mod_mul.valid [
            result <-- mod_mul.result;
            valid <-- vdd;
            sm.set_next Idle;
          ];
        ];
        
        State.Running_inv, [
          when_ mod_inv.valid [
            result <-- mod_inv.result;
            inv_exists <-- mod_inv.exists;
            valid <-- vdd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];
    
    { O.
      result = result.value -- "result"
    ; valid = valid.value -- "valid"
    ; busy = ~:(sm.is Idle) -- "busy"
    ; inv_exists = inv_exists.value -- "inv_exists"
    }
end

(* ============================================================
   Point Addition: P + Q (affine coordinates)
   ============================================================ *)
module PointAdd = struct
  module State = struct
    type t =
      | Idle
      (* λ = (yQ - yP) / (xQ - xP) *)
      | Compute_numerator     (* yQ - yP *)
      | Wait_numerator
      | Compute_denominator   (* xQ - xP *)
      | Wait_denominator
      | Compute_denom_inv     (* (xQ - xP)^(-1) *)
      | Wait_denom_inv
      | Compute_lambda        (* numerator * denom_inv *)
      | Wait_lambda
      (* xR = λ² - xP - xQ *)
      | Compute_lambda_sq     (* λ² *)
      | Wait_lambda_sq
      | Compute_xR_sub_xP     (* λ² - xP *)
      | Wait_xR_sub_xP
      | Compute_xR            (* (λ² - xP) - xQ *)
      | Wait_xR
      (* yR = λ(xP - xR) - yP *)
      | Compute_xP_minus_xR   (* xP - xR *)
      | Wait_xP_minus_xR
      | Compute_lambda_diff   (* λ * (xP - xR) *)
      | Wait_lambda_diff
      | Compute_yR            (* λ(xP - xR) - yP *)
      | Wait_yR
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; xP : 'a [@bits Config.width]
      ; yP : 'a [@bits Config.width]
      ; xQ : 'a [@bits Config.width]
      ; yQ : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { xR : 'a [@bits Config.width]
      ; yR : 'a [@bits Config.width]
      ; valid : 'a
      ; busy : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    (* Arithmetic unit interface *)
    let arith_start = Variable.wire ~default:gnd in
    let arith_op = Variable.wire ~default:(zero ArithOp.num_bits) in
    let arith_a = Variable.wire ~default:(zero Config.width) in
    let arith_b = Variable.wire ~default:(zero Config.width) in
    
    let arith = FieldArith.create
      (Scope.sub_scope scope "arith")
      { FieldArith.I.
        clock = i.clock
      ; clear = i.clear
      ; start = arith_start.value
      ; op = arith_op.value
      ; operand_a = arith_a.value
      ; operand_b = arith_b.value
      ; modulus = Config.prime_p ()
      ; num_bits = Signal.of_int ~width:9 Config.n_bitlength
      }
    in
    
    (* Intermediate registers *)
    let xP_r = Variable.reg spec ~width:Config.width in
    let yP_r = Variable.reg spec ~width:Config.width in
    let xQ_r = Variable.reg spec ~width:Config.width in
    let yQ_r = Variable.reg spec ~width:Config.width in
    let numerator = Variable.reg spec ~width:Config.width in
    let denom_inv = Variable.reg spec ~width:Config.width in
    let lambda = Variable.reg spec ~width:Config.width in
    let lambda_sq = Variable.reg spec ~width:Config.width in
    let xR = Variable.reg spec ~width:Config.width in
    let yR = Variable.reg spec ~width:Config.width in
    let xP_minus_xR = Variable.reg spec ~width:Config.width in
    let valid = Variable.reg spec ~width:1 in
    
    (* Helper to start arithmetic operation *)
    let start_op op a b =
      [ arith_start <-- vdd
      ; arith_op <-- Signal.of_int ~width:ArithOp.num_bits (ArithOp.to_int op)
      ; arith_a <-- a
      ; arith_b <-- b
      ]
    in
    
    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          when_ i.start [
            xP_r <-- i.xP;
            yP_r <-- i.yP;
            xQ_r <-- i.xQ;
            yQ_r <-- i.yQ;
            sm.set_next Compute_numerator;
          ];
        ];
        
        (* λ = (yQ - yP) / (xQ - xP) *)
        State.Compute_numerator, 
          start_op Sub i.yQ i.yP @ [sm.set_next Wait_numerator];
        
        State.Wait_numerator, [
          when_ arith.valid [
            numerator <-- arith.result;
            sm.set_next Compute_denominator;
          ];
        ];
        
        State.Compute_denominator,
          start_op Sub xQ_r.value xP_r.value @ [sm.set_next Wait_denominator];
        
        State.Wait_denominator, [
          when_ arith.valid [
            sm.set_next Compute_denom_inv;
          ];
        ];
        
        State.Compute_denom_inv,
          start_op Inv arith.result (zero Config.width) @ [sm.set_next Wait_denom_inv];
        
        State.Wait_denom_inv, [
          when_ arith.valid [
            denom_inv <-- arith.result;
            sm.set_next Compute_lambda;
          ];
        ];
        
        State.Compute_lambda,
          start_op Mul numerator.value denom_inv.value @ [sm.set_next Wait_lambda];
        
        State.Wait_lambda, [
          when_ arith.valid [
            lambda <-- arith.result;
            sm.set_next Compute_lambda_sq;
          ];
        ];
        
        (* xR = λ² - xP - xQ *)
        State.Compute_lambda_sq,
          start_op Mul lambda.value lambda.value @ [sm.set_next Wait_lambda_sq];
        
        State.Wait_lambda_sq, [
          when_ arith.valid [
            lambda_sq <-- arith.result;
            sm.set_next Compute_xR_sub_xP;
          ];
        ];
        
        State.Compute_xR_sub_xP,
          start_op Sub lambda_sq.value xP_r.value @ [sm.set_next Wait_xR_sub_xP];
        
        State.Wait_xR_sub_xP, [
          when_ arith.valid [
            sm.set_next Compute_xR;
          ];
        ];
        
        State.Compute_xR,
          start_op Sub arith.result xQ_r.value @ [sm.set_next Wait_xR];
        
        State.Wait_xR, [
          when_ arith.valid [
            xR <-- arith.result;
            sm.set_next Compute_xP_minus_xR;
          ];
        ];
        
        (* yR = λ(xP - xR) - yP *)
        State.Compute_xP_minus_xR,
          start_op Sub xP_r.value xR.value @ [sm.set_next Wait_xP_minus_xR];
        
        State.Wait_xP_minus_xR, [
          when_ arith.valid [
            xP_minus_xR <-- arith.result;
            sm.set_next Compute_lambda_diff;
          ];
        ];
        
        State.Compute_lambda_diff,
          start_op Mul lambda.value xP_minus_xR.value @ [sm.set_next Wait_lambda_diff];
        
        State.Wait_lambda_diff, [
          when_ arith.valid [
            sm.set_next Compute_yR;
          ];
        ];
        
        State.Compute_yR,
          start_op Sub arith.result yP_r.value @ [sm.set_next Wait_yR];
        
        State.Wait_yR, [
          when_ arith.valid [
            yR <-- arith.result;
            valid <-- vdd;
            sm.set_next Done;
          ];
        ];
        
        State.Done, [
          valid <-- vdd;
          when_ i.start [
            valid <-- gnd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];
    
    { O.
      xR = xR.value -- "xR"
    ; yR = yR.value -- "yR"
    ; valid = valid.value -- "valid"
    ; busy = ~:(sm.is Idle) -- "busy"
    }
end

(* ============================================================
   Point Doubling: 2P (affine coordinates)
   ============================================================ *)
module PointDouble = struct
  module State = struct
    type t =
      | Idle
      (* λ = (3xP² + a) / (2yP) *)
      | Compute_xP_sq           (* xP² *)
      | Wait_xP_sq
      | Compute_3xP_sq          (* 3 * xP² *)
      | Wait_3xP_sq
      | Compute_numerator       (* 3xP² + a *)
      | Wait_numerator
      | Compute_2yP             (* 2 * yP *)
      | Wait_2yP
      | Compute_denom_inv       (* (2yP)^(-1) *)
      | Wait_denom_inv
      | Compute_lambda          (* numerator * denom_inv *)
      | Wait_lambda
      (* xR = λ² - 2xP *)
      | Compute_lambda_sq       (* λ² *)
      | Wait_lambda_sq
      | Compute_2xP             (* 2 * xP *)
      | Wait_2xP
      | Compute_xR              (* λ² - 2xP *)
      | Wait_xR
      (* yR = λ(xP - xR) - yP *)
      | Compute_xP_minus_xR
      | Wait_xP_minus_xR
      | Compute_lambda_diff
      | Wait_lambda_diff
      | Compute_yR
      | Wait_yR
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; xP : 'a [@bits Config.width]
      ; yP : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { xR : 'a [@bits Config.width]
      ; yR : 'a [@bits Config.width]
      ; valid : 'a
      ; busy : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    let arith_start = Variable.wire ~default:gnd in
    let arith_op = Variable.wire ~default:(zero ArithOp.num_bits) in
    let arith_a = Variable.wire ~default:(zero Config.width) in
    let arith_b = Variable.wire ~default:(zero Config.width) in
    
    let arith = FieldArith.create
      (Scope.sub_scope scope "arith")
      { FieldArith.I.
        clock = i.clock
      ; clear = i.clear
      ; start = arith_start.value
      ; op = arith_op.value
      ; operand_a = arith_a.value
      ; operand_b = arith_b.value
      ; modulus = Config.prime_p ()
      ; num_bits = Signal.of_int ~width:9 Config.n_bitlength
      }
    in
    
    (* Constants *)
    let const_2 = Signal.of_int ~width:Config.width 2 in
    let const_3 = Signal.of_int ~width:Config.width 3 in
    let coeff_a = Config.coeff_a () in
    
    (* Intermediate registers *)
    let xP_r = Variable.reg spec ~width:Config.width in
    let yP_r = Variable.reg spec ~width:Config.width in
    let xP_sq = Variable.reg spec ~width:Config.width in
    let numerator = Variable.reg spec ~width:Config.width in
    let denom_inv = Variable.reg spec ~width:Config.width in
    let lambda = Variable.reg spec ~width:Config.width in
    let lambda_sq = Variable.reg spec ~width:Config.width in
    let two_xP = Variable.reg spec ~width:Config.width in
    let xR = Variable.reg spec ~width:Config.width in
    let yR = Variable.reg spec ~width:Config.width in
    let xP_minus_xR = Variable.reg spec ~width:Config.width in
    let valid = Variable.reg spec ~width:1 in
    
    let start_op op a b =
      [ arith_start <-- vdd
      ; arith_op <-- Signal.of_int ~width:ArithOp.num_bits (ArithOp.to_int op)
      ; arith_a <-- a
      ; arith_b <-- b
      ]
    in
    
    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          when_ i.start [
            xP_r <-- i.xP;
            yP_r <-- i.yP;
            sm.set_next Compute_xP_sq;
          ];
        ];
        
        (* λ = (3xP² + a) / (2yP) *)
        State.Compute_xP_sq,
          start_op Mul i.xP i.xP @ [sm.set_next Wait_xP_sq];
        
        State.Wait_xP_sq, [
          when_ arith.valid [
            xP_sq <-- arith.result;
            sm.set_next Compute_3xP_sq;
          ];
        ];
        
        State.Compute_3xP_sq,
          start_op Mul xP_sq.value const_3 @ [sm.set_next Wait_3xP_sq];
        
        State.Wait_3xP_sq, [
          when_ arith.valid [
            sm.set_next Compute_numerator;
          ];
        ];
        
        State.Compute_numerator,
          start_op Add arith.result coeff_a @ [sm.set_next Wait_numerator];
        
        State.Wait_numerator, [
          when_ arith.valid [
            numerator <-- arith.result;
            sm.set_next Compute_2yP;
          ];
        ];
        
        State.Compute_2yP,
          start_op Mul yP_r.value const_2 @ [sm.set_next Wait_2yP];
        
        State.Wait_2yP, [
          when_ arith.valid [
            sm.set_next Compute_denom_inv;
          ];
        ];
        
        State.Compute_denom_inv,
          start_op Inv arith.result (zero Config.width) @ [sm.set_next Wait_denom_inv];
        
        State.Wait_denom_inv, [
          when_ arith.valid [
            denom_inv <-- arith.result;
            sm.set_next Compute_lambda;
          ];
        ];
        
        State.Compute_lambda,
          start_op Mul numerator.value denom_inv.value @ [sm.set_next Wait_lambda];
        
        State.Wait_lambda, [
          when_ arith.valid [
            lambda <-- arith.result;
            sm.set_next Compute_lambda_sq;
          ];
        ];
        
        (* xR = λ² - 2xP *)
        State.Compute_lambda_sq,
          start_op Mul lambda.value lambda.value @ [sm.set_next Wait_lambda_sq];
        
        State.Wait_lambda_sq, [
          when_ arith.valid [
            lambda_sq <-- arith.result;
            sm.set_next Compute_2xP;
          ];
        ];
        
        State.Compute_2xP,
          start_op Mul xP_r.value const_2 @ [sm.set_next Wait_2xP];
        
        State.Wait_2xP, [
          when_ arith.valid [
            two_xP <-- arith.result;
            sm.set_next Compute_xR;
          ];
        ];
        
        State.Compute_xR,
          start_op Sub lambda_sq.value two_xP.value @ [sm.set_next Wait_xR];
        
        State.Wait_xR, [
          when_ arith.valid [
            xR <-- arith.result;
            sm.set_next Compute_xP_minus_xR;
          ];
        ];
        
        (* yR = λ(xP - xR) - yP *)
        State.Compute_xP_minus_xR,
          start_op Sub xP_r.value xR.value @ [sm.set_next Wait_xP_minus_xR];
        
        State.Wait_xP_minus_xR, [
          when_ arith.valid [
            xP_minus_xR <-- arith.result;
            sm.set_next Compute_lambda_diff;
          ];
        ];
        
        State.Compute_lambda_diff,
          start_op Mul lambda.value xP_minus_xR.value @ [sm.set_next Wait_lambda_diff];
        
        State.Wait_lambda_diff, [
          when_ arith.valid [
            sm.set_next Compute_yR;
          ];
        ];
        
        State.Compute_yR,
          start_op Sub arith.result yP_r.value @ [sm.set_next Wait_yR];
        
        State.Wait_yR, [
          when_ arith.valid [
            yR <-- arith.result;
            valid <-- vdd;
            sm.set_next Done;
          ];
        ];
        
        State.Done, [
          valid <-- vdd;
          when_ i.start [
            valid <-- gnd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];
    
    { O.
      xR = xR.value -- "xR"
    ; yR = yR.value -- "yR"
    ; valid = valid.value -- "valid"
    ; busy = ~:(sm.is Idle) -- "busy"
    }
end

(* ============================================================
   ECDSA Verifier - Main Module
   ============================================================ *)
module EcdsaVerify = struct
  module State = struct
    type t =
      | Idle
      | Check_inputs
      (* w = s^(-1) mod n *)
      | Compute_w
      | Wait_w
      (* u1 = e * w mod n *)
      | Compute_u1
      | Wait_u1
      (* u2 = r * w mod n *)
      | Compute_u2
      | Wait_u2
      (* Main loop: Shamir's trick *)
      | Loop_init
      | Loop_double
      | Wait_double
      | Loop_check_bits
      | Loop_add
      | Wait_add
      | Loop_next
      (* Final check *)
      | Final_check
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      (* Message hash *)
      ; e : 'a [@bits Config.width]
      (* Signature *)
      ; r : 'a [@bits Config.width]
      ; s : 'a [@bits Config.width]
      (* Public key Q *)
      ; q_x : 'a [@bits Config.width]
      ; q_y : 'a [@bits Config.width]
      (* Precomputed Q + G *)
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
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    (* Constants *)
    let order_n = Config.order_n () in
    let g_x = Config.g_x () in
    let g_y = Config.g_y () in
    
    (* Arithmetic unit for mod-n operations (w, u1, u2) *)
    let arith_start = Variable.wire ~default:gnd in
    let arith_op = Variable.wire ~default:(zero ArithOp.num_bits) in
    let arith_a = Variable.wire ~default:(zero Config.width) in
    let arith_b = Variable.wire ~default:(zero Config.width) in
    
    let arith = FieldArith.create
      (Scope.sub_scope scope "arith_n")
      { FieldArith.I.
        clock = i.clock
      ; clear = i.clear
      ; start = arith_start.value
      ; op = arith_op.value
      ; operand_a = arith_a.value
      ; operand_b = arith_b.value
      ; modulus = order_n
      ; num_bits = Signal.of_int ~width:9 Config.n_bitlength
      }
    in
    
    (* Point addition unit *)
    let add_start = Variable.wire ~default:gnd in
    let add_xP = Variable.wire ~default:(zero Config.width) in
    let add_yP = Variable.wire ~default:(zero Config.width) in
    let add_xQ = Variable.wire ~default:(zero Config.width) in
    let add_yQ = Variable.wire ~default:(zero Config.width) in
    
    let point_add = PointAdd.create
      (Scope.sub_scope scope "point_add")
      { PointAdd.I.
        clock = i.clock
      ; clear = i.clear
      ; start = add_start.value
      ; xP = add_xP.value
      ; yP = add_yP.value
      ; xQ = add_xQ.value
      ; yQ = add_yQ.value
      }
    in
    
    (* Point doubling unit *)
    let dbl_start = Variable.wire ~default:gnd in
    let dbl_xP = Variable.wire ~default:(zero Config.width) in
    let dbl_yP = Variable.wire ~default:(zero Config.width) in
    
    let point_dbl = PointDouble.create
      (Scope.sub_scope scope "point_dbl")
      { PointDouble.I.
        clock = i.clock
      ; clear = i.clear
      ; start = dbl_start.value
      ; xP = dbl_xP.value
      ; yP = dbl_yP.value
      }
    in
    
    (* Input/intermediate registers *)
    let e_r = Variable.reg spec ~width:Config.width in
    let r_r = Variable.reg spec ~width:Config.width in
    let s_r = Variable.reg spec ~width:Config.width in
    let q_x_r = Variable.reg spec ~width:Config.width in
    let q_y_r = Variable.reg spec ~width:Config.width in
    let qplusg_x_r = Variable.reg spec ~width:Config.width in
    let qplusg_y_r = Variable.reg spec ~width:Config.width in
    
    let w = Variable.reg spec ~width:Config.width in
    let u1 = Variable.reg spec ~width:Config.width in
    let u2 = Variable.reg spec ~width:Config.width in
    
    (* Accumulator point *)
    let acc_x = Variable.reg spec ~width:Config.width in
    let acc_y = Variable.reg spec ~width:Config.width in
    let acc_is_infinity = Variable.reg spec ~width:1 in
    
    (* Loop counter *)
    let bit_idx = Variable.reg spec ~width:9 in
    
    (* Point to add in current iteration *)
    let add_point_x = Variable.reg spec ~width:Config.width in
    let add_point_y = Variable.reg spec ~width:Config.width in
    let should_add = Variable.reg spec ~width:1 in
    
    (* Output *)
    let valid_sig = Variable.reg spec ~width:1 in
    let done_r = Variable.reg spec ~width:1 in
    
    (* Helper: check if value is zero *)
    let is_zero v = v ==:. 0 in
    
    (* Helper: get bit at index from value *)
    let get_bit value idx = 
      bit value (sel_bottom idx 8)
    in
    
    let start_arith_op op a b =
      [ arith_start <-- vdd
      ; arith_op <-- Signal.of_int ~width:ArithOp.num_bits (ArithOp.to_int op)
      ; arith_a <-- a
      ; arith_b <-- b
      ]
    in
    
    compile [
      sm.switch [
        State.Idle, [
          done_r <-- gnd;
          valid_sig <-- gnd;
          when_ i.start [
            (* Latch inputs *)
            e_r <-- i.e;
            r_r <-- i.r;
            s_r <-- i.s;
            q_x_r <-- i.q_x;
            q_y_r <-- i.q_y;
            qplusg_x_r <-- i.qplusg_x;
            qplusg_y_r <-- i.qplusg_y;
            sm.set_next Check_inputs;
          ];
        ];
        
        State.Check_inputs, [
          (* Check r, s in [1, n-1] and e in [0, n-1] *)
          let r_invalid = (i.r >=: order_n) |: (is_zero i.r) in
          let s_invalid = (i.s >=: order_n) |: (is_zero i.s) in
          let e_invalid = i.e >=: order_n in
          
          if_ (r_invalid |: s_invalid |: e_invalid) [
            valid_sig <-- gnd;
            done_r <-- vdd;
            sm.set_next Done;
          ] [
            sm.set_next Compute_w;
          ];
        ];
        
        (* w = s^(-1) mod n *)
        State.Compute_w, 
          start_arith_op Inv s_r.value (zero Config.width) @ [sm.set_next Wait_w];
        
        State.Wait_w, [
          when_ arith.valid [
            w <-- arith.result;
            sm.set_next Compute_u1;
          ];
        ];
        
        (* u1 = e * w mod n *)
        State.Compute_u1,
          start_arith_op Mul e_r.value w.value @ [sm.set_next Wait_u1];
        
        State.Wait_u1, [
          when_ arith.valid [
            u1 <-- arith.result;
            sm.set_next Compute_u2;
          ];
        ];
        
        (* u2 = r * w mod n *)
        State.Compute_u2,
          start_arith_op Mul r_r.value w.value @ [sm.set_next Wait_u2];
        
        State.Wait_u2, [
          when_ arith.valid [
            u2 <-- arith.result;
            sm.set_next Loop_init;
          ];
        ];
        
        (* Initialize loop *)
        State.Loop_init, [
          acc_x <-- zero Config.width;
          acc_y <-- zero Config.width;
          acc_is_infinity <-- vdd;
          bit_idx <-- Signal.of_int ~width:9 (Config.n_bitlength - 1);
          sm.set_next Loop_check_bits;
        ];
        
        (* Double accumulator (if not infinity) *)
        State.Loop_double, [
          dbl_start <-- vdd;
          dbl_xP <-- acc_x.value;
          dbl_yP <-- acc_y.value;
          sm.set_next Wait_double;
        ];
        
        State.Wait_double, [
          when_ point_dbl.valid [
            acc_x <-- point_dbl.xR;
            acc_y <-- point_dbl.yR;
            (* Check if result is infinity (y = 0) *)
            acc_is_infinity <-- is_zero point_dbl.yR;
            sm.set_next Loop_check_bits;
          ];
        ];
        
        (* Check bits and determine which point to add *)
        State.Loop_check_bits, [
          let bit_u1 = get_bit u1.value bit_idx.value in
          let bit_u2 = get_bit u2.value bit_idx.value in
          let bits = bit_u2 @: bit_u1 in  (* 2-bit value: u2_bit:u1_bit *)
          
          (* Select point based on bit pattern *)
          if_ (bits ==:. 0b00) [
            (* No addition needed *)
            should_add <-- gnd;
            sm.set_next Loop_next;
          ] @@ elif (bits ==:. 0b01) [
            (* Add G *)
            add_point_x <-- g_x;
            add_point_y <-- g_y;
            should_add <-- vdd;
            sm.set_next Loop_add;
          ] @@ elif (bits ==:. 0b10) [
            (* Add Q *)
            add_point_x <-- q_x_r.value;
            add_point_y <-- q_y_r.value;
            should_add <-- vdd;
            sm.set_next Loop_add;
          ] [
            (* Add Q + G *)
            add_point_x <-- qplusg_x_r.value;
            add_point_y <-- qplusg_y_r.value;
            should_add <-- vdd;
            sm.set_next Loop_add;
          ];
        ];
        
        (* Add point to accumulator *)
        State.Loop_add, [
          if_ acc_is_infinity.value [
            (* Accumulator is infinity, result is the point to add *)
            acc_x <-- add_point_x.value;
            acc_y <-- add_point_y.value;
            acc_is_infinity <-- gnd;
            sm.set_next Loop_next;
          ] @@ elif (acc_x.value ==: add_point_x.value) [
            (* Same x-coordinate: either same point or inverse *)
            let sum_y = acc_y.value +: add_point_y.value in
            (* Simplified check: if y values sum to >= p, they're inverses mod p *)
            (* More correct: should do modular add and check for 0 *)
            if_ ((sum_y ==: Config.prime_p ()) |: 
                 ((acc_y.value ==: add_point_y.value) &: (is_zero acc_y.value))) [
              (* Points are inverses, result is infinity *)
              acc_x <-- zero Config.width;
              acc_y <-- zero Config.width;
              acc_is_infinity <-- vdd;
              sm.set_next Loop_next;
            ] [
              (* Same point, use doubling *)
              dbl_start <-- vdd;
              dbl_xP <-- acc_x.value;
              dbl_yP <-- acc_y.value;
              sm.set_next Wait_double;
            ];
          ] [
            (* Different points, use addition *)
            add_start <-- vdd;
            add_xP <-- acc_x.value;
            add_yP <-- acc_y.value;
            add_xQ <-- add_point_x.value;
            add_yQ <-- add_point_y.value;
            sm.set_next Wait_add;
          ];
        ];
        
        State.Wait_add, [
          when_ point_add.valid [
            acc_x <-- point_add.xR;
            acc_y <-- point_add.yR;
            acc_is_infinity <-- is_zero point_add.yR;
            sm.set_next Loop_next;
          ];
        ];
        
        (* Move to next bit *)
        State.Loop_next, [
          if_ (bit_idx.value ==:. 0) [
            sm.set_next Final_check;
          ] [
            bit_idx <-- bit_idx.value -:. 1;
            (* Double before checking bits (unless accumulator is infinity) *)
            if_ acc_is_infinity.value [
              sm.set_next Loop_check_bits;
            ] [
              sm.set_next Loop_double;
            ];
          ];
        ];
        
        (* Final verification: x_acc mod n == r *)
        State.Final_check, [
          if_ acc_is_infinity.value [
            (* Result is infinity, signature invalid *)
            valid_sig <-- gnd;
          ] [
            (* Compare x_acc with r *)
            (* Note: should properly reduce x_acc mod n if x_acc >= n *)
            let x_matches_r = acc_x.value ==: r_r.value in
            let x_minus_n = acc_x.value -: order_n in
            let x_mod_n_matches_r = 
              (acc_x.value >=: order_n) &: (x_minus_n ==: r_r.value) 
            in
            valid_sig <-- (x_matches_r |: x_mod_n_matches_r);
          ];
          done_r <-- vdd;
          sm.set_next Done;
        ];
        
        State.Done, [
          done_r <-- vdd;
          when_ i.start [
            done_r <-- gnd;
            valid_sig <-- gnd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];
    
    { O.
      valid_signature = valid_sig.value -- "valid_signature"
    ; done_ = done_r.value -- "done"
    ; busy = ~:(sm.is Idle) -- "busy"
    }
end