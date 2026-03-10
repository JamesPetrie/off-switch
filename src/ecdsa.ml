open Base
open Hardcaml
open Signal


(* ECDSA Signature Verification for secp256k1

   Verifies ECDSA signatures using the equation:
     R = u1*G + u2*Q
   where:
     u1 = z * s^(-1) mod n
     u2 = r * s^(-1) mod n

   Signature is valid if R.x mod n == r

    Uses Renes, Costello and Batina's complete addition formula in projective coordinates

    Uses the Arith module for modular {add, sub, mul, inv}.

   Inputs:
     - z: message hash (256 bits)
     - r, s: signature components (256 bits each)
     - param_a, param_b3: curve parameters (a=0, b3=21 for secp256k1)

   Outputs:
     - valid: 1 if signature is valid, 0 otherwise
     - done_: pulses high for one cycle when verification completes
     - busy: high while verification is in progress
     - x, y, z_out: final point coordinates (for debugging)

   Hardcoded:
     - G: generator point
     - Q: public key (currently 2G for testing)
     - G+Q: precomputed sum (currently 3G)

   Note: Does not currently reduce x_affine mod n before comparison,
   or check that z, s, r are within range.

*)

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

  (* Point at infinity (z = 0) *)
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
    ; valid : 'a                        (* signature is valid *)
    ; x : 'a [@bits Config.width]
    ; y : 'a [@bits Config.width]
    ; z_out : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
    | Run_prep
    | Sample_prep (* Separate step to sample the u1, u2 values when visible in the register file *)
    | Loop
    | Run_point_add
    | Run_finalize
    | Done (* Separate step to sample the result when visible in the register file *)
  [@@deriving sexp_of, compare, enumerate]
end

type instr = { op : int; src1 : int; src2 : int; dst : int }

(* Implements:               *)
(*    w = s^(-1) mod n       *)
(*    u1 = z * w mod n       *)
(*    u2 = r * w mod n       *)
(*                           *)
(* assumes t0=s, t1=z, t2=r  *)
(* u1 placed in t1, u2 in t2 *)
let program_prepare = [|
  { op = Op.inv; src1 = Config.t0; src2 = Config.t0; dst = Config.t0 };
  { op = Op.mul; src1 = Config.t1; src2 = Config.t0; dst = Config.t1 };
  { op = Op.mul; src1 = Config.t2; src2 = Config.t0; dst = Config.t2 };
|]

let program_point_add = [|
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

(* Implements:                            *)
(*    z_inv = Z1^(-1) mod p               *)
(*    x_affine = X1 * z_inv mod p         *)
(*    result = x_affine - r mod p         *)
(*                                        *)
(* assumes t2=r - consistent with prepare *)
(* result placed in t0                    *)
(* if result == 0, then x_affine == r     *)
let program_finalize = [|
  { op = Op.inv; src1 = Config.z1; src2 = Config.z1; dst = Config.t0 };
  { op = Op.mul; src1 = Config.x1; src2 = Config.t0; dst = Config.t0 };
  { op = Op.sub; src1 = Config.t0; src2 = Config.t2; dst = Config.t0 };
|]

let program = Array.concat [program_prepare; program_point_add; program_finalize]

(* Segment boundaries — compile-time constants *)
let prepare_start   = 0
let prepare_end     = Array.length program_prepare - 1
let point_add_start = Array.length program_prepare
let point_add_end   = point_add_start + Array.length program_point_add - 1
let finalize_start  = point_add_end + 1
let finalize_end    = finalize_start + Array.length program_finalize - 1

(* Helper to extract a bit from a signal using a signal index *)
let bit_select_dynamic signal index_signal =
  let width = Signal.width signal in
  let bits = List.init width ~f:(fun i -> bit signal i) in
  mux index_signal bits

let create scope (i : _ I.t) =
  let open Always in
  let width = Config.width in
  let bit_cnt_width = Int.ceil_log2 width in
  let addr_width = Config.reg_addr_width in

  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in

  let reg_file = Array.init Config.num_regs ~f:(fun _ -> Variable.reg spec ~width) in

  (* Hardcoded point constants *)
  let const_infx = of_z ~width Config.infinity_x in
  let const_infy = of_z ~width Config.infinity_y in
  let const_infz = of_z ~width Config.infinity_z in

  let const_gx = of_z ~width Config.generator_x in
  let const_gy = of_z ~width Config.generator_y in
  let const_gz = of_z ~width Config.generator_z in

  let const_qx = of_z ~width Config.q_x in
  let const_qy = of_z ~width Config.q_y in
  let const_qz = of_z ~width Config.q_z in

  let const_gpqx = of_z ~width Config.gpq_x in
  let const_gpqy = of_z ~width Config.gpq_y in
  let const_gpqz = of_z ~width Config.gpq_z in

  (* Input register *)
  let r_reg = Variable.reg spec ~width in

  (* Scalar registers — captured at end of Prep_op, read throughout Loop *)
  let u1_reg = Variable.reg spec ~width in     (* z * s^(-1) mod n *)
  let u2_reg = Variable.reg spec ~width in     (* r * s^(-1) mod n *)

  (* Unified program counter *)
  let pc = Variable.reg spec ~width:6 in

  (* Main loop registers *)
  let bit_pos = Variable.reg spec ~width:bit_cnt_width in
  let doubling = Variable.reg spec ~width:1 in
  let last_step = Variable.reg spec ~width:1 in

  (* Output wires *)
  let done_w  = Variable.wire ~default:gnd in
  let valid_w = Variable.wire ~default:gnd in

  let current_bit_u1 = bit_select_dynamic u1_reg.value bit_pos.value in
  let current_bit_u2 = bit_select_dynamic u2_reg.value bit_pos.value in

  (* Unified instruction decode over full program *)
  let pc_decode field w =
    mux pc.value
      (Array.to_list (Array.map program ~f:(fun instr ->
         of_int ~width:w (field instr))))
  in
  let current_src1 = pc_decode (fun instr -> instr.src1) addr_width in
  let current_src2 = pc_decode (fun instr -> instr.src2) addr_width in
  let current_dst  = pc_decode (fun instr -> instr.dst)  addr_width in
  let current_op   = pc_decode (fun instr -> instr.op)   2 in

  (* Use latched values for x2/y2/z2 in point addition *)
  let reg_read idx =
    let base_values = Array.to_list (Array.map reg_file ~f:(fun r -> r.value)) in
    mux idx base_values
  in

  let arith_prime_sel_selected = (pc.value >=:. prepare_start) &&: (pc.value <=:. prepare_end) in  (* mod n for prepare *)
  let arith_read_data_a = reg_read current_src1 in
  let arith_read_data_b = reg_read current_src2 in

  let arith_out = Arith.create (Scope.sub_scope scope "arith")
    { Arith.I.
      clock = i.clock
    ; clear = i.clear
    ; valid = sm.is (State.Run_prep) ||: sm.is State.Run_point_add ||: sm.is State.Run_finalize
    ; op = current_op
    ; prime_sel = arith_prime_sel_selected
    ; reg_read_data_a = arith_read_data_a
    ; reg_read_data_b = arith_read_data_b
    }
  in

  compile [
    (* default value of output wires, should be redundant with default value specified in the declaration but just in case *)
    done_w  <-- gnd;
    valid_w <-- gnd;

    (* in either state, if an arithmetic operation completed, store the result and increment the PC *)
    when_ arith_out.ready [
      pc <-- pc.value +:. 1;
      proc (Array.to_list (Array.mapi reg_file ~f:(fun idx reg ->
        when_ (current_dst ==:. idx) [
          reg <-- arith_out.reg_write_data ])));
    ];

    sm.switch [
      State.Idle, [
        when_ i.start [
          (* capture r value to be used in Finalize state *)
          r_reg <-- i.r;

          reg_file.(Config.param_a)  <-- i.param_a;
          reg_file.(Config.param_b3) <-- i.param_b3;

          (* Initialize and start the Prepare calculations *)
          reg_file.(Config.t0) <-- i.s;   (* see program_prepare assumptions *)
          reg_file.(Config.t1) <-- i.z;   (* see program_prepare assumptions *)
          reg_file.(Config.t2) <-- i.r;   (* see program_prepare assumptions *)
          pc <--. prepare_start;
          sm.set_next Run_prep;
        ];
      ];

      State.Run_prep, [
        (* wait for program to finish and proceed to next state *)
        when_ (arith_out.ready &&: (pc.value ==:. prepare_end)) [
          sm.set_next Sample_prep;
        ];
      ];

      State.Sample_prep, [
        (* Capture u1/u2 only AFTER! the program has finished - u2 is not available in the register earlier! *)
        u1_reg <-- reg_file.(Config.t1).value; (* see program_prepare results handling *)
        u2_reg <-- reg_file.(Config.t2).value; (* see program_prepare results handling *)

        (* Initialize main point multiply calculation and move to next state *)
        bit_pos   <-- ones bit_cnt_width;
        doubling  <-- vdd;
        last_step <-- gnd;
        reg_file.(Config.x1) <-- const_infx;
        reg_file.(Config.y1) <-- const_infy;
        reg_file.(Config.z1) <-- const_infz;

        sm.set_next Loop;
      ];

      State.Loop, [
        if_ last_step.value [ (* completion condition *)
          (* Results are stored in x1, y1, z1, finalize will use those *)
          (* Initialize and start finalize calculations *)
          reg_file.(Config.t2) <-- r_reg.value;   (* see program_finalize assumptions *)
          pc <--. finalize_start;
          sm.set_next Run_finalize;
        ]
        @@ elif doubling.value [
          (* In Doubling phase, next will be Adding *)
          doubling <-- gnd;

          (* could skip the doubling if P is infinity, but do it anyway for side-channel resistance *)
          (* Initialize and start point_add program *)
          reg_file.(Config.x2) <-- reg_file.(Config.x1).value;
          reg_file.(Config.y2) <-- reg_file.(Config.y1).value;
          reg_file.(Config.z2) <-- reg_file.(Config.z1).value;
          pc <--. point_add_start;
          sm.set_next Run_point_add;
        ]
        @@ [
          (* In Adding phase, next will be Doubling *)
          doubling <-- vdd;

          last_step <-- (bit_pos.value ==:. 0);
          bit_pos <-- bit_pos.value -:. 1;

          (* could skip the add if both bits are zero, but do it anyway for side-channel resistance *)
          (* Initialize and start point_add program         *)
          (* Shamir's trick: adding G, Q or precomputed G+Q *)
          (* Reminder: R = u1*G + u2*Q                      *)
          reg_file.(Config.x2) <-- mux2 ~:current_bit_u2 (mux2 ~:current_bit_u1 const_infx const_gx) (mux2 ~:current_bit_u1 const_qx const_gpqx);
          reg_file.(Config.y2) <-- mux2 ~:current_bit_u2 (mux2 ~:current_bit_u1 const_infy const_gy) (mux2 ~:current_bit_u1 const_qy const_gpqy);
          reg_file.(Config.z2) <-- mux2 ~:current_bit_u2 (mux2 ~:current_bit_u1 const_infz const_gz) (mux2 ~:current_bit_u1 const_qz const_gpqz);
          pc <--. point_add_start;
          sm.set_next Run_point_add;
        ];
      ];

      State.Run_point_add, [
        (* wait for program to finish and proceed to next iteration *)
        when_ (arith_out.ready &&: (pc.value ==:. point_add_end)) [
          sm.set_next Loop;
        ];
      ];

      State.Run_finalize, [
        (* wait for program to finish and proceed to next state *)
        when_ (arith_out.ready &&: (pc.value ==:. finalize_end)) [
          sm.set_next Done;
        ];
      ];

      State.Done, [
        (* Combinationally assign the done and valid outputs *)
        done_w  <-- vdd;
        (* Check the result only AFTER! the program has finished - result is not available in the register earlier! *)
        valid_w <-- (reg_file.(Config.t0).value ==: zero width); (* see program_finalize result handling *)

        (* Set next state *)
        sm.set_next Idle;
      ];
    ];
  ];

  { O.
    busy = ~:(sm.is Idle)
  ; done_ = done_w.value
  ; valid = valid_w.value
  ; x = reg_file.(Config.x1).value
  ; y = reg_file.(Config.y1).value
  ; z_out = reg_file.(Config.z1).value
  }
