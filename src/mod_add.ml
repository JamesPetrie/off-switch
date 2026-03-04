open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations *)
module Config = struct
  let width = 256
end

(* Modular Addition/Subtraction Module

   Performs (a ± b) mod n where n is provided as an input.
   - Uses an internal comb_add for arithmetic
   - 2-cycle operation: Add (latch + add) → Adjust (modular reduction)
   - Automatic modular reduction
*)
module ModAdd = struct
  module State = struct
    type t =
      | Add
      | Adjust
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; a : 'a [@bits Config.width]  (* First operand *)
      ; b : 'a [@bits Config.width]  (* Second operand *)
      ; modulus : 'a [@bits Config.width]  (* Modulus n *)
      ; subtract : 'a  (* 1 = subtract, 0 = add *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result   : 'a [@bits Config.width]
      ; ready    : 'a  (* 1 result is valid *)
      ; adjusted : 'a  (* 1 if modular correction was applied this cycle, valid with ready *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in

    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    (* State machine *)
    let sm = State_machine.create (module State) spec ~enable:vdd in

    (* Registers for computation *)
    let result_ab    = Variable.reg spec ~width:Config.width in
    let carry_ab     = Variable.reg spec ~width:1 in

    (* Adder inputs muxed by state:
       - Add:    i.a ± i.b           (initial operation)
       - Adjust: result_ab ± modulus (correction step) *)
    let in_adjust = sm.is Adjust in
    let adder_a        = mux2 in_adjust result_ab.value i.a in
    let adder_b        = mux2 in_adjust i.modulus i.b in
    (* modulus adjust uses the opposite of the original operation - add if a-b, subtract if a+b *)
    let adder_subtract = mux2 in_adjust ~:(i.subtract) i.subtract in

    let adder_ready = vdd in (* currently using combinatorial adder, so no delay *)
    let comb_add_out =
      Comb_add.CombAdd.create (Scope.sub_scope scope "comb_add")
        { Comb_add.CombAdd.I.a = adder_a; b = adder_b; subtract = adder_subtract }
    in

    let result_w   = Variable.wire ~default:(zero Config.width) in
    let ready_w    = Variable.wire ~default:gnd in
    let adjusted_w = Variable.wire ~default:gnd in

    compile [
      sm.switch [
        State.Add, [
          when_ (i.valid &: adder_ready) [
            (* update register values and select next state *)
            result_ab   <-- comb_add_out.result;
            carry_ab    <-- comb_add_out.carry_out;
            sm.set_next Adjust;
          ];
        ];

        State.Adjust, [

          (* adder is computing result_ab ± modulus here *)
          (* For add:
            -- reduce if carry_ab=1 (a+b overflowed 256 bits, so a+b >= 2^256 > n)
                NOTE: comb_add_out.carry_out is not valid in this case
            -- reduce if carry_ab=0 but subtracting the modulus did not underflow either (a+b did not overflow, but is still >= n) *)
          let add_needs_adjust = carry_ab.value |: ~:(comb_add_out.carry_out) in
          (* For subtract:
              -- reduce if carry_ab=1 (a-b underflowed, so a-b < 0)
                NOTE: comb_add_out.carry_out is not valid in this case *)
          let sub_needs_adjust = carry_ab.value in

          let final_result =
            mux2 i.subtract
              (mux2 sub_needs_adjust comb_add_out.result result_ab.value)
              (mux2 add_needs_adjust comb_add_out.result result_ab.value)
          in

          when_ adder_ready [
            (* combinatorial: drive result output *)
            result_w   <-- final_result;
            ready_w    <-- vdd; (* No clear needed, Variable.wire ~default takes care *)
            adjusted_w <-- mux2 i.subtract sub_needs_adjust add_needs_adjust;
            (* select next state *)
            sm.set_next Add;
          ];
        ];
      ];
    ];

    { O.result   = result_w.value -- "result"
    ; ready      = ready_w.value -- "ready"
    ; adjusted   = adjusted_w.value -- "adjusted"
    }
end

