open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations *)
module Config = struct
  let width = 256
end

(* Modular Addition/Subtraction Module (combinatorial adder variant)

   Performs (a ± b) mod n where n is provided as an input.
   - Uses comb_add for the initial a ± b in a single cycle
   - 2-cycle operation: Free (latch + add) → Modulus_adjust (modular reduction)
   - Automatic modular reduction
*)
module ModAdd = struct
  module State = struct
    type t =
      | Free
      | Modulus_adjust
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; a : 'a [@bits Config.width]  (* First operand *)
      ; b : 'a [@bits Config.width]  (* Second operand *)
      ; modulus : 'a [@bits Config.width]  (* Modulus n *)
      ; subtract : 'a  (* 1 = subtract, 0 = add *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits Config.width]
      ; valid : 'a
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
    let modulus_reg  = Variable.reg spec ~width:Config.width in
    let is_subtract  = Variable.reg spec ~width:1 in

    (* Output registers *)
    let result = Variable.reg spec ~width:Config.width in
    let valid  = Variable.reg spec ~width:1 in

    (* Mux comb_add inputs by state:
       - Free:         i.a ± i.b           (initial operation)
       - Modulus_adjust: result_ab ± modulus (correction: add modulus if sub underflow,
                                                subtract modulus if sum higher than modulus) *)
    let in_modulus_adjust = sm.is Modulus_adjust in
    let comb_a   = mux2 in_modulus_adjust result_ab.value i.a in
    let comb_b   = mux2 in_modulus_adjust modulus_reg.value i.b in
    (* modulus adjust uses the opposite of the original operation - add if a-b, subtract if a+b *)
    let comb_subtract = mux2 in_modulus_adjust ~:(is_subtract.value) i.subtract in

    let comb_add_out =
      Comb_add.CombAdd.create (Scope.sub_scope scope "comb_add")
        { Comb_add.CombAdd.I.a = comb_a; b = comb_b; subtract = comb_subtract }
    in

    compile [
      sm.switch [
        State.Free, [
          valid <-- gnd;
          when_ i.start [
            result_ab   <-- comb_add_out.result;
            carry_ab    <-- comb_add_out.carry_out;
            modulus_reg <-- i.modulus;
            is_subtract <-- i.subtract;
            sm.set_next Modulus_adjust;
          ];
        ];
        
        State.Modulus_adjust, [
  
          (* comb_add is computing result_ab ± modulus_reg here. *)
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
            mux2 is_subtract.value
              (mux2 sub_needs_adjust comb_add_out.result result_ab.value)
              (mux2 add_needs_adjust comb_add_out.result result_ab.value)
          in

          proc [
            result <-- final_result;
            valid <-- vdd;
            sm.set_next Free;
          ];
        ];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    }
end

