open Base
open Hardcaml
open Signal

module Config = struct
  let width = 256
end

(* Binary Extended GCD for modular inverse computation

   REQUIREMENT: This implementation assumes the modulus is an odd prime.
   - For odd prime modulus and any x coprime to it, gcd(x, modulus) = 1
   - Since modulus is odd, at most one of {x, modulus} can be even
   - Therefore we never need to handle the "both even" case or factor out common powers of 2
   - This simplifies the algorithm significantly compared to the general case

   All arithmetic is driven through an external mod_add instance (shared with add/sub/mul).
*)
module ModInv = struct
  module State = struct
    type t =
      | Idle
      | Op_sel              (* decide which operation to start next *)
      | Div2_add            (* divide appropriate remainder and corresponding coeffiecient by 2, start adjusting the coefficient (c>>1 + mod>>1) if needed *)
      | Div2_p1             (* finish adjusting the coefficient (c=c+1) if needed *)
      | Sub_rems            (* subtract remainders: x - y *)
      | Sub_rems_reverse    (* reverse subtract: y - x (when x < y) *)
      | Sub_coeffs          (* subtract coefficients *)
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; x : 'a [@bits Config.width]
      ; modulus : 'a [@bits Config.width]
      ; mod_add_result   : 'a [@bits Config.width]
      ; mod_add_ready    : 'a
      ; mod_add_adjusted : 'a  (* underflow/overflow flag, valid when mod_add_ready *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits Config.width]
      ; valid : 'a
      ; exists : 'a
      ; mod_add_valid    : 'a
      ; mod_add_a        : 'a [@bits Config.width]
      ; mod_add_b        : 'a [@bits Config.width]
      ; mod_add_subtract : 'a
      (* Debug outputs *)
      ; dbg_x : 'a [@bits Config.width]
      ; dbg_y : 'a [@bits Config.width]
      ; dbg_s : 'a [@bits Config.width]
      ; dbg_u : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    let width = Config.width in

    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    let sm = State_machine.create (module State) spec ~enable:vdd in

    let x = Variable.reg spec ~width in (* remainder *)
    let y = Variable.reg spec ~width in (* remainder *)
    let s = Variable.reg spec ~width in (* coefficient for x *)
    let u = Variable.reg spec ~width in (* coefficient for y *)

    let modulus_reg = Variable.reg spec ~width:width in

    (* helper regs for the FSM *)
    let reduced_xny    = Variable.reg spec ~width:1 in (* 1 = x was reduced (x-y), 0 = y was reduced (y-x) *)
    let div2_xny       = Variable.reg spec ~width:1 in (* 1 = divide x, 0 = divide y *)
    let div2_coeff_odd = Variable.reg spec ~width:1 in (* was original coefficient odd? *)

    let result = Variable.reg spec ~width in
    let valid = Variable.reg spec ~width:1 in
    let exists = Variable.reg spec ~width:1 in

    (* Output wires for driving external mod_add *)
    let mod_add_valid_w    = Variable.wire ~default:gnd in
    let mod_add_a_w        = Variable.wire ~default:(zero Config.width) in
    let mod_add_b_w        = Variable.wire ~default:(zero Config.width) in
    let mod_add_subtract_w = Variable.wire ~default:gnd in

    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          exists <-- gnd;
          when_ i.start [
            if_ (i.x ==:. 0) [
              result <-- zero width;
              exists <-- gnd;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.modulus ==:. 0) [
              result <-- zero width;
              exists <-- gnd;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.x ==:. 1) [
              result <-- of_int ~width:width 1;
              exists <-- vdd;
              valid <-- vdd;
              sm.set_next Done;
            ] [
              x <-- i.x;
              y <-- i.modulus;
              modulus_reg <-- i.modulus;
              s <-- of_int ~width:width 1;
              u <-- zero width;
              sm.set_next Op_sel;
            ];
          ];
        ];

        State.Op_sel, [
          if_ (x.value ==:. 0) [ (* termination: x = 0 means gcd found *)
            (* if y = 1, inverse exists and equal to u, otherwise does not exist *)
            if_ (y.value ==:. 1) [
              exists <-- vdd;
              result <-- u.value;
            ] [
              exists <-- gnd;
              result <-- zero width;
            ];
            valid <-- vdd;
            sm.set_next Done;
          ] (* else, next operation selection below *)
          @@ elif (~:(lsb x.value)) [ (* if x even -> divide x (xs pair) *)
            div2_xny <-- vdd;
            div2_coeff_odd <-- lsb s.value; (* needed in Div2_p1 *)
            sm.set_next Div2_add;
          ]
          @@ elif (~:(lsb y.value)) [ (* if y even -> divide y (yu pair) *)
            div2_xny <-- gnd;
            div2_coeff_odd <-- lsb u.value; (* needed in Div2_p1 *)
            sm.set_next Div2_add;
          ]
          @@ [ (* else -> subtract remainders *)
            sm.set_next Sub_rems
          ];
        ];

        State.Div2_add, [
          (* r = r/2                r = x or y  *)
          (* if c is even:          c = s or u  *)
          (*   c = c/2                          *)
          (* else:                              *)
          (*   c = (c + mod)/2                  *)
          (*                                    *)
          (* CAUTION! (c + mod)/2 has 2 steps:  *)
          (*   1. Div2_add:                     *)
          (*      c = (c >> 1) + (mod >> 1)     *)
          (*   2. Div2_p1:                      *)
          (*      c = c + 1                     *)

          let coeff   = mux2 div2_xny.value s.value u.value in

          proc [
            (* Combinationally drive mod_add: c>>1 + mod>>1 *)
            mod_add_valid_w    <-- vdd;
            mod_add_a_w        <-- srl coeff 1;
            mod_add_b_w        <-- srl modulus_reg.value 1;
            mod_add_subtract_w <-- gnd;

            when_ i.mod_add_ready [
              (* Shift the remainder (fires exactly once) *)
              if_ div2_xny.value [
                x <-- srl x.value 1;
                s <-- mux2 (lsb s.value) i.mod_add_result (srl s.value 1);
              ] [
                y <-- srl y.value 1;
                u <-- mux2 (lsb u.value) i.mod_add_result (srl u.value 1);
              ];
              div2_coeff_odd <-- lsb coeff; (* needed for Div2_p1 *)
              sm.set_next Div2_p1;
            ];
          ];
        ];

        State.Div2_p1, [
          (* see Div2_add for what this state implements *)

          proc [
            (* Combinationally drive mod_add: c + 1 *)
            mod_add_valid_w    <-- vdd;
            mod_add_a_w        <-- mux2 div2_xny.value s.value u.value;
            mod_add_b_w        <-- of_int ~width:Config.width 1;
            mod_add_subtract_w <-- gnd;

            when_ i.mod_add_ready [
              if_ div2_xny.value [
                s <-- mux2 div2_coeff_odd.value i.mod_add_result s.value;
              ] [
                u <-- mux2 div2_coeff_odd.value i.mod_add_result u.value;
              ];
              sm.set_next Op_sel;
            ];
          ];
        ];

        State.Sub_rems, [
          (* if (x >= y):       *)
          (*   x = x - y        *)
          (* else:              *)
          (*   Sub_rems_reverse *)

          (* combinatoinal drive the adder inputs *)
          mod_add_valid_w    <-- vdd;
          mod_add_a_w        <-- x.value;
          mod_add_b_w        <-- y.value;
          mod_add_subtract_w <-- vdd;

          when_ i.mod_add_ready [
            if_ ~:(i.mod_add_adjusted) [
              (* No underflow: x >= y, result is x - y *)
              x           <-- i.mod_add_result;
              reduced_xny <-- vdd; (* x was reduced *)
              sm.set_next Sub_coeffs;
            ] [
              (* Underflow: x < y, need reverse subtraction *)
              sm.set_next Sub_rems_reverse;
            ];
          ];
        ];

        (* Note: y - x, guaranteed no underflow since Sub_rems established x < y *)
        State.Sub_rems_reverse, [
          (* y = y - x *)

          (* combinatoinal drive the adder inputs *)
          mod_add_valid_w    <-- vdd;
          mod_add_a_w        <-- y.value;
          mod_add_b_w        <-- x.value;
          mod_add_subtract_w <-- vdd;

          when_ i.mod_add_ready [
            y           <-- i.mod_add_result;
            reduced_xny <-- gnd; (* y was reduced *)
            sm.set_next Sub_coeffs;
          ];
        ];

        State.Sub_coeffs, [
          (* _old might have changed by now *)
          (* if x_old >= y_old:             *)
          (*   s = s-u                      *)
          (* else:                          *)
          (*   u = u-s                      *)

          (* combinatorial drive the adder inputs *)
          (* if x was reduced: do s - u, else: u - s *)
          mod_add_valid_w    <-- vdd;
          mod_add_a_w        <-- mux2 reduced_xny.value s.value u.value;
          mod_add_b_w        <-- mux2 reduced_xny.value u.value s.value;
          mod_add_subtract_w <-- vdd;

          when_ i.mod_add_ready [
            if_ reduced_xny.value [
              s <-- i.mod_add_result;
            ] [
              u <-- i.mod_add_result;
            ];
            sm.set_next Op_sel;
          ];
        ];

        State.Done, [
          valid <-- gnd;
          sm.set_next Idle;  (* Return to Idle after one cycle *)
        ];
      ];
    ];

    { O.result           = result.value -- "result"
    ; valid              = valid.value -- "valid"
    ; exists             = exists.value -- "exists"
    ; mod_add_valid      = mod_add_valid_w.value -- "mod_add_valid"
    ; mod_add_a          = mod_add_a_w.value -- "mod_add_a"
    ; mod_add_b          = mod_add_b_w.value -- "mod_add_b"
    ; mod_add_subtract   = mod_add_subtract_w.value -- "mod_add_subtract"
    ; dbg_x              = x.value
    ; dbg_y              = y.value
    ; dbg_s              = s.value
    ; dbg_u              = u.value
    }
end
