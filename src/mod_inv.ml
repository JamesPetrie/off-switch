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
*)
module ModInv = struct
  module State = struct
    type t =
      | Idle
      | Op_sel              (* decide which operation to start next *)
      | Div2_xs             (* divide x and s by 2, s might need adjusting *)
      | Div2_yu             (* divide y and u by 2, u might need adjusting *)
      | Sub_rems            (* subtract remainders *)
      | Sub_rems_reverse    (* reverse subtract remainders, we don't know in advance which direction needed *)
      | Sub_coeffs          (* subtract coefficients *)
      | Adjust_coeff        (* adjust the reduced coefficient after subtraction *)
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
      }
    [@@deriving sexp_of, hardcaml]
  end

module O = struct
  type 'a t =
    { result : 'a [@bits Config.width]
    ; valid : 'a
    ; exists : 'a
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

    (* helper reg for the FSM *)
    let reduced_xny = Variable.reg spec ~width:1 in (* 1 = x was reduced (x-y), 0 = y was reduced (y-x) *)
    
    let result = Variable.reg spec ~width in
    let valid = Variable.reg spec ~width:1 in
    let exists = Variable.reg spec ~width:1 in

    (* 256 bit adder instance *)
    let comb_add_a    = Variable.wire ~default:(zero width) in
    let comb_add_b    = Variable.wire ~default:(zero width) in
    let comb_subtract = Variable.wire ~default:gnd in
    let comb_add_out  =
      Comb_add.CombAdd.create (Scope.sub_scope scope "comb_add")
        { Comb_add.CombAdd.I.a = comb_add_a.value;
                             b = comb_add_b.value;
                             subtract = comb_subtract.value }
    in

    (* Compute new coefficient after a div2 step. *)
    (* if c is even:                              *)
    (*   c = c/2                                  *)
    (* else:                                      *)
    (*   c = (c + mod)/2                          *)
    let div2_new_coeff c =
      (* c + mod, using adder instance *)
      let sum_c_mod = comb_add_out.carry_out @: comb_add_out.result in (* sum might be +1 bit wide, use carry *)
      (* * if c is even: c = c/2 else: c = (c + mod)/2 *) 
      mux2 ~:(lsb c) (srl     c         1)
                     (sel_top sum_c_mod width) (* using sel_top instead of srl as sum is +1 bit wide *)
    in

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
          @@ elif (~:(lsb x.value)) [sm.set_next Div2_xs]   (* if x even -> divide x       *)
          @@ elif (~:(lsb y.value)) [sm.set_next Div2_yu]   (* if y even -> divide y       *)
          @@                        [sm.set_next Sub_rems]; (* else -> subtract remainders *)
        ];

        State.Div2_xs, [
          (* x = x/2           *)
          (* if s is even:     *)
          (*   s = s/2         *)
          (* else:             *)
          (*   s = (s + mod)/2 *)

          (* x = x/2 *)
          let new_x = srl x.value 1 in

          (* see div2_new_coeff *)
          let new_s = div2_new_coeff s.value in
          
          proc [
            (* combinatoinal drive the adder inputs *)
            comb_add_a <-- s.value;
            comb_add_b <-- modulus_reg.value;
            comb_subtract <-- gnd;
          
            (* update register values and select next state *)
            x <-- new_x;
            s <-- new_s;
            sm.set_next Op_sel;
          ];
        ];

        State.Div2_yu, [
          (* y = y/2           *)
          (* if u is even:     *)
          (*   u = u/2         *)
          (* else:             *)
          (*   u = (u + mod)/2 *)

          (* y = y/2 *)
          let new_y = srl y.value 1 in

          (* see div2_new_coeff *)
          let new_u = div2_new_coeff u.value in

          proc [
            (* combinatoinal drive the adder inputs *)
            comb_add_a <-- u.value;
            comb_add_b <-- modulus_reg.value;
            comb_subtract <-- gnd;

            (* update register values and select next state *)
            y <-- new_y;
            u <-- new_u;
            sm.set_next Op_sel;
          ];
        ];

        State.Sub_rems, [
          (* if (x >= y):       *)
          (*   x = x - y        *)
          (* else:              *)
          (*   Sub_rems_reverse *)
        
          (* combinatoinal drive the adder inputs *)
          comb_add_a <-- x.value;
          comb_add_b <-- y.value;
          comb_subtract <-- vdd;

          (* update register values and select next state  *)
          if_ ~:(comb_add_out.carry_out) [
            (* carry_out = 0 means no underflow (x was >= y), store results and move to coefficients *)
            x <-- comb_add_out.result;
            reduced_xny <-- vdd; (* Subtract reduced x *)
            (* REVISIT is it okay to skip a state conditionally? *)
            sm.set_next Sub_coeffs;
          ] [
            (* underflow, need Sub_rems_reverse instead *)
            sm.set_next Sub_rems_reverse;
          ];
        ];

        (* Note: y - x, guaranteed no underflow since Sub_rems established x < y *)
        State.Sub_rems_reverse, [
          (* y = y - x *)

          (* combinatoinal drive the adder inputs *)
          comb_add_a <-- y.value;
          comb_add_b <-- x.value;
          comb_subtract <-- vdd;

          (* update register values and select next state  *)
          y <-- comb_add_out.result;
          reduced_xny <-- gnd; (* Subtract reduced y *)
          sm.set_next Sub_coeffs;
        ];

        State.Sub_coeffs, [
          (* _old might have changed by now *)
          (* if x_old >= y_old:             *)
          (*   s = s-u                      *)
          (* else:                          *)
          (*   u = u-s                      *)

          (* combinatorial drive the adder inputs *)
          (* if x was reduced: attempt s - u, else: attempt u - s *)
          comb_add_a <-- mux2 reduced_xny.value s.value u.value;
          comb_add_b <-- mux2 reduced_xny.value u.value s.value;
          comb_subtract <-- vdd;

          (* update register values and select next state  *)
          (* the result is always valid, even if underflowed, Adjust_coeff will fix *)
          if_ reduced_xny.value [
            s <-- comb_add_out.result;
          ] [
            u <-- comb_add_out.result;
          ];
          (* if underflow, go to Adjust_coeff to add modulus *)
          if_ comb_add_out.carry_out [
            sm.set_next Adjust_coeff;
          ] [
            sm.set_next Op_sel;
          ];
        ];

        State.Adjust_coeff, [
          (* _old might have changed by now *)
          (* if x_old >= y_old:             *)
          (*   if s_old < u_old:            *)
          (*      s = s_old - u_old + mod   *)
          (* else:                          *)
          (*   if u_old < s_old:            *)
          (*      u = u_old - s_old + mod   *)

          (* combinatorial drive the adder inputs *)
          comb_add_a <-- mux2 reduced_xny.value s.value u.value;
          comb_add_b <-- modulus_reg.value;
          comb_subtract <-- gnd;

          (* update register values and select next state  *)
          if_ reduced_xny.value [
            s <-- comb_add_out.result;
          ] [
            u <-- comb_add_out.result;
          ];
          sm.set_next Op_sel;
        ];

State.Done, [
  valid <-- gnd;
  sm.set_next Idle;  (* Return to Idle after one cycle *)
];
      ];
    ];

{ O.result = result.value -- "result"
; valid = valid.value -- "valid"
; exists = exists.value -- "exists"
; dbg_x = x.value
; dbg_y = y.value
; dbg_s = s.value
; dbg_u = u.value
}
end