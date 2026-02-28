open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations *)
module Config = struct
  let width = 256
end

(* Combinatorial Addition/Subtraction Module

   Performs a ± b as a raw Config.width-bit operation (no modular reduction).
   - Pure combinatorial logic, no clock required
   - Uses (width+1)-bit intermediate to capture carry/borrow
   - The MSB of the (width+1)-bit sum acts as a 2's complement sign bit:
     add: MSB=1 means overflow; sub: MSB=1 means borrow (result negative)
*)
module CombAdd = struct
  module I = struct
    type 'a t =
      { a        : 'a [@bits Config.width]  (* First operand *)
      ; b        : 'a [@bits Config.width]  (* Second operand *)
      ; subtract : 'a                       (* 1 = subtract (a - b), 0 = add (a + b) *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result    : 'a [@bits Config.width]  (* Lower width bits of the result *)
      ; carry_out : 'a                       (* Add: 1 = overflow; Sub: 1 = borrow (result negative) *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create _scope (i : _ I.t) =
    let w = Config.width + 1 in

    (* Zero-extend first, then invert for subtraction.
       The (width+1)-bit result is a 2's complement signed value:
       its MSB is 0 for non-negative, 1 for negative (borrow). *)
    let b_ext = uresize i.b w in
    let b_eff = mux2 i.subtract ~:b_ext b_ext in
    let cin   = i.subtract in

    let sum = (uresize i.a w) +: b_eff +: (uresize cin w) in

    { O.result    = sel_bottom sum Config.width
    ; carry_out   = msb sum
    }
end
