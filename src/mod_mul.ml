open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations *)
module Config = struct
  let width = 256
  let bit_count_width = num_bits_to_represent width
end

(* Simple Modular Multiplication

   Computes (x * y) mod r where r is the modulus.

   Uses the standard shift-and-add algorithm with modular reduction at each step.
   Drives an external mod_add instance for all arithmetic. *)
module ModMul = struct
  module State = struct
    type t =
      | Idle
      | Init
      | Add
      | Double
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a  (* Pulse high to begin computation *)
      ; x : 'a [@bits Config.width]  (* First multiplicand *)
      ; y : 'a [@bits Config.width]  (* Second multiplicand *)
      ; modulus : 'a [@bits Config.width]  (* Modulus r *)
      ; num_bits : 'a [@bits Config.bit_count_width]  (* Number of bits to process in y (0-256) *)
      ; mod_add_result : 'a [@bits Config.width]  (* result from external mod_add *)
      ; mod_add_ready  : 'a                        (* ready from external mod_add *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits Config.width]  (* (x * y) mod r *)
      ; valid : 'a  (* High when result is valid *)
      ; mod_add_valid    : 'a
      ; mod_add_a        : 'a [@bits Config.width]
      ; mod_add_b        : 'a [@bits Config.width]
      ; mod_add_subtract : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    let width = Config.width in
    let bit_count_width = Config.bit_count_width in

    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    (* State machine *)
    let sm = State_machine.create (module State) spec ~enable:vdd in

    (* Result accumulator *)
    let result_acc = Variable.reg spec ~width:width in

    (* Multiplier (y) and bit counter *)
    let multiplier = Variable.reg spec ~width in
    let bit_count = Variable.reg spec ~width:bit_count_width in

    (* Current value of x (gets doubled each iteration) *)
    let x_current = Variable.reg spec ~width:width in

    let num_bits_orig = Variable.reg spec ~width:bit_count_width in

    (* Output registers *)
    let result = Variable.reg spec ~width in
    let valid = Variable.reg spec ~width:1 in

    (* Output wires for driving external mod_add *)
    let mod_add_valid_w    = Variable.wire ~default:gnd in
    let mod_add_a_w        = Variable.wire ~default:(zero Config.width) in
    let mod_add_b_w        = Variable.wire ~default:(zero Config.width) in
    let mod_add_subtract_w = Variable.wire ~default:gnd in

    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          when_ i.start [
            if_ ((i.x ==:. 0) |: (i.y ==:. 0)) [
              result <-- zero width;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.modulus <=:. 1) [
              result <-- zero width;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.num_bits ==:. 0) [
              (* Zero bits means result is 0 *)
              result <-- zero width;
              valid <-- vdd;
              sm.set_next Done;
            ] [
              x_current <-- i.x;
              multiplier <-- i.y;
              num_bits_orig <-- i.num_bits;
              sm.set_next Init;
            ];
          ];
        ];

        State.Init, [
          valid <-- gnd;
          result_acc <-- zero width;
          bit_count <-- zero bit_count_width;
          sm.set_next Add;
        ];

        State.Add, [
          if_ (lsb multiplier.value) [
            (* Combinatorially drive mod_add *)
            mod_add_valid_w    <-- vdd;
            mod_add_a_w        <-- result_acc.value;
            mod_add_b_w        <-- x_current.value;
            mod_add_subtract_w <-- gnd;

            when_ i.mod_add_ready [
              result_acc <-- i.mod_add_result;
              sm.set_next Double;
            ];
          ] [
            sm.set_next Double;  (* LSB = 0: skip addition *)
          ];
        ];

        State.Double, [
          (* Combinatorially drive mod_add: x_current + x_current (double) *)
          mod_add_valid_w    <-- vdd;
          mod_add_a_w        <-- x_current.value;
          mod_add_b_w        <-- x_current.value;
          mod_add_subtract_w <-- gnd;

          when_ i.mod_add_ready [
            let new_multiplier = srl multiplier.value 1 in
            let new_bit_count  = bit_count.value +:. 1 in
            proc [
              x_current  <-- i.mod_add_result;
              multiplier <-- new_multiplier;
              bit_count  <-- new_bit_count;

              if_ ((new_bit_count ==: num_bits_orig.value) |: (new_multiplier ==:. 0)) [
                result <-- result_acc.value;
                valid  <-- vdd;
                sm.set_next Done;
              ] @@ elif (~:(lsb new_multiplier)) [
                sm.set_next Double;   (* skip Add when next bit = 0 *)
              ] @@ [
                sm.set_next Add;
              ];
            ];
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
    ; mod_add_valid      = mod_add_valid_w.value -- "mod_add_valid"
    ; mod_add_a          = mod_add_a_w.value -- "mod_add_a"
    ; mod_add_b          = mod_add_b_w.value -- "mod_add_b"
    ; mod_add_subtract   = mod_add_subtract_w.value -- "mod_add_subtract"
    }
end
