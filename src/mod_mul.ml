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
   
   Uses the standard shift-and-add algorithm with modular reduction at each step. *)
module ModMul = struct
  module State = struct
    type t =
      | Idle
      | Init
      | Add
      | Adjust
      | Double_adjust
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
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits Config.width]  (* (x * y) mod r *)
      ; valid : 'a  (* High when result is valid *)
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
    let add_carry = Variable.reg spec ~width:1 in

    (* Multiplier (y) and bit counter *)
    let multiplier = Variable.reg spec ~width in
    let bit_count = Variable.reg spec ~width:bit_count_width in

    (* Current value of x (gets doubled each iteration) *)
    let x_current = Variable.reg spec ~width:width in
    
    (* Stored modulus *)
    let modulus_reg = Variable.reg spec ~width:width in
    let num_bits_orig = Variable.reg spec ~width:bit_count_width in

    (* Output registers *)
    let result = Variable.reg spec ~width in
    let valid = Variable.reg spec ~width:1 in

    (* 256 bit adder instance *)
    let in_add_state        = sm.is Add in
    let in_double_adj_state = sm.is Double_adjust in
    let x_doubled           = sll x_current.value 1 in
    
    let comb_add_a    = mux2 ~:in_double_adj_state result_acc.value x_doubled         in
    let comb_add_b    = mux2   in_add_state        x_current.value  modulus_reg.value in
    let comb_subtract = mux2   in_add_state        gnd              vdd               in
    let comb_add_out  =
      Comb_add.CombAdd.create (Scope.sub_scope scope "comb_add")
        { Comb_add.CombAdd.I.a = comb_add_a; b = comb_add_b; subtract = comb_subtract }
    in

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
              modulus_reg <-- i.modulus;
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
          (* Check LSB of multiplier *)
          let current_bit = lsb multiplier.value in

          (* If bit is set, add current x to result *)
          let after_add = mux2 current_bit comb_add_out.result result_acc.value in

          proc [
            valid <-- gnd;
            result_acc <-- after_add;
            add_carry  <-- mux2 current_bit comb_add_out.carry_out gnd;
            sm.set_next Adjust;
          ];
        ];

        State.Adjust, [

          (* Reduce if >= modulus:
              -- if the addition had overflow (add_carry=1)
              -- if subtracting the modulus does not underflow (comb_add_out.carry_out=0) *)
          let add_reduce_needed = add_carry.value |: ~:(comb_add_out.carry_out) in
          let after_add_reduce = mux2 add_reduce_needed comb_add_out.result result_acc.value in
          
          proc [
            valid <-- gnd;  
            result_acc <-- after_add_reduce;
            add_carry  <-- gnd;  (* Clear carry as it is N/A here *)
            sm.set_next Double_adjust;
          ];
        ];

        State.Double_adjust, [

          (* Double x for next iteration and
              reduce new x if >= modulus:
              -- if the doubling caused overflow (MSB=1)
              -- if subtracting the modulus does not underflow (comb_add_out.carry_out=0) *)
          let double_reduce_needed = msb x_current.value |: ~:(comb_add_out.carry_out) in
          let new_x = mux2 double_reduce_needed comb_add_out.result x_doubled in
          let new_multiplier = srl multiplier.value 1 in
          let new_bit_count = bit_count.value +:. 1 in
          
          proc [
            valid <-- gnd;
            x_current <-- new_x;
            multiplier <-- new_multiplier;
            bit_count <-- new_bit_count;

            if_ ((new_bit_count ==: num_bits_orig.value) |: (new_multiplier ==:. 0)) [
              (* Exit when done OR when no more bits to process *)
              result <-- result_acc.value;
              valid <-- vdd;
              sm.set_next Done;
            ] (* REVISIT is it okay to just skip Add and Adjust if LSB=0? *)
            @@ elif ( ~:(lsb new_multiplier) ) [
              (* Skip Add if next bit is 0 *)
              sm.set_next Double_adjust;
            ]
            @@ [
              sm.set_next Add;
            ];
          ];
        ];

        State.Done, [
          valid <-- gnd;
          sm.set_next Idle;  (* Return to Idle after one cycle *)
        ];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    }
end

