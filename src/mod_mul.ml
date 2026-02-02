open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations *)
module Config = struct
  let width = 256
end

(* Simple Modular Multiplication
   
   Computes (x * y) mod r where r is the modulus.
   
   Uses the standard shift-and-add algorithm with modular reduction at each step. *)
module ModMul = struct
  module State = struct
    type t =
      | Idle
      | Init
      | Loop
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
      ; num_bits : 'a [@bits 9]  (* Number of bits to process in y (0-256) *)
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

    (* Need one extra bit to handle doubling without overflow *)
    let acc_width = width + 1 in

    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    (* State machine *)
    let sm = State_machine.create (module State) spec ~enable:vdd in

    (* Result accumulator *)
    let result_acc = Variable.reg spec ~width:acc_width in

    (* Multiplier (y) and bit counter *)
    let multiplier = Variable.reg spec ~width in
    let bit_count = Variable.reg spec ~width:9 in

    (* Current value of x (gets doubled each iteration) *)
    let x_current = Variable.reg spec ~width:acc_width in
    
    (* Stored modulus *)
    let modulus_orig = Variable.reg spec ~width:acc_width in
    let num_bits_orig = Variable.reg spec ~width:9 in

    (* Output registers *)
    let result = Variable.reg spec ~width in
    let valid = Variable.reg spec ~width:1 in

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
              x_current <-- uresize i.x acc_width;
              modulus_orig <-- uresize i.modulus acc_width;
              multiplier <-- i.y;
              num_bits_orig <-- i.num_bits;
              sm.set_next Init;
            ];
          ];
        ];

        State.Init, [
          valid <-- gnd;
          result_acc <-- zero acc_width;
          bit_count <-- of_int ~width:9 0;
          sm.set_next Loop;
        ];

        State.Loop, [
          valid <-- gnd;
          
            if_ ((bit_count.value ==: num_bits_orig.value) |: (multiplier.value ==:. 0)) [
    (* Exit when done OR when no more bits to process *)
    result <-- sel_bottom result_acc.value width;
    valid <-- vdd;
    sm.set_next Done;
  ] [
            (* Check LSB of multiplier *)
            let current_bit = lsb multiplier.value in
            
            (* If bit is set, add current x to result *)
            let after_add = 
              mux2 current_bit
                (result_acc.value +: x_current.value)
                result_acc.value
            in
            
            (* Reduce if >= modulus   
              todo: improve efficiency by subtracting 
            first and check underflow as mux condition*)
            let after_add_reduce =
              mux2 (after_add >=: modulus_orig.value)
                (after_add -: modulus_orig.value)
                after_add
            in
            
            (* Double x for next iteration *)
            let x_doubled = sll x_current.value 1 in
            
            (* Reduce doubled x if >= modulus 
              todo: improve efficiency by subtracting 
            first and check underflow as mux condition*)

            let x_doubled_reduce =
              mux2 (x_doubled >=: modulus_orig.value)
                (x_doubled -: modulus_orig.value)
                x_doubled
            in
            
            proc [
              result_acc <-- after_add_reduce;
              x_current <-- x_doubled_reduce;
              multiplier <-- srl multiplier.value 1;  (* Shift right *)
              bit_count <-- bit_count.value +:. 1;
            ];
          ];
        ];

        State.Done, [
  valid <-- vdd;
  sm.set_next Idle;  (* Return to Idle after one cycle *)
];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    }
end

