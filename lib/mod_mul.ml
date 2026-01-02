open Base
open Hardcaml
open Signal

module ModMul = struct
  module State = struct
    type t =
      | Idle
      | Shifting
      | Done
    [@@deriving sexp_of, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; x : 'a [@bits Config.width]
      ; y : 'a [@bits Config.width]
      ; modulus : 'a [@bits Config.width]
      ; num_bits : 'a [@bits 9]
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
    
    let extended_width = Config.width + 1 in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    let accumulator = Variable.reg spec ~width:extended_width in
    let shifted_x = Variable.reg spec ~width:extended_width in
    let y_reg = Variable.reg spec ~width:Config.width in
    let modulus_reg = Variable.reg spec ~width:extended_width in
    let bit_counter = Variable.reg spec ~width:9 in
    
    let result = Variable.reg spec ~width:Config.width in
    let valid = Variable.wire ~default:gnd in
    
    compile [
      sm.switch [
        State.Idle, [
          when_ i.start [
            accumulator <-- zero extended_width;
            shifted_x <-- uresize i.x extended_width;
            y_reg <-- i.y;
            modulus_reg <-- uresize i.modulus extended_width;
            bit_counter <-- zero 9;
            sm.set_next Shifting;
          ];
        ];
        
        State.Shifting, [
          let current_bit = bit y_reg.value 0 in
          
          let acc_plus_x = accumulator.value +: shifted_x.value in
          let new_acc_before_mod = mux2 current_bit acc_plus_x accumulator.value in
          let new_acc = mux2 (new_acc_before_mod >=: modulus_reg.value)
            (new_acc_before_mod -: modulus_reg.value)
            new_acc_before_mod
          in
          
          let shifted_x_2 = sll shifted_x.value 1 in
          let new_shifted_x = mux2 (shifted_x_2 >=: modulus_reg.value)
            (shifted_x_2 -: modulus_reg.value)
            shifted_x_2
          in
          
          proc [
            accumulator <-- new_acc;
            shifted_x <-- new_shifted_x;
            y_reg <-- srl y_reg.value 1;
            bit_counter <-- bit_counter.value +:. 1;
            
            when_ (bit_counter.value ==: (i.num_bits -:. 1)) [
              result <-- sel_bottom new_acc Config.width;
              valid <-- vdd;
              sm.set_next Idle;
            ];
          ];
        ];
        
        State.Done, [];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    }
end
