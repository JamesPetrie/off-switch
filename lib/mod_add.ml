open Base
open Hardcaml
open Signal

module ModAdd = struct
  module State = struct
    type t =
      | Idle
      | Process_chunk_0
      | Process_chunk_1
      | Process_chunk_2
      | Process_chunk_3
      | Process_chunk_4
      | Process_chunk_5
      | Process_chunk_6
      | Process_chunk_7
      | Final_adjust
    [@@deriving sexp_of, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; a : 'a [@bits Config.width]
      ; b : 'a [@bits Config.width]
      ; modulus : 'a [@bits Config.width]
      ; subtract : 'a
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
    
    let intermediate_width = 257 in
    let chunk_width = 32 in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    let accumulator = Variable.reg spec ~width:intermediate_width in
    let operand_a = Variable.reg spec ~width:intermediate_width in
    let operand_b = Variable.reg spec ~width:intermediate_width in
    let modulus_reg = Variable.reg spec ~width:intermediate_width in
    let carry = Variable.reg spec ~width:1 in
    let is_subtract = Variable.reg spec ~width:1 in
    
    let result = Variable.reg spec ~width:Config.width in
    let valid = Variable.wire ~default:gnd in
    
    let get_chunk signal idx =
      let low_bit = idx * chunk_width in
      let high_bit = Int.min (low_bit + chunk_width - 1) (intermediate_width - 1) in
      sel_bottom (srl signal low_bit) (high_bit - low_bit + 1)
    in
    
    let set_chunk_in_acc acc_val chunk_val idx =
      let low_bit = idx * chunk_width in
      let chunk_size = if idx = 7 then 33 else chunk_width in
      let mask_low = uresize (ones chunk_size) intermediate_width in
      let mask = sll mask_low low_bit in
      let chunk_extended = sll (uresize chunk_val intermediate_width) low_bit in
      (acc_val &: ~:(mask)) |: chunk_extended
    in
    
    let process_chunk_logic idx next_state =
      let chunk_a = get_chunk operand_a.value idx in
      let chunk_b = get_chunk operand_b.value idx in
      let actual_chunk_width = if idx = 7 then 33 else chunk_width in
      
      let sum_with_carry = 
        (uresize chunk_a (actual_chunk_width + 1)) +:
        (uresize chunk_b (actual_chunk_width + 1)) +:
        (uresize carry.value (actual_chunk_width + 1))
      in
      
      let result_chunk = sel_bottom sum_with_carry actual_chunk_width in
      let new_carry = if idx = 7 then gnd else bit sum_with_carry actual_chunk_width in
      
      [
        accumulator <-- set_chunk_in_acc accumulator.value result_chunk idx;
        carry <-- new_carry;
        sm.set_next next_state;
      ]
    in

    compile [
      sm.switch [
        State.Idle, [
          when_ i.start [
            operand_a <-- uresize i.a intermediate_width;
            modulus_reg <-- uresize i.modulus intermediate_width;
            
            if_ i.subtract [
              let b_inverted_256 = ~:(i.b) in
              let b_inverted_257 = uresize b_inverted_256 intermediate_width in
              proc [
                operand_b <-- b_inverted_257;
                carry <-- vdd;
              ];
            ] [
              proc [
                operand_b <-- uresize i.b intermediate_width;
                carry <-- gnd;
              ];
            ];
            
            accumulator <-- zero intermediate_width;
            is_subtract <-- i.subtract;
            sm.set_next Process_chunk_0;
          ];
        ];

        State.Process_chunk_0, process_chunk_logic 0 Process_chunk_1;
        State.Process_chunk_1, process_chunk_logic 1 Process_chunk_2;
        State.Process_chunk_2, process_chunk_logic 2 Process_chunk_3;
        State.Process_chunk_3, process_chunk_logic 3 Process_chunk_4;
        State.Process_chunk_4, process_chunk_logic 4 Process_chunk_5;
        State.Process_chunk_5, process_chunk_logic 5 Process_chunk_6;
        State.Process_chunk_6, process_chunk_logic 6 Process_chunk_7;
        State.Process_chunk_7, process_chunk_logic 7 Final_adjust;
        
        State.Final_adjust, [
          let acc_bit_256 = msb accumulator.value in
          let sub_is_negative = ~:acc_bit_256 in
          let sub_corrected = accumulator.value +: modulus_reg.value in
          let add_reduced = accumulator.value -: modulus_reg.value in
          let add_is_negative = msb add_reduced in
          
          let final_result = 
            mux2 is_subtract.value
              (mux2 sub_is_negative
                (sel_bottom sub_corrected Config.width)
                (sel_bottom accumulator.value Config.width))
              (mux2 add_is_negative 
                (sel_bottom accumulator.value Config.width)
                (sel_bottom add_reduced Config.width))
          in
          
          proc [
            result <-- final_result;
            valid <-- vdd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    }
end
