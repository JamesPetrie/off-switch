open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations *)
module Config = struct
  let width = 256
  let chunk_width = 32  (* Process 32 bits per cycle for 8 cycles *)
end

(* Modular Addition/Subtraction Module
   
   Performs (a ± b) mod n where n is provided as an input.
   - Uses 257-bit signed intermediate representation
   - Implements 2's complement subtraction with XOR and carry
   - 8-cycle pipelined operation (mostly 32-bit chunks)
   - Automatic modular reduction
*)
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
      | Done
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
    
    (* 257 bits to handle signed intermediate values *)
    let intermediate_width = 257 in
    let chunk_width = Config.chunk_width in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    
    (* State machine *)
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    (* Registers for computation *)
    let accumulator = Variable.reg spec ~width:intermediate_width in
    let operand_a = Variable.reg spec ~width:intermediate_width in
    let operand_b = Variable.reg spec ~width:intermediate_width in
    let modulus_reg = Variable.reg spec ~width:intermediate_width in  (* Store modulus *)
    let carry = Variable.reg spec ~width:1 in
    let is_subtract = Variable.reg spec ~width:1 in
    
    (* Output registers *)
    let result = Variable.reg spec ~width:Config.width in
    let valid = Variable.reg spec ~width:1 in
    
    (* Helper: extract chunk from signal *)
    let get_chunk signal idx =
      let low_bit = idx * chunk_width in
      let high_bit = Int.min (low_bit + chunk_width - 1) (intermediate_width - 1) in
      sel_bottom (srl signal low_bit) (high_bit - low_bit + 1)
    in
    
    (* Helper: set chunk in accumulator *)
    let set_chunk_in_acc acc_val chunk_val idx =
      let low_bit = idx * chunk_width in
      let chunk_size = 
        if idx = 7 then 33  (* Last chunk is 33 bits (257 - 224) *)
        else chunk_width 
      in
      let mask_low = uresize (ones chunk_size) intermediate_width in
      let mask = sll mask_low low_bit in
      let chunk_extended = sll (uresize chunk_val intermediate_width) low_bit in
      (acc_val &: ~:(mask)) |: chunk_extended
    in
    
    (* Helper to create processing logic for a chunk *)
    let process_chunk_logic idx next_state =
      let chunk_a = get_chunk operand_a.value idx in
      let chunk_b = get_chunk operand_b.value idx in
      
      (* Determine chunk width for this iteration *)
      let actual_chunk_width = 
        if idx = 7 then 33 else chunk_width 
      in
      
      (* Add chunks with carry *)
      let sum_with_carry = 
        (uresize chunk_a (actual_chunk_width + 1)) +:
        (uresize chunk_b (actual_chunk_width + 1)) +:
        (uresize carry.value (actual_chunk_width + 1))
      in
      
      (* Extract result chunk and new carry *)
      let result_chunk = sel_bottom sum_with_carry actual_chunk_width in
      let new_carry = 
        if idx = 7 then gnd  (* No carry from last chunk *)
        else bit sum_with_carry actual_chunk_width
      in
      
      [
        valid <-- gnd;
        accumulator <-- set_chunk_in_acc accumulator.value result_chunk idx;
        carry <-- new_carry;
        sm.set_next next_state;
      ]
    in

    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          when_ i.start [
            (* Initialize: extend inputs to 257 bits *)
            operand_a <-- uresize i.a intermediate_width;
            
            (* Store the modulus extended to 257 bits *)
            modulus_reg <-- uresize i.modulus intermediate_width;
            
            (* For subtraction: invert 256 bits, then extend to 257 bits *)
            if_ i.subtract [
              (* Invert only the 256-bit value, then zero-extend to 257 bits *)
              let b_inverted_256 = ~:(i.b) in
              let b_inverted_257 = uresize b_inverted_256 intermediate_width in
              proc [
                operand_b <-- b_inverted_257;
                carry <-- vdd;  (* Initial carry for 2's complement *)
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
    
    (* Handle immediate restart *)
    if_ i.start [
      (* Start new operation immediately *)
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
    ] [
      sm.set_next Idle;
    ];
  ];
];

State.Done, [
  (* This state is no longer used, but keep for safety *)
  valid <-- gnd;
  sm.set_next Idle;
];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    }
end

