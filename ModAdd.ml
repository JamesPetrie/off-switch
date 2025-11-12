open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations with a hardcoded prime modulus *)
module Config = struct
  let width = 256
  let chunk_width = 32  (* Process 32 bits per cycle for 8 cycles *)
  
  (* Hardcoded 256-bit prime modulus (secp256k1 prime as example) *)
  (* n = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1 *)
  let prime_modulus_z = 
    Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663"
  
  let prime_modulus_constant () =
    let hex_str = Z.format "%x" prime_modulus_z in
    let padded = String.pad_left hex_str ~len:((width + 3) / 4) ~char:'0' in
    Constant.of_hex_string ~width:width ~signedness:Unsigned padded
end

(* Modular Addition/Subtraction Module
   
   Performs (a ± b) mod n where n is the hardcoded prime modulus.
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
    
    (* Hardcoded modulus as a ROM constant *)
    let modulus_257 = 
      uresize (Signal.of_constant (Config.prime_modulus_constant ())) intermediate_width
    in
    
    (* State machine *)
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    (* Registers for computation *)
    let accumulator = Variable.reg spec ~width:intermediate_width in
    let operand_a = Variable.reg spec ~width:intermediate_width in
    let operand_b = Variable.reg spec ~width:intermediate_width in
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
          (* For subtraction in 257-bit 2's complement:
             After computing a + (~b) + 1 where b's lower 256 bits are inverted:
             - If bit 256 of accumulator is 0, result is negative (a < b), add modulus
             - If bit 256 of accumulator is 1, result is positive (a >= b), use lower 256 bits
             
             For addition:
             - Try subtracting modulus
             - If result MSB is 1 (negative), keep original; else use reduced
          *)
          
          (* For subtraction: check bit 256 of the accumulator *)
          let acc_bit_256 = msb accumulator.value in
          let sub_is_negative = ~:acc_bit_256 in
          
          (* For subtraction: if negative, add modulus *)
          let sub_corrected = accumulator.value +: modulus_257 in
          
          (* For addition: try subtracting modulus *)
          let add_reduced = accumulator.value -: modulus_257 in
          let add_is_negative = msb add_reduced in
          
          (* Select final result based on operation *)
          let final_result = 
            mux2 is_subtract.value
              (* Subtraction: if bit 256 is 0, we need correction *)
              (mux2 sub_is_negative
                (sel_bottom sub_corrected Config.width)
                (sel_bottom accumulator.value Config.width))
              (* Addition: if reduction makes it negative, keep original; else use reduced *)
              (mux2 add_is_negative 
                (sel_bottom accumulator.value Config.width)
                (sel_bottom add_reduced Config.width))
          in
          
          proc [
            result <-- final_result;
            valid <-- vdd;
            sm.set_next Done;
          ];
        ];
        
        State.Done, [
          valid <-- vdd;
          when_ i.start [
            valid <-- gnd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    }
end

(* Test code *)
let test () =
  Stdio.printf "=== ModAdd Hardware Test (256-bit modular add/sub) ===\n\n";
  Stdio.printf "Modulus (n) = %s\n\n" (Z.to_string Config.prime_modulus_z);

  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(ModAdd.I)(ModAdd.O) in
  let sim = Sim.create (ModAdd.create scope) in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((Config.width + 3) / 4) ~char:'0' in
    Bits.of_hex ~width:Config.width padded
  in

  let bits_to_z bits =
    let bin_str = Bits.to_bstr bits in
    Z.of_string_base 2 bin_str
  in

  let test_case name a_z b_z is_sub =
    let op_str = if is_sub then "-" else "+" in
    Stdio.printf "Test: %s\n" name;
    Stdio.printf "  a %s b mod n\n" op_str;
    Stdio.printf "  a = %s\n" (Z.to_string a_z);
    Stdio.printf "  b = %s\n" (Z.to_string b_z);

    let expected_z = 
      let raw = if is_sub then Z.(a_z - b_z) else Z.(a_z + b_z) in
      Z.(erem raw Config.prime_modulus_z)
    in
    Stdio.printf "  expected = %s\n" (Z.to_string expected_z);

    (* Reset *)
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.a := Bits.zero Config.width;
    inputs.b := Bits.zero Config.width;
    inputs.subtract := Bits.gnd;
    Cyclesim.cycle sim;

    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;

    (* Start computation *)
    inputs.a := z_to_bits a_z;
    inputs.b := z_to_bits b_z;
    inputs.subtract := if is_sub then Bits.vdd else Bits.gnd;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;

    inputs.start := Bits.gnd;

    (* Wait for result *)
    let max_cycles = 20 in
    let rec wait cycle_count =
      if cycle_count >= max_cycles then begin
        Stdio.printf "  ERROR: Timeout after %d cycles\n\n" max_cycles;
        false
      end else if Bits.to_bool !(outputs.valid) then begin
        let result_z = bits_to_z !(outputs.result) in
        
        Stdio.printf "  Completed in %d cycles\n" cycle_count;
        Stdio.printf "  result = %s\n" (Z.to_string result_z);
        
        let verified = Z.equal result_z expected_z in
        
        if verified then
          Stdio.printf "  Verification: PASS ✓\n"
        else
          Stdio.printf "  Verification: FAIL ✗ (diff: %s)\n" 
            (Z.to_string Z.(result_z - expected_z));
        
        Stdio.printf "\n";
        verified
      end else begin
        Cyclesim.cycle sim;
        wait (cycle_count + 1)
      end
    in
    wait 0
  in

  let results = [
    (* Basic addition tests *)
    test_case "Simple addition"
      (Z.of_int 100) (Z.of_int 200) false;
    
    test_case "Addition with modular wrap"
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671600")
      (Z.of_int 100) false;
    
    (* Basic subtraction tests *)
    test_case "Simple subtraction"
      (Z.of_int 500) (Z.of_int 300) true;
    
    test_case "Subtraction requiring modular correction"
      (Z.of_int 100) (Z.of_int 200) true;
    
    test_case "Subtraction from modulus-1"
      Z.(Config.prime_modulus_z - one) (Z.of_int 10) true;
    
    (* Edge cases *)
    test_case "Add zero"
      (Z.of_int 12345) Z.zero false;
    
    test_case "Subtract zero"
      (Z.of_int 12345) Z.zero true;
    
    test_case "Add to get exactly modulus (wraps to 0)"
      (Z.of_string "57896044618658097711785492504343953926634992332820282019728792003954417335831")
      (Z.of_string "57896044618658097711785492504343953926634992332820282019728792003954417335832")
      false;
    
    (* Large value tests *)
    test_case "Large addition"
      (Z.of_string "80000000000000000000000000000000000000000000000000000000000000000000000000")
      (Z.of_string "30000000000000000000000000000000000000000000000000000000000000000000000000")
      false;
    
    test_case "Large subtraction"
      (Z.of_string "80000000000000000000000000000000000000000000000000000000000000000000000000")
      (Z.of_string "30000000000000000000000000000000000000000000000000000000000000000000000000")
      true;
    
    (* Edge: subtract from zero *)
    test_case "Zero minus value (wraps around)"
      Z.zero (Z.of_int 42) true;
    
    (* Edge: identical values *)
    test_case "Subtract identical values"
      (Z.of_int 999999) (Z.of_int 999999) true;
    
    test_case "Add identical values"
      (Z.of_int 999999) (Z.of_int 999999) false;
    
    (* Maximum values *)
    test_case "Add two maximum values (modulus-1)"
      Z.(Config.prime_modulus_z - one) 
      Z.(Config.prime_modulus_z - one) 
      false;
    
    test_case "Subtract from maximum"
      Z.(Config.prime_modulus_z - one) 
      (Z.of_int 1) 
      true;
    
    test_case "Add 1 to maximum (wraps to 0)"
      Z.(Config.prime_modulus_z - one) 
      Z.one 
      false;
    
    (* Powers of 2 *)
    test_case "Add powers of 2: 2^128 + 2^127"
      (Z.shift_left Z.one 128)
      (Z.shift_left Z.one 127)
      false;
    
    test_case "Subtract powers of 2: 2^200 - 2^100"
      (Z.shift_left Z.one 200)
      (Z.shift_left Z.one 100)
      true;
    
    test_case "Subtract larger power from smaller: 2^100 - 2^200"
      (Z.shift_left Z.one 100)
      (Z.shift_left Z.one 200)
      true;
    
    (* Near modulus boundaries *)
    test_case "Add near modulus: (n-10) + 5"
      Z.(Config.prime_modulus_z - of_int 10)
      (Z.of_int 5)
      false;
    
    test_case "Add near modulus: (n-10) + 20"
      Z.(Config.prime_modulus_z - of_int 10)
      (Z.of_int 20)
      false;
    
    test_case "Subtract near modulus: (n-10) - 20"
      Z.(Config.prime_modulus_z - of_int 10)
      (Z.of_int 20)
      true;
    
    (* Specific bit patterns *)
    test_case "All ones in lower 128 bits"
      Z.(shift_left one 128 - one)
      (Z.of_int 1)
      false;
    
    test_case "Alternating bit pattern: 0xAAAA...AAAA + 0x5555...5555"
      (Z.of_string "0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
      (Z.of_string "0x5555555555555555555555555555555555555555555555555555555555555555")
      false;
    
    test_case "Subtract alternating patterns"
      (Z.of_string "0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
      (Z.of_string "0x5555555555555555555555555555555555555555555555555555555555555555")
      true;
    
    (* Random-looking large numbers *)
    test_case "Large random numbers: addition"
      (Z.of_string "98765432109876543210987654321098765432109876543210987654321098765432")
      (Z.of_string "12345678901234567890123456789012345678901234567890123456789012345678")
      false;
    
    test_case "Large random numbers: subtraction"
      (Z.of_string "98765432109876543210987654321098765432109876543210987654321098765432")
      (Z.of_string "12345678901234567890123456789012345678901234567890123456789012345678")
      true;
    
    (* Carry propagation tests *)
    test_case "Maximum carry propagation in addition"
      (Z.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
      Z.one
      false;
    
    test_case "Borrow propagation in subtraction"
      (Z.shift_left Z.one 255)
      Z.one
      true;
    
    (* Chunk boundary tests (32-bit chunks) *)
    test_case "Values that cross 32-bit boundaries: 2^32"
      (Z.shift_left Z.one 32)
      Z.one
      false;
    
    test_case "Multiple of 2^32"
      (Z.shift_left Z.one 64)
      (Z.shift_left Z.one 32)
      false;
    
    test_case "Subtract across chunk boundary"
      (Z.of_string "0x100000000")  (* 2^32 *)
      (Z.of_string "0x1")
      true;
    
    (* Prime number specific tests (using modulus properties) *)
    test_case "Add modulus to value (should wrap)"
      (Z.of_int 12345)
      Config.prime_modulus_z
      false;
    
    test_case "Subtract modulus from value (should return same)"
      (Z.of_int 12345)
      Config.prime_modulus_z
      true;
    
    (* Small numbers with large results after modular arithmetic *)
    test_case "Small subtraction with wrap: 10 - 100"
      (Z.of_int 10)
      (Z.of_int 100)
      true;
    
    test_case "Very small subtraction: 1 - 2"
      Z.one
      (Z.of_int 2)
      true;
    
    (* Middle range values *)
    test_case "Mid-range addition"
      (Z.shift_left Z.one 127)
      (Z.shift_left Z.one 126)
      false;
    
    test_case "Mid-range subtraction"
      (Z.shift_left Z.one 127)
      (Z.shift_left Z.one 126)
      true;
    
    (* Sequential operations *)
    test_case "Add 1+1"
      Z.one Z.one false;
    
    test_case "Add 2^255 + 2^255"
      (Z.shift_left Z.one 255)
      (Z.shift_left Z.one 255)
      false;
    
    (* Stress test with maximum operands *)
    test_case "Maximum operands sum exactly to modulus"
      (Z.of_string "57896044618658097711785492504343953926634992332820282019728792003954417335831")
      (Z.of_string "57896044618658097711785492504343953926634992332820282019728792003954417335832")
      false;
    
    (* Additional corner cases *)
    test_case "Subtract 1 from 0 (wraps to n-1)"
      Z.zero
      Z.one
      true;
    
    test_case "Add (n-1) + 1 (wraps to 0)"
      Z.(Config.prime_modulus_z - one)
      Z.one
      false;
    
    test_case "Add (n-1) + 2 (wraps to 1)"
      Z.(Config.prime_modulus_z - one)
      (Z.of_int 2)
      false;
  ] in

  let passed = List.count results ~f:Fn.id in
  let total = List.length results in

  Stdio.printf "=== Test Summary ===\n";
  Stdio.printf "Passed: %d/%d\n" passed total;

  if passed = total then begin
    Stdio.printf "\n✓ All tests passed!\n"
  end else
    Stdio.printf "\n✗ Some tests failed\n"

let () = test ()