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

    (* Accumulator *)
    let acc = Variable.reg spec ~width:acc_width in

    (* Multiplier (y) and bit counter *)
    let multiplier = Variable.reg spec ~width in
    let bit_count = Variable.reg spec ~width:9 in

    (* Stored inputs *)
    let x_orig = Variable.reg spec ~width:acc_width in
    let modulus_orig = Variable.reg spec ~width:acc_width in

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
            ] [
              x_orig <-- uresize i.x acc_width;
              modulus_orig <-- uresize i.modulus acc_width;
              multiplier <-- i.y;
              sm.set_next Init;
            ];
          ];
        ];

        State.Init, [
          valid <-- gnd;
          acc <-- zero acc_width;
          bit_count <-- of_int ~width:9 0;
          sm.set_next Loop;
        ];

        State.Loop, [
          valid <-- gnd;
          
          if_ (bit_count.value ==: of_int ~width:9 width) [
            (* All bits processed *)
            result <-- sel_bottom acc.value width;
            valid <-- vdd;
            sm.set_next Done;
          ] [
            (* Step 1: Double the accumulator *)
            let doubled = sll acc.value 1 in
            
            (* Step 2: If doubled >= modulus, subtract modulus *)
            let after_double_reduce = 
              mux2 (doubled >=: modulus_orig.value)
                (doubled -: modulus_orig.value)
                doubled
            in
            
            (* Step 3: If current bit of multiplier is 1, add x *)
            let current_bit = msb multiplier.value in
            let after_add = 
              mux2 current_bit
                (after_double_reduce +: x_orig.value)
                after_double_reduce
            in
            
            (* Step 4: If result >= modulus, subtract modulus *)
            let after_add_reduce =
              mux2 (after_add >=: modulus_orig.value)
                (after_add -: modulus_orig.value)
                after_add
            in
            
            proc [
              acc <-- after_add_reduce;
              multiplier <-- sll multiplier.value 1;
              bit_count <-- bit_count.value +:. 1;
            ];
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
  Stdio.printf "=== ModMul Hardware Test (256-bit with Zarith) ===\n\n";

  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(ModMul.I)(ModMul.O) in
  let sim = Sim.create (ModMul.create scope) in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((Config.width + 3) / 4) ~char:'0' in
    Bits.of_hex ~width:Config.width padded
  in

  let test_case name x_z y_z mod_z =
    Stdio.printf "Test: %s\n" name;
    Stdio.printf "  x       = %s\n" (Z.to_string x_z);
    Stdio.printf "  y       = %s\n" (Z.to_string y_z);
    Stdio.printf "  modulus = %s\n" (Z.to_string mod_z);

    let expected_z = Z.((x_z * y_z) mod mod_z) in
    Stdio.printf "  expected= %s\n" (Z.to_string expected_z);

    let x_bits = z_to_bits x_z in
    let y_bits = z_to_bits y_z in
    let mod_bits = z_to_bits mod_z in

    (* Reset *)
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.x := Bits.zero Config.width;
    inputs.y := Bits.zero Config.width;
    inputs.modulus := Bits.zero Config.width;
    Cyclesim.cycle sim;

    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;

    (* Start computation *)
    inputs.x := x_bits;
    inputs.y := y_bits;
    inputs.modulus := mod_bits;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;

    inputs.start := Bits.gnd;

    (* Wait for result *)
    let max_cycles = 500 in
    let rec wait cycle_count =
      if cycle_count >= max_cycles then begin
        Stdio.printf "  ERROR: Timeout after %d cycles\n\n" max_cycles;
        false
      end else if Bits.to_bool !(outputs.valid) then begin
        let result_bits = !(outputs.result) in
        let result_z = 
          let bin_str = Bits.to_bstr result_bits in
          Z.of_string_base 2 bin_str
        in
        
        Stdio.printf "  Completed in %d cycles\n" cycle_count;
        Stdio.printf "  result  = %s\n" (Z.to_string result_z);
        
        let verified = Z.equal result_z expected_z in
        
        if verified then
          Stdio.printf "  Verification: PASS ✓\n"
        else
          Stdio.printf "  Verification: FAIL ✗\n";
        
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
    (* Basic small tests *)
    test_case "3 * 5 mod 7"
      (Z.of_int 3) (Z.of_int 5) (Z.of_int 7);
    
    test_case "12 * 15 mod 17"
      (Z.of_int 12) (Z.of_int 15) (Z.of_int 17);
    
    test_case "123 * 456 mod 1009"
      (Z.of_int 123) (Z.of_int 456) (Z.of_int 1009);
    
    (* Larger tests *)
    test_case "Large: 123456 * 789012 mod 1000003"
      (Z.of_int 123456) (Z.of_int 789012) (Z.of_int 1000003);
    
    (* 64-bit *)
    test_case "64-bit multiplication"
      (Z.of_string "12345678901234")
      (Z.of_string "98765432109876")
      (Z.of_string "999999999999999989");
    
    (* 128-bit *)
    test_case "128-bit multiplication"
      (Z.of_string "123456789012345678901234567890")
      (Z.of_string "987654321098765432109876543210")
      (Z.of_string "340282366920938463463374607431768211297");
    
    (* 256-bit with secp256k1 prime *)
    test_case "256-bit: multiplication mod secp256k1 prime"
      (Z.of_string "123456789012345678901234567890123456789")
      (Z.of_string "987654321098765432109876543210987654321")
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663");
  ] in

  let passed = List.count results ~f:Fn.id in
  let total = List.length results in

  Stdio.printf "=== Test Summary ===\n";
  Stdio.printf "Passed: %d/%d\n" passed total;

  if passed = total then begin
    Stdio.printf "\n";
    Stdio.printf "███████╗██╗   ██╗ ██████╗ ██████╗███████╗███████╗███████╗\n";
    Stdio.printf "██╔════╝██║   ██║██╔════╝██╔════╝██╔════╝██╔════╝██╔════╝\n";
    Stdio.printf "███████╗██║   ██║██║     ██║     █████╗  ███████╗███████╗\n";
    Stdio.printf "╚════██║██║   ██║██║     ██║     ██╔══╝  ╚════██║╚════██║\n";
    Stdio.printf "███████║╚██████╔╝╚██████╗╚██████╗███████╗███████║███████║\n";
    Stdio.printf "╚══════╝ ╚═════╝  ╚═════╝ ╚═════╝╚══════╝╚══════╝╚══════╝\n";
    Stdio.printf "\nAll tests passed! ✓✓✓\n"
  end else
    Stdio.printf "Some tests failed ✗\n"

let () = test ()