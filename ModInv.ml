open Base
open Hardcaml
open Signal

(* Configuration for 256-bit operations *)
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
      | Init
      | Loop
      | Finalize
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a  (* Pulse high to begin computation *)
      ; x : 'a [@bits Config.width]  (* Value to invert *)
      ; modulus : 'a [@bits Config.width]  (* Odd prime modulus *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits Config.width]  (* Modular inverse of x *)
      ; valid : 'a  (* High when result is valid *)
      ; exists : 'a  (* High if inverse exists (gcd was 1) *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    let width = Config.width in
    
    (* Use same width for everything - simpler and cleaner
       Extra 4 bits: 2 for intermediate overflow, 2 for safety margin *)
    let full_width = width + 4 in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    
    (* Create state machine *)
    let sm = State_machine.create (module State) spec ~enable:vdd in

    (* All variables use full_width for simplicity *)
    let x = Variable.reg spec ~width:full_width in
    let y = Variable.reg spec ~width:full_width in
    let s = Variable.reg spec ~width:full_width in
    let t = Variable.reg spec ~width:full_width in
    let u = Variable.reg spec ~width:full_width in
    let v = Variable.reg spec ~width:full_width in
    
    let x_orig = Variable.reg spec ~width:full_width in
    let modulus_orig = Variable.reg spec ~width:full_width in
    
    let result = Variable.reg spec ~width in
    let valid = Variable.reg spec ~width:1 in
    let exists = Variable.reg spec ~width:1 in

    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          when_ i.start [
            (* Handle special cases *)
            if_ (i.x ==:. 0) [
              (* 0 has no modular inverse *)
              result <-- zero width;
              exists <-- gnd;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.modulus ==:. 0) [
              (* Invalid modulus *)
              result <-- zero width;
              exists <-- gnd;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.x ==:. 1) [
              (* 1^(-1) = 1 *)
              result <-- of_int ~width:width 1;
              exists <-- vdd;
              valid <-- vdd;
              sm.set_next Done;
            ] [
              (* Normal case: extend inputs to full_width *)
              x_orig <-- uresize i.x full_width;
              modulus_orig <-- uresize i.modulus full_width;
              sm.set_next Init;
            ];
          ];
        ];

        State.Init, [
          valid <-- gnd;
          
          (* Initialize algorithm *)
          x <-- x_orig.value;
          y <-- modulus_orig.value;
          
          s <-- of_int ~width:full_width 1;
          t <-- zero full_width;
          u <-- zero full_width;
          v <-- of_int ~width:full_width 1;
          
          exists <-- gnd;
          sm.set_next Loop;
        ];

        State.Loop, [
          valid <-- gnd;
          
          (* Termination check: if x == 0, then y is the gcd *)
          if_ (x.value ==:. 0) [
            (* For modular inverse, gcd must be 1 *)
            if_ (y.value ==:. 1) [
              exists <-- vdd;
              sm.set_next Finalize;
            ] [
              (* gcd != 1, no inverse exists *)
              exists <-- gnd;
              result <-- zero width;
              valid <-- vdd;
              sm.set_next Done;
            ];
          ] [
            (* Main loop body: reduce x and y toward gcd *)
            
            (* while x is even: divide x by 2, adjust coefficients s and t *)
            if_ (~:(lsb x.value)) [
              x <-- srl x.value 1;
              
              if_ ((~:(lsb s.value)) &: (~:(lsb t.value))) [
                s <-- sra s.value 1;
                t <-- sra t.value 1;
              ] [
                (* No resize needed - everything is already the same width! *)
                s <-- sra (s.value +: modulus_orig.value) 1;
                t <-- sra (t.value -: x_orig.value) 1;
              ];
            ] @@ elif (~:(lsb y.value)) [
              y <-- srl y.value 1;
              
              if_ ((~:(lsb u.value)) &: (~:(lsb v.value))) [
                u <-- sra u.value 1;
                v <-- sra v.value 1;
              ] [
                u <-- sra (u.value +: modulus_orig.value) 1;
                v <-- sra (v.value -: x_orig.value) 1;
              ];
            ] [
              (* Both x and y are odd: subtract the smaller from the larger *)
              if_ (x.value >=: y.value) [
                x <-- x.value -: y.value;
                s <-- s.value -: u.value;
                t <-- t.value -: v.value;
              ] [
                y <-- y.value -: x.value;
                u <-- u.value -: s.value;
                v <-- v.value -: t.value;
              ];
            ];
          ];
        ];

        State.Finalize, [
          let u_low = sel_bottom u.value width in
          let modulus_low = sel_bottom modulus_orig.value width in
          let u_plus_mod_low = sel_bottom (u.value +: modulus_orig.value) width in
          let u_normalized = mux2 (msb u.value) u_plus_mod_low u_low in
          
          proc [
            if_ (u_normalized >=: modulus_low) [
              result <-- (u_normalized -: modulus_low);
            ] [
              result <-- u_normalized;
            ];
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
    ; exists = exists.value -- "exists"
    }
end

(* Test code with additional test cases *)
let test () =
  Stdio.printf "=== ModInv Hardware Test (256-bit with Zarith) ===\n";
  Stdio.printf "=== Assuming odd prime modulus ===\n\n";
  
  (* Create simulator *)
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(ModInv.I)(ModInv.O) in
  let sim = Sim.create (ModInv.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  (* Helper to convert Z to Bits *)
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:((Config.width + 3) / 4) ~char:'0' in
    Bits.of_hex ~width:Config.width padded
  in
  
  (* Helper function to test one case *)
  let test_case name x_z mod_z should_have_inverse expected_result_z_opt =
    Stdio.printf "Test: %s\n" name;
    Stdio.printf "  x       = %s\n" (Z.to_string x_z);
    Stdio.printf "  modulus = %s\n" (Z.to_string mod_z);
    
    (* Convert Z to Bits *)
    let x_bits = z_to_bits x_z in
    let mod_bits = z_to_bits mod_z in
    
    (* Reset *)
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.x := Bits.zero Config.width;
    inputs.modulus := Bits.zero Config.width;
    Cyclesim.cycle sim;
    
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;
    
    (* Start computation *)
    inputs.x := x_bits;
    inputs.modulus := mod_bits;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    
    inputs.start := Bits.gnd;
    
    (* Wait for result *)
    let max_cycles = 3000 in
    let rec wait cycle_count =
      if cycle_count >= max_cycles then begin
        Stdio.printf "  ERROR: Timeout after %d cycles\n\n" max_cycles;
        false
      end else if Bits.to_bool !(outputs.valid) then begin
        let result_bits = !(outputs.result) in
        let inverse_exists = Bits.to_bool !(outputs.exists) in
        
        Stdio.printf "  Completed in %d cycles\n" cycle_count;
        Stdio.printf "  Inverse exists: %b\n" inverse_exists;
        
        let test_passed = 
          if Bool.(<>) inverse_exists should_have_inverse then begin
            Stdio.printf "  ERROR: Expected inverse to %sexist ✗\n" 
              (if should_have_inverse then "" else "not ");
            false
          end else if inverse_exists then begin
            let result_z = 
              let bin_str = Bits.to_bstr result_bits in
              Z.of_string_base 2 bin_str
            in
            Stdio.printf "  result  = %s\n" (Z.to_string result_z);
            
            let product = Z.(x_z * result_z mod mod_z) in
            let verified = Z.equal product Z.one in
            
            Stdio.printf "  Verification: (x * result) mod modulus = %s\n" 
              (Z.to_string product);
            
            if verified then
              Stdio.printf "  Mathematical verification: PASS ✓\n"
            else
              Stdio.printf "  Mathematical verification: FAIL ✗\n";
            
            let matches_expected = 
              match expected_result_z_opt with
              | Some expected_z when Z.equal result_z expected_z ->
                  Stdio.printf "  Matches expected value ✓\n";
                  true
              | Some expected_z ->
                  Stdio.printf "  Note: Expected %s but got %s\n" 
                    (Z.to_string expected_z) (Z.to_string result_z);
                  verified
              | None ->
                  verified
            in
            
            verified && matches_expected
          end else begin
            Stdio.printf "  Correctly determined no inverse exists ✓\n";
            true
          end
        in
        
        Stdio.printf "\n";
        test_passed
      end else begin
        Cyclesim.cycle sim;
        wait (cycle_count + 1)
      end
    in
    wait 0
  in
  
  (* Run test cases *)
  let results = [
    (* Basic small tests with odd primes *)
    test_case "3^(-1) mod 7" 
      (Z.of_int 3) (Z.of_int 7) true (Some (Z.of_int 5));
      
    test_case "7^(-1) mod 13" 
      (Z.of_int 7) (Z.of_int 13) true (Some (Z.of_int 2));
      
    test_case "5^(-1) mod 11" 
      (Z.of_int 5) (Z.of_int 11) true (Some (Z.of_int 9));
      
    test_case "17^(-1) mod 31" 
      (Z.of_int 17) (Z.of_int 31) true (Some (Z.of_int 11));
    
    (* Larger odd primes *)
    test_case "42^(-1) mod 2017" 
      (Z.of_int 42) (Z.of_int 2017) true (Some (Z.of_int 1969));
      
    test_case "123^(-1) mod 257" 
      (Z.of_int 123) (Z.of_int 257) true None;
    
    (* Edge cases with odd primes *)
    test_case "1^(-1) mod 7" 
      (Z.of_int 1) (Z.of_int 7) true (Some (Z.of_int 1));
      
    test_case "2^(-1) mod 3" 
      (Z.of_int 2) (Z.of_int 3) true (Some (Z.of_int 2));
      
    (* Large 64-bit odd prime *)
    test_case "Large 64-bit inverse"
      (Z.of_string "123456789012345")
      (Z.of_string "999999999999999989")
      true None;
      
    (* Very large 128-bit odd prime *)
    test_case "128-bit inverse"
      (Z.of_string "123456789012345678901234567890")
      (Z.of_string "340282366920938463463374607431768211297")
      true None;
    
    (* 254-bit prime *)
    test_case "254-bit: inverse mod 254-bit prime"
      (Z.of_string "123456789012345678901234567890")
      (Z.of_string "28948022309329048855892746252171976963317496166410141009864396001978282409867")
      true None;
    
    (* 255-bit prime *)
    test_case "255-bit: inverse mod 255-bit prime"
      (Z.of_string "987654321098765432109876543210")
      (Z.of_string "57896044618658097711785492504343953926634992332820282019728792003956564819949")
      true None;
      
    (* Cryptographic-sized numbers (256-bit) *)
    (* Secp256k1 prime: 2^256 - 2^32 - 977 *)
    test_case "256-bit: inverse mod secp256k1 prime"
      (Z.of_string "12345678901234567890")
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007908834671663")
      true None;
      
    (* Another large odd prime close to 2^256 *)
    test_case "256-bit: inverse mod large odd prime"
      (Z.of_string "999999999999999999")
      (Z.of_string "115792089237316195423570985008687907853269984665640564039457584007913129639747")
      true None;
      
    (* Edge case: (m-1)^(-1) mod m should give m-1 for odd prime *)
    test_case "Edge: (m-1)^(-1) mod m"
      (Z.of_int 12)
      (Z.of_int 13)
      true (Some (Z.of_int 12));
  ] in
  
  (* Summary *)
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
    Stdio.printf "\nAll cryptographic tests passed! ✓✓✓\n"
  end else
    Stdio.printf "Some tests failed - NOT READY FOR PRODUCTION ✗\n"

let () = test ()