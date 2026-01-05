open Base
open Hardcaml

let () =
  Stdio.printf "=== Arith Unit Test ===\n\n";
  
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Arith.I)(Arith.O) in
  let sim = Sim.create (Arith.create scope) in
  
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  
  (* Simulated register file *)
  let registers = Array.init 32 ~f:(fun _ -> Z.zero) in
  
  let z_to_bits z =
    let hex_str = Z.format "%x" z in
    let padded = String.pad_left hex_str ~len:64 ~char:'0' in
    Bits.of_hex ~width:256 padded
  in
  
  let bits_to_z bits = Z.of_string_base 2 (Bits.to_bstr bits) in
  
  let reset () =
    inputs.clear := Bits.vdd;
    inputs.start := Bits.gnd;
    inputs.op := Bits.zero 2;
    inputs.prime_sel := Bits.gnd;
    inputs.addr_a := Bits.zero 5;
    inputs.addr_b := Bits.zero 5;
    inputs.addr_out := Bits.zero 5;
    inputs.reg_read_data_a := Bits.zero 256;
    inputs.reg_read_data_b := Bits.zero 256;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim
  in
  
  let run_op ~op ~prime_sel ~addr_a ~addr_b ~addr_out =
    (* Reset to clear any leftover state *)
    inputs.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    Cyclesim.cycle sim;
    
    (* Provide initial register data for the requested addresses *)
    inputs.reg_read_data_a := z_to_bits registers.(addr_a);
    inputs.reg_read_data_b := z_to_bits registers.(addr_b);
    
    (* Start operation *)
    inputs.start := Bits.vdd;
    inputs.op := Bits.of_int ~width:2 op;
    inputs.prime_sel := if prime_sel then Bits.vdd else Bits.gnd;
    inputs.addr_a := Bits.of_int ~width:5 addr_a;
    inputs.addr_b := Bits.of_int ~width:5 addr_b;
    inputs.addr_out := Bits.of_int ~width:5 addr_out;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    
    (* Run until done *)
    let max_cycles = 3000 in
    let rec wait n =
      if n >= max_cycles then begin
        Stdio.printf "  TIMEOUT after %d cycles\n" max_cycles;
        false
      end else begin
        (* Update register file read data based on requested addresses *)
        let read_addr_a = Bits.to_int !(outputs.reg_read_addr_a) in
        let read_addr_b = Bits.to_int !(outputs.reg_read_addr_b) in
        inputs.reg_read_data_a := z_to_bits registers.(read_addr_a);
        inputs.reg_read_data_b := z_to_bits registers.(read_addr_b);
        
        (* Check for write and done BEFORE cycling - they're combinational now *)
        let is_done = Bits.to_bool !(outputs.done_) in
        let is_write = Bits.to_bool !(outputs.reg_write_enable) in
        
        if is_write then begin
          let write_addr = Bits.to_int !(outputs.reg_write_addr) in
          let write_data = bits_to_z !(outputs.reg_write_data) in
          registers.(write_addr) <- write_data
        end;
        
        if is_done then begin
          (* Cycle once more to return to Idle *)
          Cyclesim.cycle sim;
          true
        end else begin
          Cyclesim.cycle sim;
          wait (n + 1)
        end
      end
    in
    wait 0
  in
  
  let prime_p = Arith.Config.prime_p in
  let prime_n = Arith.Config.prime_n in
  
  (* Test result tracking *)
  let results = ref [] in
  let record result = results := result :: !results in
  
  (* ============================================ *)
  (* ADDITION TESTS (adapted from mod_add tests) *)
  (* ============================================ *)
  
  Stdio.printf "=== Addition Tests ===\n\n";
  
  (* Test: Simple addition *)
  registers.(0) <- Z.of_int 100;
  registers.(1) <- Z.of_int 200;
  reset ();
  Stdio.printf "Test: Simple addition (100 + 200 mod p)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:0 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:10 then begin
    let expected = Z.((registers.(0) + registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(10));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(10) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Addition with modular wrap *)
  registers.(0) <- Z.(prime_p - of_int 63);
  registers.(1) <- Z.of_int 100;
  reset ();
  Stdio.printf "Test: Addition with modular wrap ((p-63) + 100 mod p)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:0 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:10 then begin
    let expected = Z.((registers.(0) + registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(10));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(10) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Add zero *)
  registers.(0) <- Z.of_int 12345;
  registers.(1) <- Z.zero;
  reset ();
  Stdio.printf "Test: Add zero (12345 + 0 mod p)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:0 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:10 then begin
    let expected = Z.((registers.(0) + registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(10));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(10) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Addition with prime_n (curve order) *)
  registers.(0) <- Z.of_int 100;
  registers.(1) <- Z.of_int 200;
  reset ();
  Stdio.printf "Test: Addition with curve order n (100 + 200 mod n)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:0 ~prime_sel:true ~addr_a:0 ~addr_b:1 ~addr_out:10 then begin
    let expected = Z.((registers.(0) + registers.(1)) mod prime_n) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(10));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(10) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* =============================================== *)
  (* SUBTRACTION TESTS (adapted from mod_add tests) *)
  (* =============================================== *)
  
  Stdio.printf "=== Subtraction Tests ===\n\n";
  
  (* Test: Simple subtraction *)
  registers.(0) <- Z.of_int 500;
  registers.(1) <- Z.of_int 300;
  reset ();
  Stdio.printf "Test: Simple subtraction (500 - 300 mod p)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:11 then begin
    let expected = Z.(erem (registers.(0) - registers.(1)) prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(11));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(11) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Subtraction requiring modular correction *)
  registers.(0) <- Z.of_int 100;
  registers.(1) <- Z.of_int 200;
  reset ();
  Stdio.printf "Test: Subtraction with wrap (100 - 200 mod p)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:11 then begin
    let expected = Z.(erem (registers.(0) - registers.(1)) prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(11));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(11) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Subtraction from modulus-1 *)
  registers.(0) <- Z.(prime_p - one);
  registers.(1) <- Z.of_int 10;
  reset ();
  Stdio.printf "Test: Subtraction from (p-1) - 10 mod p\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:11 then begin
    let expected = Z.(erem (registers.(0) - registers.(1)) prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(11));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(11) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Subtract zero *)
  registers.(0) <- Z.of_int 12345;
  registers.(1) <- Z.zero;
  reset ();
  Stdio.printf "Test: Subtract zero (12345 - 0 mod p)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:11 then begin
    let expected = Z.(erem (registers.(0) - registers.(1)) prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(11));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(11) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Subtraction with prime_n *)
  registers.(0) <- Z.of_int 10;
  registers.(1) <- Z.of_int 20;
  reset ();
  Stdio.printf "Test: Subtraction with curve order n (10 - 20 mod n)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  if run_op ~op:1 ~prime_sel:true ~addr_a:0 ~addr_b:1 ~addr_out:11 then begin
    let expected = Z.(erem (registers.(0) - registers.(1)) prime_n) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(11));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(11) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* ================================================= *)
  (* MULTIPLICATION TESTS (adapted from mod_mul tests) *)
  (* ================================================= *)
  
  Stdio.printf "=== Multiplication Tests ===\n\n";
  
  (* Test: Simple multiplication *)
  registers.(0) <- Z.of_int 3;
  registers.(1) <- Z.of_int 5;
  reset ();
  Stdio.printf "Test: Simple multiplication (3 * 5 mod p)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Multiply by zero *)
  registers.(0) <- Z.of_int 12345;
  registers.(1) <- Z.zero;
  reset ();
  Stdio.printf "Test: Multiply by zero (12345 * 0 mod p)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Multiply by one *)
  registers.(0) <- Z.of_int 12345;
  registers.(1) <- Z.one;
  reset ();
  Stdio.printf "Test: Multiply by one (12345 * 1 mod p)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Medium multiplication *)
  registers.(0) <- Z.of_int 123456;
  registers.(1) <- Z.of_int 789012;
  reset ();
  Stdio.printf "Test: Medium multiplication (123456 * 789012 mod p)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: 64-bit multiplication *)
  registers.(0) <- Z.of_string "12345678901234";
  registers.(1) <- Z.of_string "98765432109876";
  reset ();
  Stdio.printf "Test: 64-bit multiplication\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: 128-bit multiplication *)
  registers.(0) <- Z.of_string "123456789012345678901234567890";
  registers.(1) <- Z.of_string "987654321098765432109876543210";
  reset ();
  Stdio.printf "Test: 128-bit multiplication\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: 256-bit multiplication *)
  registers.(0) <- Z.of_string "123456789012345678901234567890123456789";
  registers.(1) <- Z.of_string "987654321098765432109876543210987654321";
  reset ();
  Stdio.printf "Test: 256-bit multiplication mod secp256k1 prime\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Multiplication with prime_n *)
  registers.(0) <- Z.of_int 12345;
  registers.(1) <- Z.of_int 67890;
  reset ();
  Stdio.printf "Test: Multiplication with curve order n (12345 * 67890 mod n)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  y = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:true ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_n) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(12) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* ============================================== *)
  (* INVERSION TESTS (adapted from mod_inv tests)  *)
  (* ============================================== *)
  
  Stdio.printf "=== Inversion Tests ===\n\n";
  
  (* Test: Simple inversion 3^(-1) mod p *)
  registers.(0) <- Z.of_int 3;
  reset ();
  Stdio.printf "Test: Simple inversion (3^(-1) mod p)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  result = %s\n" (Z.to_string registers.(13));
    if inv_exists then begin
      let product = Z.((registers.(0) * registers.(13)) mod prime_p) in
      Stdio.printf "  Verification: x * result mod p = %s\n" (Z.to_string product);
      let pass = Z.equal product Z.one in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass
    end else record false
  end else record false;
  
  (* Test: Inversion of 1 *)
  registers.(0) <- Z.one;
  reset ();
  Stdio.printf "Test: Inversion of 1 (1^(-1) mod p = 1)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  result = %s\n" (Z.to_string registers.(13));
    let pass = inv_exists && Z.equal registers.(13) Z.one in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Inversion of (p-1) should be (p-1) since (p-1)^2 = 1 mod p *)
  registers.(0) <- Z.(prime_p - one);
  reset ();
  Stdio.printf "Test: Inversion of (p-1)^(-1) mod p = (p-1)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  result = %s\n" (Z.to_string registers.(13));
    if inv_exists then begin
      let product = Z.((registers.(0) * registers.(13)) mod prime_p) in
      Stdio.printf "  Verification: x * result mod p = %s\n" (Z.to_string product);
      let pass = Z.equal product Z.one in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass
    end else record false
  end else record false;
  
  (* Test: 64-bit inversion *)
  registers.(0) <- Z.of_string "123456789012345";
  reset ();
  Stdio.printf "Test: 64-bit inversion\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  result = %s\n" (Z.to_string registers.(13));
    if inv_exists then begin
      let product = Z.((registers.(0) * registers.(13)) mod prime_p) in
      Stdio.printf "  Verification: x * result mod p = %s\n" (Z.to_string product);
      let pass = Z.equal product Z.one in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass
    end else record false
  end else record false;
  
  (* Test: 128-bit inversion *)
  registers.(0) <- Z.of_string "123456789012345678901234567890";
  reset ();
  Stdio.printf "Test: 128-bit inversion\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  result = %s\n" (Z.to_string registers.(13));
    if inv_exists then begin
      let product = Z.((registers.(0) * registers.(13)) mod prime_p) in
      Stdio.printf "  Verification: x * result mod p = %s\n" (Z.to_string product);
      let pass = Z.equal product Z.one in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass
    end else record false
  end else record false;
  
  (* Test: 256-bit inversion *)
  registers.(0) <- Z.of_string "12345678901234567890123456789012345678901234567890";
  reset ();
  Stdio.printf "Test: 256-bit inversion mod secp256k1 prime\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  result = %s\n" (Z.to_string registers.(13));
    if inv_exists then begin
      let product = Z.((registers.(0) * registers.(13)) mod prime_p) in
      Stdio.printf "  Verification: x * result mod p = %s\n" (Z.to_string product);
      let pass = Z.equal product Z.one in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass
    end else record false
  end else record false;
  
  (* Test: Inversion with prime_n (curve order) *)
  registers.(0) <- Z.of_string "999999999999999999";
  reset ();
  Stdio.printf "Test: Inversion with curve order n\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:true ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  result = %s\n" (Z.to_string registers.(13));
    if inv_exists then begin
      let product = Z.((registers.(0) * registers.(13)) mod prime_n) in
      Stdio.printf "  Verification: x * result mod n = %s\n" (Z.to_string product);
      let pass = Z.equal product Z.one in
      Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
      record pass
    end else record false
  end else record false;
  
  (* ============================================== *)
  (* CHAINED OPERATIONS TEST                       *)
  (* ============================================== *)
  
  Stdio.printf "=== Chained Operations Test ===\n\n";
  
  (* Test a sequence: (a + b) * c mod p *)
  registers.(0) <- Z.of_int 100;
  registers.(1) <- Z.of_int 200;
  registers.(2) <- Z.of_int 50;
  reset ();
  
  Stdio.printf "Test: Chained operations ((a + b) * c mod p)\n";
  Stdio.printf "  a = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  b = %s\n" (Z.to_string registers.(1));
  Stdio.printf "  c = %s\n" (Z.to_string registers.(2));
  
  (* First: r[20] = r[0] + r[1] *)
  let chain_pass = ref true in
  if run_op ~op:0 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:20 then begin
    Stdio.printf "  Step 1: r[20] = a + b = %s\n" (Z.to_string registers.(20));
    (* Second: r[21] = r[20] * r[2] *)
    if run_op ~op:2 ~prime_sel:false ~addr_a:20 ~addr_b:2 ~addr_out:21 then begin
      let expected = Z.(((registers.(0) + registers.(1)) * registers.(2)) mod prime_p) in
      Stdio.printf "  Step 2: r[21] = r[20] * c = %s\n" (Z.to_string registers.(21));
      Stdio.printf "  expected = %s\n" (Z.to_string expected);
      chain_pass := Z.equal registers.(21) expected;
      Stdio.printf "  %s\n\n" (if !chain_pass then "PASS ✓" else "FAIL ✗")
    end else chain_pass := false
  end else chain_pass := false;
  record !chain_pass;
  
  (* ============================================== *)
  (* BACK-TO-BACK OPERATIONS TEST                  *)
  (* ============================================== *)
  
  Stdio.printf "=== Back-to-Back Operations Test ===\n\n";
  
  registers.(0) <- Z.of_int 7;
  registers.(1) <- Z.of_int 11;
  reset ();
  
  Stdio.printf "Test: Rapid back-to-back operations\n";
  let btb_pass = ref true in
  
  (* Op 1: add *)
  if run_op ~op:0 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:25 then begin
    let exp1 = Z.((of_int 7 + of_int 11) mod prime_p) in
    if not (Z.equal registers.(25) exp1) then btb_pass := false;
    Stdio.printf "  Op 1 (add): %s (expected %s) %s\n" 
      (Z.to_string registers.(25)) (Z.to_string exp1)
      (if Z.equal registers.(25) exp1 then "✓" else "✗")
  end else btb_pass := false;
  
  (* Op 2: sub *)
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:26 then begin
    let exp2 = Z.(erem (of_int 7 - of_int 11) prime_p) in
    if not (Z.equal registers.(26) exp2) then btb_pass := false;
    Stdio.printf "  Op 2 (sub): %s (expected %s) %s\n" 
      (Z.to_string registers.(26)) (Z.to_string exp2)
      (if Z.equal registers.(26) exp2 then "✓" else "✗")
  end else btb_pass := false;
  
  (* Op 3: mul *)
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:27 then begin
    let exp3 = Z.((of_int 7 * of_int 11) mod prime_p) in
    if not (Z.equal registers.(27) exp3) then btb_pass := false;
    Stdio.printf "  Op 3 (mul): %s (expected %s) %s\n" 
      (Z.to_string registers.(27)) (Z.to_string exp3)
      (if Z.equal registers.(27) exp3 then "✓" else "✗")
  end else btb_pass := false;
  
  (* Op 4: inv *)
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:28 then begin
    let product = Z.((of_int 7 * registers.(28)) mod prime_p) in
    if not (Z.equal product Z.one) then btb_pass := false;
    Stdio.printf "  Op 4 (inv): 7 * %s mod p = %s %s\n" 
      (Z.to_string registers.(28)) (Z.to_string product)
      (if Z.equal product Z.one then "✓" else "✗")
  end else btb_pass := false;
  
  Stdio.printf "  %s\n\n" (if !btb_pass then "PASS ✓" else "FAIL ✗");
  record !btb_pass;
  
  (* ============================================== *)
  (* TEST SUMMARY                                  *)
  (* ============================================== *)
  
  let results_list = List.rev !results in
  let passed = List.count results_list ~f:Fn.id in
  let total = List.length results_list in
  
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
    Stdio.printf "\nAll Arith unit tests passed! ✓✓✓\n"
  end else
    Stdio.printf "\n✗ Some tests failed - review above for details\n"