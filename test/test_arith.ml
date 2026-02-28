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
    let max_cycles = 10_000 in
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
  (* EDGE CASE TESTS                               *)
  (* ============================================== *)
  
  Stdio.printf "=== Edge Case Tests ===\n\n";
  
  (* Test: Inverse of zero should not exist *)
  registers.(0) <- Z.zero;
  reset ();
  Stdio.printf "Test: 0^(-1) mod p (should not exist)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:15 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    let pass = not inv_exists in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: x - x = 0 *)
  registers.(0) <- Z.of_string "98765432109876543210";
  reset ();
  Stdio.printf "Test: x - x mod p = 0\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:15 then begin
    let expected = Z.zero in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(15));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(15) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: (p-1) + 1 mod p = 0 *)
  registers.(0) <- Z.(prime_p - one);
  registers.(1) <- Z.one;
  reset ();
  Stdio.printf "Test: (p-1) + 1 mod p = 0\n";
  Stdio.printf "  a = p-1\n";
  Stdio.printf "  b = 1\n";
  if run_op ~op:0 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:15 then begin
    let expected = Z.zero in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(15));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(15) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: 0 - 1 mod p = p-1 *)
  registers.(0) <- Z.zero;
  registers.(1) <- Z.one;
  reset ();
  Stdio.printf "Test: 0 - 1 mod p = p-1\n";
  Stdio.printf "  a = 0\n";
  Stdio.printf "  b = 1\n";
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:15 then begin
    let expected = Z.(prime_p - one) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(15));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(15) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: (p-1) * (p-1) mod p = 1  since (-1)*(-1) = 1 *)
  registers.(0) <- Z.(prime_p - one);
  reset ();
  Stdio.printf "Test: (p-1) * (p-1) mod p = 1\n";
  Stdio.printf "  x = p-1 (which is -1 mod p)\n";
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:15 then begin
    let expected = Z.one in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(15));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(15) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Squaring via same register (addr_a = addr_b) *)
  registers.(0) <- Z.of_int 12345;
  reset ();
  Stdio.printf "Test: Squaring x^2 mod p (addr_a = addr_b)\n";
  Stdio.printf "  x = %s\n" (Z.to_string registers.(0));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:15 then begin
    let expected = Z.((registers.(0) * registers.(0)) mod prime_p) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(15));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(15) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: In-place operation (addr_a = addr_out) *)
  registers.(5) <- Z.of_int 100;
  registers.(6) <- Z.of_int 50;
  let orig_val = registers.(5) in
  reset ();
  Stdio.printf "Test: In-place add r[5] <- r[5] + r[6]\n";
  Stdio.printf "  r[5] = %s\n" (Z.to_string registers.(5));
  Stdio.printf "  r[6] = %s\n" (Z.to_string registers.(6));
  if run_op ~op:0 ~prime_sel:false ~addr_a:5 ~addr_b:6 ~addr_out:5 then begin
    let expected = Z.((orig_val + registers.(6)) mod prime_p) in
    Stdio.printf "  result r[5] = %s\n" (Z.to_string registers.(5));
    Stdio.printf "  expected    = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(5) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Large values mod n *)
  registers.(0) <- Z.of_string "115792089237316195423570985008687907852837564279074904382605163141518161494000";
  registers.(1) <- Z.of_string "500";
  reset ();
  Stdio.printf "Test: Large value near n, addition mod n\n";
  Stdio.printf "  a = n - 337\n";
  Stdio.printf "  b = 500\n";
  if run_op ~op:0 ~prime_sel:true ~addr_a:0 ~addr_b:1 ~addr_out:15 then begin
    let expected = Z.((registers.(0) + registers.(1)) mod prime_n) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(15));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(15) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;
  
  (* Test: Large multiplication mod n *)
  registers.(0) <- Z.of_string "57896044618658097711785492504343953926418782139537452191302581570759080747168";
  registers.(1) <- Z.of_string "2";
  reset ();
  Stdio.printf "Test: Large multiplication mod n (value near n/2 * 2)\n";
  Stdio.printf "  a ≈ n/2\n";
  Stdio.printf "  b = 2\n";
  if run_op ~op:2 ~prime_sel:true ~addr_a:0 ~addr_b:1 ~addr_out:15 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_n) in
    Stdio.printf "  result   = %s\n" (Z.to_string registers.(15));
    Stdio.printf "  expected = %s\n" (Z.to_string expected);
    let pass = Z.equal registers.(15) expected in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    record pass
  end else record false;

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


  (* Add this test to test_arith.ml, before the test summary section *)

(* ============================================== *)
(* BACK-TO-BACK MULTIPLICATION TEST              *)
(* ============================================== *)

Stdio.printf "=== Back-to-Back Multiplication Test ===\n\n";

(* Test: Two consecutive multiplications with different operands *)
registers.(0) <- Z.of_string "55066263022277343669578718895168534326250603453777594175500187360389116729240";
registers.(1) <- Z.of_string "55066263022277343669578718895168534326250603453777594175500187360389116729240";
registers.(2) <- Z.of_string "32670510020758816978083085130507043184471273380659243275938904335757337482424";
registers.(3) <- Z.of_string "32670510020758816978083085130507043184471273380659243275938904335757337482424";
reset ();

Stdio.printf "Test: Back-to-back multiplications with different operands\n";
Stdio.printf "  r[0] = r[1] = G_x (secp256k1 generator x)\n";
Stdio.printf "  r[2] = r[3] = G_y (secp256k1 generator y)\n\n";

(* First multiplication: r[10] = r[0] * r[1] = G_x * G_x *)
Stdio.printf "Step 1: r[10] = r[0] * r[1] (G_x * G_x)\n";
let mul1_pass = 
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:10 then begin
    let expected1 = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  result   = %s...\n" (String.prefix (Z.to_string registers.(10)) 40);
    Stdio.printf "  expected = %s...\n" (String.prefix (Z.to_string expected1) 40);
    let pass = Z.equal registers.(10) expected1 in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    pass
  end else false
in

(* Second multiplication: r[11] = r[2] * r[3] = G_y * G_y *)
Stdio.printf "Step 2: r[11] = r[2] * r[3] (G_y * G_y)\n";
let mul2_pass =
  if run_op ~op:2 ~prime_sel:false ~addr_a:2 ~addr_b:3 ~addr_out:11 then begin
    let expected2 = Z.((registers.(2) * registers.(3)) mod prime_p) in
    Stdio.printf "  result   = %s...\n" (String.prefix (Z.to_string registers.(11)) 40);
    Stdio.printf "  expected = %s...\n" (String.prefix (Z.to_string expected2) 40);
    let pass = Z.equal registers.(11) expected2 in
    Stdio.printf "  %s\n\n" (if pass then "PASS ✓" else "FAIL ✗");
    pass
  end else false
in

(* Verify the two results are different *)
Stdio.printf "Step 3: Verify results are different\n";
let different = not (Z.equal registers.(10) registers.(11)) in
Stdio.printf "  r[10] != r[11]: %b\n" different;
Stdio.printf "  %s\n\n" (if different then "PASS ✓" else "FAIL ✗ (results should differ!)");

let btb_mul_pass = mul1_pass && mul2_pass && different in
Stdio.printf "Back-to-back multiplication test: %s\n\n" 
  (if btb_mul_pass then "PASS ✓" else "FAIL ✗");
record btb_mul_pass;

(* ============================================== *)
(* TRIPLE MULTIPLICATION TEST                    *)
(* ============================================== *)

Stdio.printf "=== Triple Consecutive Multiplication Test ===\n\n";

registers.(0) <- Z.of_int 12345;
registers.(1) <- Z.of_int 67890;
registers.(2) <- Z.of_int 11111;
registers.(3) <- Z.of_int 22222;
registers.(4) <- Z.of_int 33333;
registers.(5) <- Z.of_int 44444;
reset ();

Stdio.printf "Test: Three consecutive multiplications\n";

(* Mul 1 *)
let triple_pass = ref true in
Stdio.printf "  Mul 1: r[20] = r[0] * r[1] = 12345 * 67890\n";
if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:20 then begin
  let exp1 = Z.((of_int 12345 * of_int 67890) mod prime_p) in
  if not (Z.equal registers.(20) exp1) then triple_pass := false;
  Stdio.printf "    result=%s expected=%s %s\n" 
    (Z.to_string registers.(20)) (Z.to_string exp1)
    (if Z.equal registers.(20) exp1 then "✓" else "✗")
end else triple_pass := false;

(* Mul 2 *)
Stdio.printf "  Mul 2: r[21] = r[2] * r[3] = 11111 * 22222\n";
if run_op ~op:2 ~prime_sel:false ~addr_a:2 ~addr_b:3 ~addr_out:21 then begin
  let exp2 = Z.((of_int 11111 * of_int 22222) mod prime_p) in
  if not (Z.equal registers.(21) exp2) then triple_pass := false;
  Stdio.printf "    result=%s expected=%s %s\n" 
    (Z.to_string registers.(21)) (Z.to_string exp2)
    (if Z.equal registers.(21) exp2 then "✓" else "✗")
end else triple_pass := false;

(* Mul 3 *)
Stdio.printf "  Mul 3: r[22] = r[4] * r[5] = 33333 * 44444\n";
if run_op ~op:2 ~prime_sel:false ~addr_a:4 ~addr_b:5 ~addr_out:22 then begin
  let exp3 = Z.((of_int 33333 * of_int 44444) mod prime_p) in
  if not (Z.equal registers.(22) exp3) then triple_pass := false;
  Stdio.printf "    result=%s expected=%s %s\n" 
    (Z.to_string registers.(22)) (Z.to_string exp3)
    (if Z.equal registers.(22) exp3 then "✓" else "✗")
end else triple_pass := false;

Stdio.printf "  Triple multiplication test: %s\n\n" 
  (if !triple_pass then "PASS ✓" else "FAIL ✗");
record !triple_pass;

(* ============================================== *)
(* MIXED OPERATION SEQUENCE TEST                 *)
(* ============================================== *)

Stdio.printf "=== Mixed Operation Sequence Test ===\n\n";

registers.(0) <- Z.of_int 1000;
registers.(1) <- Z.of_int 2000;
registers.(2) <- Z.of_int 500;
reset ();

Stdio.printf "Test: mul -> add -> mul -> sub -> mul\n";

let mixed_pass = ref true in

(* Mul *)
Stdio.printf "  Op 1 (mul): r[10] = r[0] * r[1]\n";
if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:10 then begin
  let exp = Z.((of_int 1000 * of_int 2000) mod prime_p) in
  if not (Z.equal registers.(10) exp) then mixed_pass := false;
  Stdio.printf "    result=%s %s\n" (Z.to_string registers.(10))
    (if Z.equal registers.(10) exp then "✓" else "✗")
end else mixed_pass := false;

(* Add *)
Stdio.printf "  Op 2 (add): r[11] = r[10] + r[2]\n";
if run_op ~op:0 ~prime_sel:false ~addr_a:10 ~addr_b:2 ~addr_out:11 then begin
  let exp = Z.((registers.(10) + of_int 500) mod prime_p) in
  if not (Z.equal registers.(11) exp) then mixed_pass := false;
  Stdio.printf "    result=%s %s\n" (Z.to_string registers.(11))
    (if Z.equal registers.(11) exp then "✓" else "✗")
end else mixed_pass := false;

(* Mul *)
Stdio.printf "  Op 3 (mul): r[12] = r[11] * r[2]\n";
if run_op ~op:2 ~prime_sel:false ~addr_a:11 ~addr_b:2 ~addr_out:12 then begin
  let exp = Z.((registers.(11) * of_int 500) mod prime_p) in
  if not (Z.equal registers.(12) exp) then mixed_pass := false;
  Stdio.printf "    result=%s %s\n" (Z.to_string registers.(12))
    (if Z.equal registers.(12) exp then "✓" else "✗")
end else mixed_pass := false;

(* Sub *)
Stdio.printf "  Op 4 (sub): r[13] = r[12] - r[0]\n";
if run_op ~op:1 ~prime_sel:false ~addr_a:12 ~addr_b:0 ~addr_out:13 then begin
  let exp = Z.(erem (registers.(12) - of_int 1000) prime_p) in
  if not (Z.equal registers.(13) exp) then mixed_pass := false;
  Stdio.printf "    result=%s %s\n" (Z.to_string registers.(13))
    (if Z.equal registers.(13) exp then "✓" else "✗")
end else mixed_pass := false;

(* Mul *)
Stdio.printf "  Op 5 (mul): r[14] = r[13] * r[1]\n";
if run_op ~op:2 ~prime_sel:false ~addr_a:13 ~addr_b:1 ~addr_out:14 then begin
  let exp = Z.((registers.(13) * of_int 2000) mod prime_p) in
  if not (Z.equal registers.(14) exp) then mixed_pass := false;
  Stdio.printf "    result=%s %s\n" (Z.to_string registers.(14))
    (if Z.equal registers.(14) exp then "✓" else "✗")
end else mixed_pass := false;

Stdio.printf "  Mixed operation test: %s\n\n" 
  (if !mixed_pass then "PASS ✓" else "FAIL ✗");
record !mixed_pass;
  
(* ============================================== *)
(* TRUE BACK-TO-BACK TEST (no reset between ops) *)
(* ============================================== *)

Stdio.printf "=== True Back-to-Back Test (no reset) ===\n\n";

(* Do a single reset at the start *)
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;

(* Setup register data *)
let val_a = Z.of_int 12345 in
let val_b = Z.of_int 67890 in
let val_c = Z.of_int 11111 in
let val_d = Z.of_int 22222 in

(* Helper to run op WITHOUT reset *)
let run_op_no_reset ~op ~data_a ~data_b =
  inputs.start := Bits.vdd;
  inputs.op := Bits.of_int ~width:2 op;
  inputs.prime_sel := Bits.gnd;
  inputs.addr_a := Bits.zero 5;
  inputs.addr_b := Bits.zero 5;
  inputs.addr_out := Bits.zero 5;
  inputs.reg_read_data_a := z_to_bits data_a;
  inputs.reg_read_data_b := z_to_bits data_b;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  let max_cycles = 3000 in
  let rec wait n =
    if n >= max_cycles then begin
      Stdio.printf "  TIMEOUT after %d cycles\n" max_cycles;
      None
    end else if Bits.to_bool !(outputs.done_) then begin
      let result = bits_to_z !(outputs.reg_write_data) in
      Some result
    end else begin
      Cyclesim.cycle sim;
      wait (n + 1)
    end
  in
  wait 0
in

Stdio.printf "Op 1: 12345 * 67890\n";
let result1 = run_op_no_reset ~op:2 ~data_a:val_a ~data_b:val_b in
(match result1 with
| Some r -> 
    let expected = Z.((val_a * val_b) mod prime_p) in
    let pass = Z.equal r expected in
    Stdio.printf "  result=%s expected=%s %s\n" 
      (Z.to_string r) (Z.to_string expected) (if pass then "✓" else "✗")
| None -> ());

Stdio.printf "Op 2 (immediate): 11111 * 22222\n";
let result2 = run_op_no_reset ~op:2 ~data_a:val_c ~data_b:val_d in
(match result2 with
| Some r -> 
    let expected = Z.((val_c * val_d) mod prime_p) in
    let pass = Z.equal r expected in
    Stdio.printf "  result=%s expected=%s %s\n" 
      (Z.to_string r) (Z.to_string expected) (if pass then "✓" else "✗")
| None -> ());

let true_btb_pass = 
  match result1, result2 with
  | Some r1, Some r2 ->
      let exp1 = Z.((val_a * val_b) mod prime_p) in
      let exp2 = Z.((val_c * val_d) mod prime_p) in
      Z.equal r1 exp1 && Z.equal r2 exp2
  | _ -> false
in
Stdio.printf "True back-to-back test: %s\n\n" 
  (if true_btb_pass then "PASS ✓" else "FAIL ✗");
record true_btb_pass;


(* ============================================== *)
(* POINT_MUL PATTERN TEST                         *)
(* ============================================== *)

Stdio.printf "=== Point_mul Pattern Test ===\n\n";

Stdio.printf "Test: Simulating Point_mul's first two operations\n";
Stdio.printf "  Op 0: t0 <- x1 * x2 (with x1=x2=0, point at infinity)\n";
Stdio.printf "  Op 1: t1 <- y1 * y2 (with y1=y2=1, point at infinity)\n\n";

(* Single reset at start *)
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;

(* Simulate Point_mul's register file state after Init:
   x1 = 0 (infinity_x)
   y1 = 1 (infinity_y)  
   z1 = 0 (infinity_z)
   When j=0 (doubling), x2=x1, y2=y1, z2=z1 *)

let pm_registers = Array.init 17 ~f:(fun _ -> Z.zero) in
pm_registers.(9) <- Z.zero;   (* x1 = 0 *)
pm_registers.(10) <- Z.one;   (* y1 = 1 *)
pm_registers.(11) <- Z.zero;  (* z1 = 0 *)
pm_registers.(12) <- Z.zero;  (* x2 = x1 = 0 (because j=0) *)
pm_registers.(13) <- Z.one;   (* y2 = y1 = 1 (because j=0) *)
pm_registers.(14) <- Z.zero;  (* z2 = z1 = 0 (because j=0) *)

let run_pm_op ~src1 ~src2 ~dst =
  (* Provide register data *)
  inputs.reg_read_data_a := z_to_bits pm_registers.(src1);
  inputs.reg_read_data_b := z_to_bits pm_registers.(src2);
  
  (* Start multiplication *)
  inputs.start := Bits.vdd;
  inputs.op := Bits.of_int ~width:2 2;  (* mul *)
  inputs.prime_sel := Bits.gnd;
  inputs.addr_a := Bits.of_int ~width:5 src1;
  inputs.addr_b := Bits.of_int ~width:5 src2;
  inputs.addr_out := Bits.of_int ~width:5 dst;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  (* Wait for completion, updating reg reads as requested *)
  let max_cycles = 300 in
  let rec wait n =
    if n >= max_cycles then begin
      Stdio.printf "    TIMEOUT after %d cycles!\n" max_cycles;
      Stdio.printf "    busy=%d done=%d\n" 
        (Bits.to_int !(outputs.busy))
        (Bits.to_int !(outputs.done_));
      None
    end else begin
      let read_addr_a = Bits.to_int !(outputs.reg_read_addr_a) in
      let read_addr_b = Bits.to_int !(outputs.reg_read_addr_b) in
      inputs.reg_read_data_a := z_to_bits pm_registers.(read_addr_a);
      inputs.reg_read_data_b := z_to_bits pm_registers.(read_addr_b);
      
      if Bits.to_bool !(outputs.done_) then begin
        let result = bits_to_z !(outputs.reg_write_data) in
        if Bits.to_bool !(outputs.reg_write_enable) then begin
          let write_addr = Bits.to_int !(outputs.reg_write_addr) in
          pm_registers.(write_addr) <- result
        end;
        Stdio.printf "    Completed in %d cycles\n" n;
        Some result
      end else begin
        Cyclesim.cycle sim;
        wait (n + 1)
      end
    end
  in
  wait 0
in

(* Op 0: t0 <- x1 * x2 = 0 * 0 = 0 *)
Stdio.printf "Op 0: t0 <- x1 * x2 (src1=9, src2=12, dst=0)\n";
Stdio.printf "  x1 = %s, x2 = %s\n" (Z.to_string pm_registers.(9)) (Z.to_string pm_registers.(12));
let op0_result = run_pm_op ~src1:9 ~src2:12 ~dst:0 in
(match op0_result with
| Some r -> 
    let expected = Z.zero in
    Stdio.printf "    result = %s, expected = %s %s\n\n" 
      (Z.to_string r) (Z.to_string expected)
      (if Z.equal r expected then "✓" else "✗")
| None -> Stdio.printf "\n");

(* Op 1: t1 <- y1 * y2 = 1 * 1 = 1 *)
Stdio.printf "Op 1: t1 <- y1 * y2 (src1=10, src2=13, dst=1)\n";
Stdio.printf "  y1 = %s, y2 = %s\n" (Z.to_string pm_registers.(10)) (Z.to_string pm_registers.(13));
let op1_result = run_pm_op ~src1:10 ~src2:13 ~dst:1 in
(match op1_result with
| Some r -> 
    let expected = Z.one in
    Stdio.printf "    result = %s, expected = %s %s\n\n" 
      (Z.to_string r) (Z.to_string expected)
      (if Z.equal r expected then "✓" else "✗")
| None -> Stdio.printf "\n");

(* Op 2: t2 <- z1 * z2 = 0 * 0 = 0 *)
Stdio.printf "Op 2: t2 <- z1 * z2 (src1=11, src2=14, dst=2)\n";
Stdio.printf "  z1 = %s, z2 = %s\n" (Z.to_string pm_registers.(11)) (Z.to_string pm_registers.(14));
let op2_result = run_pm_op ~src1:11 ~src2:14 ~dst:2 in
(match op2_result with
| Some r -> 
    let expected = Z.zero in
    Stdio.printf "    result = %s, expected = %s %s\n\n" 
      (Z.to_string r) (Z.to_string expected)
      (if Z.equal r expected then "✓" else "✗")
| None -> Stdio.printf "\n");

let pm_pattern_pass = 
  match op0_result, op1_result, op2_result with
  | Some r0, Some r1, Some r2 ->
      Z.equal r0 Z.zero && Z.equal r1 Z.one && Z.equal r2 Z.zero
  | _ -> false
in
Stdio.printf "Point_mul pattern test: %s\n\n" 
  (if pm_pattern_pass then "PASS ✓" else "FAIL ✗");
record pm_pattern_pass;

(* ============================================== *)
(* DETAILED MONTGOMERY DEBUG TEST                 *)
(* ============================================== *)

Stdio.printf "=== Detailed Montgomery Debug Test ===\n\n";

Stdio.printf "Test: Investigating 0*0 followed by 1*1 hang\n\n";

(* Single reset at start *)
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;

let run_mul_verbose ~a ~b ~label =
  Stdio.printf "%s: %s * %s\n" label (Z.to_string a) (Z.to_string b);
  
  inputs.reg_read_data_a := z_to_bits a;
  inputs.reg_read_data_b := z_to_bits b;
  inputs.start := Bits.vdd;
  inputs.op := Bits.of_int ~width:2 2;  (* mul *)
  inputs.prime_sel := Bits.gnd;
  inputs.addr_a := Bits.zero 5;
  inputs.addr_b := Bits.zero 5;
  inputs.addr_out := Bits.zero 5;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  
  for cycle = 1 to 20 do
    let busy = Bits.to_int !(outputs.busy) in
    let done_ = Bits.to_int !(outputs.done_) in
    Stdio.printf "  cycle %2d: busy=%d done=%d\n" cycle busy done_;
    
    if done_ = 1 then begin
      let result = bits_to_z !(outputs.reg_write_data) in
      Stdio.printf "  Result: %s\n\n" (Z.to_string result);
      (* Cycle once more to clear done state *)
      Cyclesim.cycle sim;
      ()
    end else
      Cyclesim.cycle sim
  done
in

(* Test 1: Start fresh with 1*1 - should work *)
Stdio.printf "--- Test A: Fresh start with 1*1 ---\n";
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;
run_mul_verbose ~a:Z.one ~b:Z.one ~label:"Op";

(* Test 2: Fresh start with 0*0 then 1*1 *)
Stdio.printf "--- Test B: Fresh 0*0 then 1*1 ---\n";
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;
run_mul_verbose ~a:Z.zero ~b:Z.zero ~label:"Op1 (0*0)";
run_mul_verbose ~a:Z.one ~b:Z.one ~label:"Op2 (1*1)";

(* Test 3: Fresh start with 0*0 then 2*3 *)
Stdio.printf "--- Test C: Fresh 0*0 then 2*3 ---\n";
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;
run_mul_verbose ~a:Z.zero ~b:Z.zero ~label:"Op1 (0*0)";
run_mul_verbose ~a:(Z.of_int 2) ~b:(Z.of_int 3) ~label:"Op2 (2*3)";

(* Test 4: Fresh start with 5*7 then 1*1 *)
Stdio.printf "--- Test D: Fresh 5*7 then 1*1 ---\n";
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;
run_mul_verbose ~a:(Z.of_int 5) ~b:(Z.of_int 7) ~label:"Op1 (5*7)";
run_mul_verbose ~a:Z.one ~b:Z.one ~label:"Op2 (1*1)";

(* Test 5: Fresh start with 5*7 then 0*0 then 1*1 *)
Stdio.printf "--- Test E: Fresh 5*7 then 0*0 then 1*1 ---\n";
inputs.clear := Bits.vdd;
Cyclesim.cycle sim;
inputs.clear := Bits.gnd;
Cyclesim.cycle sim;
run_mul_verbose ~a:(Z.of_int 5) ~b:(Z.of_int 7) ~label:"Op1 (5*7)";
run_mul_verbose ~a:Z.zero ~b:Z.zero ~label:"Op2 (0*0)";
run_mul_verbose ~a:Z.one ~b:Z.one ~label:"Op3 (1*1)";

Stdio.printf "=== End Montgomery Debug Test ===\n\n";

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