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
        let read_addr_a = Bits.to_int !(outputs.reg_read_addr_a) in
        let read_addr_b = Bits.to_int !(outputs.reg_read_addr_b) in
        inputs.reg_read_data_a := z_to_bits registers.(read_addr_a);
        inputs.reg_read_data_b := z_to_bits registers.(read_addr_b);
        
        Cyclesim.cycle sim;
        
        if Bits.to_bool !(outputs.reg_write_enable) then begin
          let write_addr = Bits.to_int !(outputs.reg_write_addr) in
          let write_data = bits_to_z !(outputs.reg_write_data) in
          registers.(write_addr) <- write_data
        end;
        
        if Bits.to_bool !(outputs.done_) then true
        else wait (n + 1)
      end
    in
    wait 0
  in
  
  let prime_p = Arith.Config.prime_p in
  
  (* Initialize some registers *)
  registers.(0) <- Z.of_int 100;
  registers.(1) <- Z.of_int 200;
  registers.(2) <- Z.of_int 50;
  registers.(3) <- Z.of_string "12345678901234567890";
  
  reset ();
  
  (* Test addition: r[10] = r[0] + r[1] mod p *)
  Stdio.printf "Test: r[10] = r[0] + r[1] mod p\n";
  Stdio.printf "  r[0] = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  r[1] = %s\n" (Z.to_string registers.(1));
  if run_op ~op:0 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:10 then begin
    let expected = Z.((registers.(0) + registers.(1)) mod prime_p) in
    Stdio.printf "  r[10] = %s\n" (Z.to_string registers.(10));
    Stdio.printf "  Expected: %s\n" (Z.to_string expected);
    Stdio.printf "  %s\n\n" (if Z.equal registers.(10) expected then "PASS ✓" else "FAIL ✗")
  end;
  
  (* Test subtraction: r[11] = r[0] - r[1] mod p *)
  Stdio.printf "Test: r[11] = r[0] - r[1] mod p\n";
  Stdio.printf "  r[0] = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  r[1] = %s\n" (Z.to_string registers.(1));
  if run_op ~op:1 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:11 then begin
    let expected = Z.(erem (registers.(0) - registers.(1)) prime_p) in
    Stdio.printf "  r[11] = %s\n" (Z.to_string registers.(11));
    Stdio.printf "  Expected: %s\n" (Z.to_string expected);
    Stdio.printf "  %s\n\n" (if Z.equal registers.(11) expected then "PASS ✓" else "FAIL ✗")
  end;
  
  (* Test multiplication: r[12] = r[0] * r[1] mod p *)
  Stdio.printf "Test: r[12] = r[0] * r[1] mod p\n";
  Stdio.printf "  r[0] = %s\n" (Z.to_string registers.(0));
  Stdio.printf "  r[1] = %s\n" (Z.to_string registers.(1));
  if run_op ~op:2 ~prime_sel:false ~addr_a:0 ~addr_b:1 ~addr_out:12 then begin
    let expected = Z.((registers.(0) * registers.(1)) mod prime_p) in
    Stdio.printf "  r[12] = %s\n" (Z.to_string registers.(12));
    Stdio.printf "  Expected: %s\n" (Z.to_string expected);
    Stdio.printf "  %s\n\n" (if Z.equal registers.(12) expected then "PASS ✓" else "FAIL ✗")
  end;
  
  (* Test inversion: r[13] = r[0]^(-1) mod p *)
  Stdio.printf "Test: r[13] = r[0]^(-1) mod p\n";
  Stdio.printf "  r[0] = %s\n" (Z.to_string registers.(0));
  if run_op ~op:3 ~prime_sel:false ~addr_a:0 ~addr_b:0 ~addr_out:13 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  r[13] = %s\n" (Z.to_string registers.(13));
    if inv_exists then begin
      let product = Z.((registers.(0) * registers.(13)) mod prime_p) in
      Stdio.printf "  Verification: r[0] * r[13] mod p = %s\n" (Z.to_string product);
      Stdio.printf "  %s\n\n" (if Z.equal product Z.one then "PASS ✓" else "FAIL ✗")
    end
  end;
  
  (* Test with larger number *)
  Stdio.printf "Test: r[14] = r[3]^(-1) mod p (larger number)\n";
  Stdio.printf "  r[3] = %s\n" (Z.to_string registers.(3));
  if run_op ~op:3 ~prime_sel:false ~addr_a:3 ~addr_b:0 ~addr_out:14 then begin
    let inv_exists = Bits.to_bool !(outputs.inv_exists) in
    Stdio.printf "  Inverse exists: %b\n" inv_exists;
    Stdio.printf "  r[14] = %s\n" (Z.to_string registers.(14));
    if inv_exists then begin
      let product = Z.((registers.(3) * registers.(14)) mod prime_p) in
      Stdio.printf "  Verification: r[3] * r[14] mod p = %s\n" (Z.to_string product);
      Stdio.printf "  %s\n" (if Z.equal product Z.one then "PASS ✓" else "FAIL ✗")
    end
  end