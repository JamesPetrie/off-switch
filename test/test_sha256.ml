(* test_sha256.ml - SHA-256 core tests against NIST vectors *)

open Hardcaml
open Hardcaml.Bits

let printf = Printf.printf

(* Convert a hex string to a Bits.t of given width *)
let hex_to_bits ~width hex_str =
  let hex_str = String.lowercase_ascii hex_str in
  let nibbles = String.length hex_str in
  let bits_needed = nibbles * 4 in
  let full = of_hex ~signedness:Unsigned ~width:bits_needed hex_str in
  if bits_needed > width then Bits.select full (width - 1) 0
  else if bits_needed < width then uresize full width
  else full

(* Convert Bits.t to hex string *)
let bits_to_hex b =
  let width = Bits.width b in
  let num_nibbles = (width + 3) / 4 in
  let s = Buffer.create num_nibbles in
  let i = ref (width - 1) in
  while !i >= 0 do
    let nibble_val = ref 0 in
    for bit_pos = 3 downto 0 do
      if !i >= 0 then begin
        if Bits.to_int (Bits.select b !i !i) = 1 then
          nibble_val := !nibble_val lor (1 lsl bit_pos);
        decr i
      end
    done;
    Buffer.add_char s (Printf.sprintf "%x" !nibble_val).[0]
  done;
  Buffer.contents s

(* Build a padded SHA-256 block for a message.
   For messages <= 55 bytes, fits in one 512-bit block.
   Returns a list of 512-bit blocks. *)
let sha256_pad msg_bytes =
  let msg_len = List.length msg_bytes in
  let msg_bits = msg_len * 8 in
  (* Add 0x80 byte *)
  let padded = msg_bytes @ [0x80] in
  (* Pad with zeros until length ≡ 56 mod 64 *)
  let current_len = List.length padded in
  let target_mod = 56 in
  let zeros_needed =
    let r = current_len mod 64 in
    if r <= target_mod then target_mod - r
    else 64 - r + target_mod
  in
  let padded = padded @ List.init zeros_needed (fun _ -> 0) in
  (* Append 64-bit big-endian length *)
  let len_bytes = List.init 8 (fun i ->
    (msg_bits lsr ((7 - i) * 8)) land 0xff
  ) in
  let padded = padded @ len_bytes in
  (* Split into 512-bit (64-byte) blocks *)
  let num_blocks = List.length padded / 64 in
  List.init num_blocks (fun block_idx ->
    let block_bytes = List.init 64 (fun i ->
      List.nth padded (block_idx * 64 + i)
    ) in
    (* Convert to 512-bit value, big-endian *)
    List.fold_left (fun acc byte ->
      let acc_shifted = if Bits.width acc = 0 then Bits.empty
                        else acc in
      if Bits.is_empty acc_shifted then of_int ~width:8 byte
      else acc_shifted @: (of_int ~width:8 byte)
    ) Bits.empty block_bytes
  )

let () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Sha256_core.I)(Sha256_core.O) in
  let sim = Sim.create (Sha256_core.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in

  let passed = ref 0 in
  let total = ref 0 in

  let reset () =
    inputs.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clear := gnd
  in

  let run_hash blocks expected_hex =
    incr total;
    reset ();
    let num_blocks = List.length blocks in
    List.iteri (fun idx block ->
      if idx = 0 then begin
        inputs.init := vdd;
        inputs.block := block;
        Cyclesim.cycle sim;
        inputs.init := gnd;
      end else begin
        (* Wait for ready after previous block *)
        let wait = ref 0 in
        while Bits.to_int !(outputs.ready) = 0 && !wait < 200 do
          Cyclesim.cycle sim;
          incr wait
        done;
        inputs.next := vdd;
        inputs.block := block;
        Cyclesim.cycle sim;
        inputs.next := gnd;
        (* Extra cycle to let digest_valid clear *)
        Cyclesim.cycle sim;
      end;
      (* Wait for digest_valid *)
      let max_cycles = 200 in
      let cycles = ref 0 in
      while Bits.to_int !(outputs.digest_valid) = 0 && !cycles < max_cycles do
        Cyclesim.cycle sim;
        incr cycles
      done;
      if idx = num_blocks - 1 then begin
        let digest_hex = bits_to_hex !(outputs.digest) in
        let expected_lower = String.lowercase_ascii expected_hex in
        if String.equal digest_hex expected_lower then begin
          printf "  PASS ✓\n";
          incr passed
        end else begin
          printf "  FAIL ✗\n";
          printf "    expected: %s\n" expected_lower;
          printf "    got:      %s\n" digest_hex;
        end
      end
    ) blocks
  in

  printf "=== SHA-256 Core Test ===\n\n";

  (* Test 1: SHA-256("abc") *)
  printf "Test 1: SHA-256(\"abc\")\n";
  let blocks = sha256_pad [0x61; 0x62; 0x63] in
  run_hash blocks "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad";

  (* Test 2: SHA-256("") - empty string *)
  printf "\nTest 2: SHA-256(\"\")\n";
  let blocks = sha256_pad [] in
  run_hash blocks "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";

  (* Test 3: SHA-256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
     This is a 2-block message (56 bytes + padding crosses block boundary) *)
  printf "\nTest 3: SHA-256(\"abcdbcde...nopq\") - 2 blocks\n";
  let msg = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" in
  let msg_bytes = List.init (String.length msg) (fun i -> Char.code msg.[i]) in
  let blocks = sha256_pad msg_bytes in
  printf "  Number of blocks: %d\n" (List.length blocks);
  run_hash blocks "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1";

  (* Test 4: SHA-256("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
     Another 2-block message *)
  printf "\nTest 4: SHA-256(\"abcdefgh...rstu\") - 2 blocks\n";
  let msg = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu" in
  let msg_bytes = List.init (String.length msg) (fun i -> Char.code msg.[i]) in
  let blocks = sha256_pad msg_bytes in
  printf "  Number of blocks: %d\n" (List.length blocks);
  run_hash blocks "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1";

  printf "\n=== Test Summary ===\n";
  printf "Passed: %d/%d\n" !passed !total;
  if !passed = !total then
    printf "\nAll SHA-256 tests passed! ✓\n"
  else
    printf "\nSome tests FAILED!\n"
