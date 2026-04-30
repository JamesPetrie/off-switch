(* test_merkle_verify.ml - Merkle authentication path verification test *)
open Hardcaml
open Hardcaml.Bits

let printf = Printf.printf

let hex_to_bits ~width hex_str =
  of_hex ~signedness:Unsigned ~width hex_str

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

(* Test vectors from reference_lms.py *)
let identifier_hex = "01010101010101010101010101010101"
let q = 5
let tree_height = 4
let leaf_hash_hex = "fb8d108876e60082dc3329945d02017cd361903d3c384af6d7a55f34e2e47a36"
let merkle_root_hex = "1c2e06bc6fe6bc3f7c6e7fc998277f82fd8fa2f79aa26eb1d3c66999083d4ce5"

let auth_path_hex = [|
  "88e8870b6c5c462d0b438df1d9ec4fbf0da2dc0968b7908c943840d19342f63b";
  "bb2e3fd7fa4a51914aec2b24dafcdf68c292f2b019021f47cc2320dbf8174bb9";
  "04e412c0e49cf794208062b8917d60d21d4410f738792bf3f3e2472a0de192ba";
  "ae749d878fe57433df878f2b4155c32cfec2358d46b64d8bca3759e5802a6a2d";
|]

let () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Merkle_verify.I)(Merkle_verify.O) in
  let sim = Sim.create (Merkle_verify.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in

  (* Reset *)
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;

  printf "=== Merkle Path Verification Test ===\n\n";

  inputs.leaf_hash := hex_to_bits ~width:256 leaf_hash_hex;
  inputs.leaf_index := of_int ~width:32 q;
  inputs.tree_height := of_int ~width:6 tree_height;
  inputs.identifier := hex_to_bits ~width:128 identifier_hex;

  inputs.start := vdd;
  Cyclesim.cycle sim;
  inputs.start := gnd;

  let max_cycles = 10_000 in
  let cycle_count = ref 0 in
  let nodes_provided = ref 0 in

  while Bits.to_int !(outputs.done_) = 0 && !cycle_count < max_cycles do
    if Bits.to_int !(outputs.request_auth_node) = 1 then begin
      let level = Bits.to_int !(outputs.level) in
      printf "  Level %d: providing auth node %s...\n" level
        (String.sub auth_path_hex.(level) 0 16);
      inputs.auth_node := hex_to_bits ~width:256 auth_path_hex.(level);
      inputs.auth_node_valid := vdd;
      Cyclesim.cycle sim;
      inputs.auth_node_valid := gnd;
      incr cycle_count;
      incr nodes_provided;
    end else begin
      Cyclesim.cycle sim;
      incr cycle_count;
    end
  done;

  printf "\n  Total cycles: %d\n" !cycle_count;
  printf "  Auth nodes provided: %d\n" !nodes_provided;

  let done_ = Bits.to_int !(outputs.done_) = 1 in
  printf "  Done: %b\n" done_;

  if done_ then begin
    let computed_root = bits_to_hex !(outputs.computed_root) in
    printf "  Computed root: %s\n" computed_root;
    printf "  Expected root: %s\n" merkle_root_hex;
    if String.equal computed_root merkle_root_hex then
      printf "\nMerkle verification test: PASS ✓\n"
    else
      printf "\nMerkle verification test: FAIL ✗ (root mismatch)\n"
  end else
    printf "\nMerkle verification test: FAIL ✗ (timeout)\n"
