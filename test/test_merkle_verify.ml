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
let leaf_hash_hex = "11f0311e47469420b5d8d4449ef1f8bbf71c49f0e867269f1a5a0fc0eb46d90c"
let merkle_root_hex = "ae247eab7668d68995dfd7ad9c8fa2fc9c3ab078ea2a68b5e3013346ed394a37"

let auth_path_hex = [|
  "16a07337b0e9698fba159da2615cd486c9aff5c3c158a17afb83c5fc7a7893f8";
  "73606768f67c693edc536ff1c75f233ee9e597bc83dc089d9acdb8c772ec449c";
  "71cb251154f21ba3ce401ffbc63c430ea98eccf5a3b9b7f3a70e872958ba7ea2";
  "66f7d9a0d2c34f8eeedd0e73a75b39518bbfed788ebe83103bcc97b8cb1b09a2";
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
