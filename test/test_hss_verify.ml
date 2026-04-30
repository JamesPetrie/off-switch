(* test_hss_verify.ml - End-to-end HSS/LMS signature verification test

   Tests the full HSS orchestrator: Q hash → WOTS+ chains → Kc accumulation
   → leaf hash → Merkle path verification → root comparison.

   Test vectors from reference_lms.py.
*)

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
let message_hex = "0b98309ccea6343bf486b4b04ec7d7b7a5b5adda1edf46e43a15b5e99edc21b4"
let c_hex = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
let merkle_root_hex = "1c2e06bc6fe6bc3f7c6e7fc998277f82fd8fa2f79aa26eb1d3c66999083d4ce5"
let tree_height = 4

let signature_hex = [|
  "3a480283407063ca6a57ad7201dce50eed9d7ffe3eb8eeff0d8729ab6c69138e";
  "bda0576ef69ede2a299e5909cdf76c4beb21c752584ed14aeaab9a832176ad82";
  "ad53eea01adbc6cc2843dd2339e21bc3db8d7631fafc3c90172b9e349afc00fd";
  "3b49a4edd1a859efde2412d990101c5295c43a747e0bfedaf6316ce9dfa2df37";
  "a31c08a41d14877e0900e1edfcfd9cda520d2099be46a19fdfd98314c97b7a6c";
  "ed985ecad9eff85bb39578751b35ac960605155f7087f1bd6e93c54a4c475431";
  "b0a8911e31adc20f59baed39d27cfea6263a904f57313d394870edd2fccea3c4";
  "e905292f112d21bbdf2f09e512f81e20321974d3b89f6b37a1f60dd99f7382db";
  "8646c92a0c0e4b5c483dbd8cf27b93406bcc80fb53aa252115a5542350bee06b";
  "b2e5aef13580eea74b5d923672f6cc6e7aa84a62bd90cd673a501a6897c49acd";
  "9f57d5bd1a6f74584f7f60ffea7a9d06c99377783b09868a515b884230b7c6a9";
  "b9a0e9c4dc02ea6619a0353fb0e5b30dfad2038a5b3b6736a03f97cd8de79854";
  "dc86e794edbb8a7103ab5b2837e1f4c1bab260d6399bb4d8e698bffbba5f2663";
  "ca689a94cb780d89cf76906b98c88bbd2a953d65eb9200695b8d7d937678c9f5";
  "8294eee9ee409fc48839b6eaae7f3f6dab91bdc039879333754a8b58d636929c";
  "10dd2807334d8df653995b3bab70d66f79e5ba2a5ef751992706c7aad52989f2";
  "b41f98dea875f68edffece8623e2de6225f9e2ddd8e982d5f11342371662bc51";
  "747bdb5fb0b81e3b0db585e6d0065024cf2146a5a21f76686da03e8faf850a79";
  "848544d345860ab1e2d8518a45dd26dcd924d74b088b8ef21e73d59dc996adcb";
  "a88c0b422826e37d1815eb1e6b3da2129e26c0b992fd4f0112c362a160b7b59e";
  "fe117086153eae51639f815ce11aa2df07f969b37a6712879499da1e445bcb63";
  "3eff23af41275547be3cb3021b57872ef1c9855c323bb3826afccc7a4dfbcf2c";
  "be723107964340550c5d5e79a37fbf9c893ad88060310387dfbfb12800b28080";
  "3bc358a50ac437946044353efc8922f1925dc4b05f43979fdf8e03b3045493e0";
  "771c28c482520e0fd639e0076e2c6b2d054124f90acb2690154555116e8d796f";
  "5cbfcc9e4287814f56573038875ba5539ea72c028636f55489c014cee30df046";
  "b0086d805aa6698d8d0e25dcc26e5a6924c4842ece8666fd1dfaa8feaabf236d";
  "59ccc128fc44877abbf74b50ec940f73ef1a405d36b1917e50bbcaac0c044db7";
  "8fb3c75fc722075b140236c472062befafd6e27fc8f9e9eb75cff151b29e957c";
  "75e2b540b4eee1d133180c386658833d5ebcd0655d7b3f5d53dd547c4c050d90";
  "9ff47e0c4d3410a85214ddba0b769d8af436eb54943a2e788f076d8a257d525b";
  "eb3e49cc6204bf7b4493a756bd5a3eb9551fd2009464b3161f2f7917c83049b5";
  "e7b8728e14a0c7e19b0881311b01383329e4a64a7bf88cb1de2a002c547da905";
  "ebd37232f7e264d3fce5c4731cbcfcf83a1080380f4a53d0a48a634b98632f01";
|]

let auth_path_hex = [|
  "88e8870b6c5c462d0b438df1d9ec4fbf0da2dc0968b7908c943840d19342f63b";
  "bb2e3fd7fa4a51914aec2b24dafcdf68c292f2b019021f47cc2320dbf8174bb9";
  "04e412c0e49cf794208062b8917d60d21d4410f738792bf3f3e2472a0de192ba";
  "ae749d878fe57433df878f2b4155c32cfec2358d46b64d8bca3759e5802a6a2d";
|]

let () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface(Hss_verify.I)(Hss_verify.O) in
  let sim = Sim.create (Hss_verify.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in

  (* Reset *)
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;

  printf "=== HSS/LMS End-to-End Verification Test ===\n\n";

  (* Set up static inputs *)
  inputs.identifier := hex_to_bits ~width:128 identifier_hex;
  inputs.leaf_index := of_int ~width:32 q;
  inputs.tree_height := of_int ~width:6 tree_height;
  inputs.root_pub_key := hex_to_bits ~width:256 merkle_root_hex;
  inputs.message := hex_to_bits ~width:256 message_hex;
  inputs.randomizer := hex_to_bits ~width:256 c_hex;

  (* Start verification *)
  inputs.start := vdd;
  Cyclesim.cycle sim;
  inputs.start := gnd;

  (* Run the verification loop *)
  let max_cycles = 2_000_000 in
  let cycle_count = ref 0 in
  let chains_fed = ref 0 in
  let auth_nodes_fed = ref 0 in

  while Bits.to_int !(outputs.done_) = 0 && !cycle_count < max_cycles do
    (* Check if WOTS is requesting a signature element *)
    if Bits.to_int !(outputs.request_sig_element) = 1 then begin
      let chain_idx = Bits.to_int !(outputs.sig_element_index) in
      if chain_idx < 34 then begin
        inputs.sig_element := hex_to_bits ~width:256 signature_hex.(chain_idx);
        inputs.sig_element_valid := vdd;
        Cyclesim.cycle sim;
        inputs.sig_element_valid := gnd;
        incr cycle_count;
        incr chains_fed;
        if !chains_fed <= 3 || !chains_fed = 34 then
          printf "  WOTS chain %d: loaded\n" chain_idx;
      end else begin
        Cyclesim.cycle sim;
        incr cycle_count;
      end
    end
    (* Check if Merkle is requesting an auth node *)
    else if Bits.to_int !(outputs.request_auth_node) = 1 then begin
      let level = Bits.to_int !(outputs.auth_level) in
      if level < tree_height then begin
        printf "  Merkle level %d: providing auth node\n" level;
        inputs.auth_node := hex_to_bits ~width:256 auth_path_hex.(level);
        inputs.auth_node_valid := vdd;
        Cyclesim.cycle sim;
        inputs.auth_node_valid := gnd;
        incr cycle_count;
        incr auth_nodes_fed;
      end else begin
        Cyclesim.cycle sim;
        incr cycle_count;
      end
    end
    else begin
      Cyclesim.cycle sim;
      incr cycle_count;
    end
  done;

  printf "\n  Total cycles: %d\n" !cycle_count;
  printf "  Signature chains fed: %d\n" !chains_fed;
  printf "  Auth nodes fed: %d\n" !auth_nodes_fed;

  let valid = Bits.to_int !(outputs.valid) = 1 in
  let done_ = Bits.to_int !(outputs.done_) = 1 in
  printf "  Done: %b\n" done_;
  printf "  Valid: %b\n" valid;

  if done_ && valid then
    printf "\nHSS/LMS verification test: PASS\n"
  else
    printf "\nHSS/LMS verification test: FAIL\n"
