(* merkle_verify.ml - Merkle authentication path verification

   Verifies a Merkle authentication path from leaf to root.
   Uses SHA-256 for internal node hashing per RFC 8554:
     H(I || u32str(node_num) || u16str(D_INTR) || left || right)
     = 16 + 4 + 2 + 32 + 32 = 86 bytes → requires 2 SHA-256 blocks.

   Block 1 (64 bytes = 512 bits): I(16) || node_num(4) || D_INTR(2) || left(32) || right[0:9](10)
   Block 2 (64 bytes = 512 bits): right[10:31](22) || 0x80(1) || zeros(33) || length_be(8)
                                   where length = 86*8 = 688 bits
*)

open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; leaf_hash : 'a [@bits 256]
    ; leaf_index : 'a [@bits 32]
    ; tree_height : 'a [@bits 6]
    ; identifier : 'a [@bits 128]
    (* Auth path streaming *)
    ; auth_node : 'a [@bits 256]
    ; auth_node_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { busy : 'a
    ; done_ : 'a
    ; computed_root : 'a [@bits 256]
    ; request_auth_node : 'a
    ; level : 'a [@bits 6]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
    | Request_node
    | Wait_node
    | Hash_block1       (* Feed first 512-bit block to SHA-256 *)
    | Wait_block1       (* Wait for block 1 digest *)
    | Hash_block2       (* Feed second 512-bit block *)
    | Wait_block2       (* Wait for block 2 digest = final hash *)
    | Advance_level
    | Done_state
  [@@deriving sexp_of, compare, equal, enumerate]
end

let create scope (i : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in

  (* SHA-256 core *)
  let sha_init = Variable.wire ~default:gnd in
  let sha_next = Variable.wire ~default:gnd in
  let sha_block = Variable.wire ~default:(zero 512) in

  let sha = Sha256_core.create (Scope.sub_scope scope "sha256")
    { Sha256_core.I.
      clock = i.clock
    ; clear = i.clear
    ; init = sha_init.value
    ; next = sha_next.value
    ; block = sha_block.value
    }
  in

  (* State registers *)
  let current_node = Variable.reg spec ~width:256 in
  let current_level = Variable.reg spec ~width:6 in
  let node_index = Variable.reg spec ~width:32 in
  let sibling = Variable.reg spec ~width:256 in
  let is_right_child = Variable.reg spec ~width:1 in

  let request_node = Variable.wire ~default:gnd in
  let done_signal = Variable.wire ~default:gnd in

  let d_intr = of_int ~width:16 0x8383 in
  let parent_num = srl node_index.value 1 in

  (* Determine left and right children *)
  let left_node = mux2 is_right_child.value sibling.value current_node.value in
  let right_node = mux2 is_right_child.value current_node.value sibling.value in

  (* Construct hash blocks for: I(128) || parent(32) || D_INTR(16) || left(256) || right(256)
     Total message: 688 bits = 86 bytes

     Block 1 (512 bits): I(128) || parent(32) || D_INTR(16) || left(256) || right[255:176](80)
       128 + 32 + 16 + 256 + 80 = 512 ✓

     Block 2 (512 bits): right[175:0](176) || 0x80(8) || zeros(264) || length(64)
       176 + 8 + 264 + 64 = 512 ✓
  *)
  let block1 =
    i.identifier @: parent_num @: d_intr @: left_node @: right_node.:[(255, 176)]
  in
  let block2 =
    right_node.:[(175, 0)] @: (of_int ~width:8 0x80) @: (zero 264) @: (of_int ~width:64 688)
  in

  compile [
    sm.switch [
      State.Idle, [
        when_ i.start [
          current_node <-- i.leaf_hash;
          current_level <--. 0;
          node_index <-- ((log_shift sll (of_int ~width:32 1) i.tree_height) +: i.leaf_index);
          sm.set_next Request_node;
        ];
      ];

      State.Request_node, [
        if_ (current_level.value >=: uresize i.tree_height 6) [
          done_signal <-- vdd;
          sm.set_next Done_state;
        ] [
          request_node <-- vdd;
          is_right_child <-- node_index.value.:[(0, 0)];
          sm.set_next Wait_node;
        ];
      ];

      State.Wait_node, [
        when_ i.auth_node_valid [
          sibling <-- i.auth_node;
          sm.set_next Hash_block1;
        ];
      ];

      State.Hash_block1, [
        sha_block <-- block1;
        sha_init <-- vdd;
        sm.set_next Wait_block1;
      ];

      State.Wait_block1, [
        when_ sha.digest_valid [
          sm.set_next Hash_block2;
        ];
      ];

      State.Hash_block2, [
        when_ sha.ready [
          sha_block <-- block2;
          sha_next <-- vdd;
          sm.set_next Wait_block2;
        ];
      ];

      State.Wait_block2, [
        when_ sha.digest_valid [
          current_node <-- sha.digest;
          node_index <-- parent_num;
          current_level <-- current_level.value +:. 1;
          sm.set_next Advance_level;
        ];
      ];

      State.Advance_level, [
        sm.set_next Request_node;
      ];

      State.Done_state, [
        sm.set_next Idle;
      ];
    ];
  ];

  { O.
    busy = ~:(sm.is Idle)
  ; done_ = done_signal.value
  ; computed_root = current_node.value
  ; request_auth_node = request_node.value
  ; level = current_level.value
  }
