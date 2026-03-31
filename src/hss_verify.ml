(* hss_verify.ml - HSS/LMS signature verification orchestrator

   Top-level verification for L=1 HSS (single Merkle tree) per RFC 8554.
   Orchestrates:
   1. Q = H(I || q || D_MESG || C || message)  -- 2-block SHA-256 (86 bytes)
   2. WOTS+ chain computation: 34 chains, producing pk_candidate[0..33]
   3. Kc = H(I || q || D_PBLC || pk0 || ... || pk33) -- 18-block incremental SHA-256 (1110 bytes)
   4. leaf_hash = H(I || q || D_LEAF || Kc) -- 1-block SHA-256 (54 bytes)
   5. Merkle path verification: leaf_hash to root

   Uses own SHA-256 core for Q, Kc, leaf_hash. WOTS and Merkle have their own cores.

   Kc block structure (1110 bytes = 18 blocks):
   Block 0:  prefix(176) || pk0(256) || pk1_hi(80)  where prefix = I(128)||q(32)||D_PBLC(16)
   Block n:  pk_{2n-1}_lo(176) || pk_{2n}(256) || pk_{2n+1}_hi(80)  for n=1..16
   Block 17: pk33_lo(176) || 0x80(8) || zeros(264) || length_8880(64)
*)

open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; identifier : 'a [@bits 128]
    ; leaf_index : 'a [@bits 32]
    ; tree_height : 'a [@bits 6]
    ; root_pub_key : 'a [@bits 256]
    ; message : 'a [@bits 256]
    ; randomizer : 'a [@bits 256]
    ; sig_element : 'a [@bits 256]
    ; sig_element_valid : 'a
    ; auth_node : 'a [@bits 256]
    ; auth_node_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { busy : 'a
    ; done_ : 'a
    ; valid : 'a
    ; request_sig_element : 'a
    ; sig_element_index : 'a [@bits 6]
    ; request_auth_node : 'a
    ; auth_level : 'a [@bits 6]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
    | Q_block1 | Q_wait1 | Q_block2 | Q_wait2
    | Wots_start | Wots_run
    | Kc_wait_block
    | Kc_finalize | Kc_wait_final
    | Leaf_hash | Leaf_wait
    | Merkle_start | Merkle_run
    | Compare | Done_state
  [@@deriving sexp_of, compare, equal, enumerate]
end

let d_mesg = of_int ~width:16 0x8181
let d_pblc = of_int ~width:16 0x8080
let d_leaf = of_int ~width:16 0x8282

let create scope (i : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in

  (* SHA-256 core for Q, Kc, leaf hash *)
  let sha_init = Variable.wire ~default:gnd in
  let sha_next = Variable.wire ~default:gnd in
  let sha_block = Variable.wire ~default:(zero 512) in

  let sha = Sha256_core.create (Scope.sub_scope scope "sha256_main")
    { Sha256_core.I.
      clock = i.clock; clear = i.clear
    ; init = sha_init.value; next = sha_next.value; block = sha_block.value
    }
  in

  (* WOTS+ engine with pk flow control *)
  let wots_start = Variable.wire ~default:gnd in
  let pk_ack = Variable.wire ~default:gnd in
  let q_hash_reg = Variable.reg spec ~width:256 in

  let wots = Wots_verify.create (Scope.sub_scope scope "wots")
    { Wots_verify.I.
      clock = i.clock; clear = i.clear
    ; start = wots_start.value
    ; message_hash = q_hash_reg.value
    ; expected_kc = zero 256
    ; identifier = i.identifier
    ; leaf_index = i.leaf_index
    ; sig_element = i.sig_element
    ; sig_element_valid = i.sig_element_valid
    ; pk_ack = pk_ack.value
    }
  in

  (* Merkle engine *)
  let merkle_start = Variable.wire ~default:gnd in
  let leaf_hash_reg = Variable.reg spec ~width:256 in

  let merkle = Merkle_verify.create (Scope.sub_scope scope "merkle")
    { Merkle_verify.I.
      clock = i.clock; clear = i.clear
    ; start = merkle_start.value
    ; leaf_hash = leaf_hash_reg.value
    ; leaf_index = i.leaf_index
    ; tree_height = i.tree_height
    ; identifier = i.identifier
    ; auth_node = i.auth_node
    ; auth_node_valid = i.auth_node_valid
    }
  in

  (* Kc accumulation state *)
  let kc_buf_lo = Variable.reg spec ~width:176 in     (* prefix or previous pk's low 22 bytes *)
  let kc_buf_even = Variable.reg spec ~width:256 in    (* stored even-indexed pk element *)
  let kc_pk_count = Variable.reg spec ~width:6 in      (* 0..34: pk elements consumed *)
  let kc_is_first = Variable.reg spec ~width:1 in      (* 1 if next Kc block uses init *)
  let kc_computed = Variable.reg spec ~width:256 in

  (* Result *)
  let result_valid = Variable.reg spec ~width:1 in
  let done_signal = Variable.wire ~default:gnd in

  (* Q hash blocks: H(I || q || D_MESG || C || message) = 86 bytes → 2 blocks *)
  let q_block1 =
    i.identifier @: i.leaf_index @: d_mesg @: i.randomizer @: i.message.:[(255, 176)]
  in
  let q_block2 =
    i.message.:[(175, 0)] @: (of_int ~width:8 0x80) @: (zero 264) @: (of_int ~width:64 688)
  in

  (* Leaf hash block: H(I || q || D_LEAF || Kc) = 54 bytes → 1 block *)
  let leaf_block =
    i.identifier @: i.leaf_index @: d_leaf @: kc_computed.value
    @: (of_int ~width:8 0x80) @: (zero 8) @: (of_int ~width:64 432)
  in

  (* Kc data block: prefix/pk_low(176) || pk_even(256) || pk_odd_hi(80) *)
  let kc_data_block =
    kc_buf_lo.value @: kc_buf_even.value @: wots.pk_candidate.:[(255, 176)]
  in

  (* Kc final padding block: pk_low(176) || 0x80(8) || zeros(264) || length(64) *)
  let kc_final_block =
    kc_buf_lo.value @: (of_int ~width:8 0x80) @: (zero 264) @: (of_int ~width:64 8880)
  in

  compile [
    sm.switch [
      State.Idle, [
        when_ i.start [
          result_valid <--. 0;
          kc_pk_count <--. 0;
          kc_is_first <--. 1;
          (* Initialize Kc prefix: I || q || D_PBLC = 176 bits *)
          kc_buf_lo <-- (i.identifier @: i.leaf_index @: d_pblc);
          sm.set_next Q_block1;
        ];
      ];

      (* Phase 1: Compute Q *)
      State.Q_block1, [
        sha_block <-- q_block1;
        sha_init <-- vdd;
        sm.set_next Q_wait1;
      ];
      State.Q_wait1, [
        when_ sha.digest_valid [ sm.set_next Q_block2; ];
      ];
      State.Q_block2, [
        sha_block <-- q_block2;
        sha_next <-- vdd;
        sm.set_next Q_wait2;
      ];
      State.Q_wait2, [
        when_ sha.digest_valid [
          q_hash_reg <-- sha.digest;
          sm.set_next Wots_start;
        ];
      ];

      (* Phase 2+3: WOTS chains + Kc accumulation *)
      State.Wots_start, [
        wots_start <-- vdd;
        sm.set_next Wots_run;
      ];

      State.Wots_run, [
        when_ wots.pk_candidate_valid [
          if_ (kc_pk_count.value.:[(0, 0)] ==:. 0) [
            (* Even pk: just store it, acknowledge immediately *)
            kc_buf_even <-- wots.pk_candidate;
            kc_pk_count <-- kc_pk_count.value +:. 1;
            pk_ack <-- vdd;
          ] [
            (* Odd pk: construct and feed Kc block *)
            sha_block <-- kc_data_block;
            if_ kc_is_first.value [
              sha_init <-- vdd;
            ] [
              sha_next <-- vdd;
            ];
            kc_buf_lo <-- wots.pk_candidate.:[(175, 0)];
            kc_is_first <--. 0;
            kc_pk_count <-- kc_pk_count.value +:. 1;
            pk_ack <-- vdd;
            sm.set_next Kc_wait_block;
          ];
        ];
        (* If WOTS done and all 34 pk's consumed, finalize Kc *)
        when_ (wots.done_ &: (kc_pk_count.value >=:. 34)) [
          sm.set_next Kc_finalize;
        ];
      ];

      State.Kc_wait_block, [
        when_ sha.digest_valid [
          if_ (kc_pk_count.value >=:. 34) [
            sm.set_next Kc_finalize;
          ] [
            sm.set_next Wots_run;
          ];
        ];
      ];

      (* Phase 3b: Kc final padding block *)
      State.Kc_finalize, [
        sha_block <-- kc_final_block;
        sha_next <-- vdd;
        sm.set_next Kc_wait_final;
      ];
      State.Kc_wait_final, [
        when_ sha.digest_valid [
          kc_computed <-- sha.digest;
          sm.set_next Leaf_hash;
        ];
      ];

      (* Phase 4: Leaf hash *)
      State.Leaf_hash, [
        sha_block <-- leaf_block;
        sha_init <-- vdd;
        sm.set_next Leaf_wait;
      ];
      State.Leaf_wait, [
        when_ sha.digest_valid [
          leaf_hash_reg <-- sha.digest;
          sm.set_next Merkle_start;
        ];
      ];

      (* Phase 5: Merkle verification *)
      State.Merkle_start, [
        merkle_start <-- vdd;
        sm.set_next Merkle_run;
      ];
      State.Merkle_run, [
        when_ merkle.done_ [
          if_ (merkle.computed_root ==: i.root_pub_key) [
            result_valid <--. 1;
          ] [
            result_valid <--. 0;
          ];
          sm.set_next Compare;
        ];
      ];

      State.Compare, [
        sm.set_next Done_state;
      ];
      State.Done_state, [
        done_signal <-- vdd;
        sm.set_next Idle;
      ];
    ];
  ];

  { O.
    busy = ~:(sm.is Idle)
  ; done_ = done_signal.value
  ; valid = result_valid.value
  ; request_sig_element = wots.request_sig_element
  ; sig_element_index = wots.chain_index
  ; request_auth_node = merkle.request_auth_node
  ; auth_level = merkle.level
  }
