(* security_block.ml - Top-level security block integration

   Implements the embedded off-switch architecture from Petrie's paper.

   Components:
   - TRNG submodule for nonce generation
   - HSS/LMS submodule for license verification (post-quantum)
   - Inline usage allowance counter
   - Inline workload with output gating

   State machine:
   - Requests nonce from TRNG at initialization
   - Publishes nonce for external license generation
   - Verifies received HSS/LMS signatures
   - Increments allowance on valid license, then generates new nonce
   - On invalid license, returns to Publish with same nonce

   Workload output is gated by ANDing each bit with (allowance > 0).
   Allowance decrements every clock cycle (time-based authorization).

   The HSS verifier uses a streaming interface: it requests signature
   elements and auth path nodes from outside via request/response signals.
*)

open Hardcaml
open Signal

module Config = struct
  let nonce_width = 256
  let allowance_width = 64
  let init_delay_cycles = 100
  let allowance_increment = 1_000_000_000_000  (* ~17 min at 1GHz *)
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    (* License interface *)
    ; license_submit : 'a
    ; license_leaf_index : 'a [@bits 32]
    ; license_randomizer : 'a [@bits 256]
    (* HSS signature streaming *)
    ; sig_element : 'a [@bits 256]
    ; sig_element_valid : 'a
    ; auth_node : 'a [@bits 256]
    ; auth_node_valid : 'a
    (* HSS public key configuration *)
    ; root_pub_key : 'a [@bits 256]
    ; identifier : 'a [@bits 128]
    ; tree_height : 'a [@bits 6]
    (* Workload interface *)
    ; workload_valid : 'a
    ; int8_a : 'a [@bits 8]
    ; int8_b : 'a [@bits 8]
    (* TRNG seed for testing *)
    ; trng_seed : 'a [@bits Config.nonce_width]
    ; trng_load_seed : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { (* Nonce output for external license generation *)
      nonce : 'a [@bits Config.nonce_width]
    ; nonce_ready : 'a
    (* Workload output *)
    ; int8_result : 'a [@bits 8]
    ; result_valid : 'a
    (* Status *)
    ; allowance : 'a [@bits Config.allowance_width]
    ; enabled : 'a
    (* HSS signature request interface *)
    ; request_sig_element : 'a
    ; sig_element_index : 'a [@bits 6]
    ; request_auth_node : 'a
    ; auth_level : 'a [@bits 6]
    (* Debug *)
    ; state_debug : 'a [@bits 4]
    ; licenses_accepted : 'a [@bits 16]
    ; hss_busy : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Init_delay
    | Request_nonce
    | Wait_nonce
    | Publish
    | Verify_start
    | Verify_wait
  [@@deriving sexp_of, compare, equal, enumerate]
end

let create scope (i : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in

  (* === TRNG Submodule === *)
  let trng_request_new = Variable.wire ~default:gnd in

  let trng = Trng.create (Scope.sub_scope scope "trng")
    { Trng.I.
      clock = i.clock
    ; clear = i.clear
    ; enable = vdd
    ; request_new = trng_request_new.value
    ; seed = i.trng_seed
    ; load_seed = i.trng_load_seed
    }
  in

  (* === Nonce Register === *)
  let current_nonce = Variable.reg spec ~width:Config.nonce_width in

  (* === HSS/LMS Verification Submodule === *)
  let hss_start = Variable.wire ~default:gnd in
  let license_leaf_index_reg = Variable.reg spec ~width:32 in
  let license_randomizer_reg = Variable.reg spec ~width:256 in

  let hss = Hss_verify.create (Scope.sub_scope scope "hss")
    { Hss_verify.I.
      clock = i.clock
    ; clear = i.clear
    ; start = hss_start.value
    ; identifier = i.identifier
    ; leaf_index = license_leaf_index_reg.value
    ; tree_height = i.tree_height
    ; root_pub_key = i.root_pub_key
    ; message = current_nonce.value
    ; randomizer = license_randomizer_reg.value
    ; sig_element = i.sig_element
    ; sig_element_valid = i.sig_element_valid
    ; auth_node = i.auth_node
    ; auth_node_valid = i.auth_node_valid
    }
  in

  (* === Usage Allowance (inline) === *)
  let allowance = Variable.reg spec ~width:Config.allowance_width in
  let increment_allowance = Variable.wire ~default:gnd in

  let enabled = allowance.value >:. 0 in

  (* Allowance update: increment takes priority, otherwise decrement each cycle *)
  let increment_amount = of_int ~width:Config.allowance_width Config.allowance_increment in
  let incremented_allowance =
    let sum = allowance.value +: increment_amount in
    (* Saturate on overflow *)
    mux2 (sum <: allowance.value) (ones Config.allowance_width) sum
  in
  let decremented_allowance = allowance.value -:. 1 in

  (* === Statistics === *)
  let licenses_accepted = Variable.reg spec ~width:16 in

  (* === Init Delay Counter === *)
  let delay_counter = Variable.reg spec ~width:16 in

  (* === State Machine === *)
  compile [
    (* Allowance update logic - runs every cycle independent of state machine *)
    if_ increment_allowance.value [
      allowance <-- incremented_allowance;
    ] @@ elif (allowance.value >:. 0) [
      allowance <-- decremented_allowance;
    ] [];

    (* State machine *)
    sm.switch [
      State.Init_delay, [
        delay_counter <-- delay_counter.value +:. 1;
        when_ (delay_counter.value >=:. Config.init_delay_cycles) [
          sm.set_next Request_nonce;
        ];
      ];

      State.Request_nonce, [
        trng_request_new <-- vdd;
        sm.set_next Wait_nonce;
      ];

      State.Wait_nonce, [
        when_ trng.nonce_valid [
          current_nonce <-- trng.nonce;
          sm.set_next Publish;
        ];
      ];

      State.Publish, [
        (* Nonce is stable, wait for license submission *)
        when_ i.license_submit [
          license_leaf_index_reg <-- i.license_leaf_index;
          license_randomizer_reg <-- i.license_randomizer;
          sm.set_next Verify_start;
        ];
      ];

      State.Verify_start, [
        when_ (~:(hss.busy)) [
          hss_start <-- vdd;
          sm.set_next Verify_wait;
        ];
      ];

      State.Verify_wait, [
        when_ hss.done_ [
          if_ hss.valid [
            (* Valid license: increment allowance, get new nonce *)
            increment_allowance <-- vdd;
            licenses_accepted <-- licenses_accepted.value +:. 1;
            sm.set_next Request_nonce;
          ] [
            (* Invalid license: return to Publish with same nonce *)
            sm.set_next Publish;
          ];
        ];
      ];
    ];
  ];

  (* === Workload: Signed Int8 Addition with AND-based Output Gating === *)
  let int8_sum =
    let a_signed = i.int8_a in
    let b_signed = i.int8_b in
    (a_signed +: b_signed).:[(7, 0)]  (* Wrapping addition *)
  in

  (* SECURITY GATE: AND each output bit with enabled signal *)
  let enabled_mask = repeat enabled 8 in
  let gated_result = int8_sum &: enabled_mask in

  (* Pipeline register for workload output *)
  let result_reg = Variable.reg spec ~width:8 in
  let result_valid_reg = Variable.reg spec ~width:1 in

  compile [
    result_valid_reg <-- i.workload_valid;
    result_reg <-- gated_result;
  ];

  (* === State Encoding for Debug === *)
  let state_encoding =
    uresize (sm.current) 4
  in

  { O.
    nonce = current_nonce.value
  ; nonce_ready = sm.is Publish
  ; int8_result = result_reg.value
  ; result_valid = result_valid_reg.value
  ; allowance = allowance.value
  ; enabled = enabled
  ; request_sig_element = hss.request_sig_element
  ; sig_element_index = hss.sig_element_index
  ; request_auth_node = hss.request_auth_node
  ; auth_level = hss.auth_level
  ; state_debug = state_encoding
  ; licenses_accepted = licenses_accepted.value
  ; hss_busy = hss.busy
  }
