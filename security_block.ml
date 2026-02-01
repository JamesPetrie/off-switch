(* security_block.ml - Top-level security block integration
   
   Implements the embedded off-switch architecture from Petrie's paper.
   
   Components:
   - TRNG submodule for nonce generation
   - ECDSA submodule for license verification
   - Inline usage allowance counter
   - Inline Int8 workload with output gating
   
   State machine:
   - Requests nonce from TRNG
   - Publishes nonce for external license generation
   - Verifies received licenses via ECDSA
   - Increments allowance on valid license
   
   Workload continues during verification as long as allowance > 0.
*)

open Hardcaml
open Signal

module Config = struct
  let nonce_width = 256
  let signature_width = 256
  let allowance_width = 32
  let init_delay_cycles = 100
  let allowance_increment = 1_000_000
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    (* License interface *)
    ; license_valid : 'a
    ; license_r : 'a [@bits Config.signature_width]
    ; license_s : 'a [@bits Config.signature_width]
    (* Workload interface *)
    ; workload_valid : 'a
    ; int8_a : 'a [@bits 8]
    ; int8_b : 'a [@bits 8]
    (* ECDSA curve parameters *)
    ; param_a : 'a [@bits Config.signature_width]
    ; param_b3 : 'a [@bits Config.signature_width]
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
    (* Debug *)
    ; state_debug : 'a [@bits 4]
    ; licenses_accepted : 'a [@bits 16]
    ; licenses_rejected : 'a [@bits 16]
    ; ecdsa_busy : 'a
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
    | Update
  [@@deriving sexp_of, compare, enumerate]
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
  
  (* === ECDSA Submodule === *)
  let ecdsa_start = Variable.wire ~default:gnd in
  let license_r_reg = Variable.reg spec ~width:Config.signature_width in
  let license_s_reg = Variable.reg spec ~width:Config.signature_width in
  
  let ecdsa = Ecdsa.create (Scope.sub_scope scope "ecdsa")
    { Ecdsa.I.
      clock = i.clock
    ; clear = i.clear
    ; start = ecdsa_start.value
    ; z = current_nonce.value
    ; r = license_r_reg.value
    ; s = license_s_reg.value
    ; param_a = i.param_a
    ; param_b3 = i.param_b3
    }
  in
  
  (* === Usage Allowance (inline) === *)
  let allowance = Variable.reg spec ~width:Config.allowance_width in
  let increment_allowance = Variable.wire ~default:gnd in
  
  let enabled = allowance.value >:. 0 in
  
  let workload_active = i.workload_valid &: enabled in
  
  (* Allowance update: increment takes priority, no simultaneous inc/dec *)
  let increment_amount = of_int ~width:Config.allowance_width Config.allowance_increment in
  let incremented_allowance = 
    let sum = allowance.value +: increment_amount in
    (* Saturate on overflow *)
    mux2 (sum <: allowance.value) (ones Config.allowance_width) sum
  in
  let decremented_allowance = allowance.value -:. 1 in
  
  (* === Statistics === *)
  let licenses_accepted = Variable.reg spec ~width:16 in
  let licenses_rejected = Variable.reg spec ~width:16 in
  
  (* === Init Delay Counter === *)
  let delay_counter = Variable.reg spec ~width:16 in
  
  (* === State Machine === *)
  compile [
    (* Allowance update logic - runs every cycle independent of state machine *)
    if_ increment_allowance.value [
      allowance <-- incremented_allowance;
    ] @@ elif (workload_active &: (allowance.value >:. 0)) [
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
        when_ i.license_valid [
          license_r_reg <-- i.license_r;
          license_s_reg <-- i.license_s;
          sm.set_next Verify_start;
        ];
      ];
      
      State.Verify_start, [
        when_ (~:(ecdsa.busy)) [
          ecdsa_start <-- vdd;
          sm.set_next Verify_wait;
        ];
      ];
      
      State.Verify_wait, [
        when_ ecdsa.done_ [
          sm.set_next Update;
        ];
      ];
      
      State.Update, [
        if_ ecdsa.valid [
          increment_allowance <-- vdd;
          licenses_accepted <-- licenses_accepted.value +:. 1;
        ] [
          licenses_rejected <-- licenses_rejected.value +:. 1;
        ];
        sm.set_next Request_nonce;
      ];
    ];
  ];
  
  (* === Workload: Signed Int8 Addition with Output Gating === *)
  let int8_sum = 
    let a_signed = i.int8_a in
    let b_signed = i.int8_b in
    (a_signed +: b_signed).:[(7, 0)]  (* Wrapping addition *)
  in
  
  let gated_result = mux2 enabled int8_sum (zero 8) in
  
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
  ; state_debug = state_encoding
  ; licenses_accepted = licenses_accepted.value
  ; licenses_rejected = licenses_rejected.value
  ; ecdsa_busy = ecdsa.busy
  }