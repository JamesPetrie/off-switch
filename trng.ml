(* trng.ml - True Random Number Generator
   Prototype: Simple counter (replace with ring oscillator for production)
   
   For production, this would use a ring oscillator design like Vasyltsov et al.
   The counter prototype allows deterministic testing.
*)

open Hardcaml
open Signal

module Config = struct
  let width = 256
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; enable : 'a
    ; request_new : 'a
    ; seed : 'a [@bits Config.width]
    ; load_seed : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { nonce : 'a [@bits Config.width]
    ; nonce_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let create _scope (i : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  
  (* Free-running counter simulating entropy source *)
  let counter = Variable.reg spec ~width:Config.width in
  
  (* Latched nonce value - stable until new request *)
  let nonce_reg = Variable.reg spec ~width:Config.width in
  let nonce_valid = Variable.reg spec ~width:1 in
  
  compile [
    (* Counter increments when enabled *)
    when_ i.enable [
      counter <-- counter.value +:. 1;
    ];
    
    (* Load seed for deterministic testing *)
    when_ i.load_seed [
      counter <-- i.seed;
    ];
    
    (* Latch new nonce on request *)
    when_ i.request_new [
      nonce_reg <-- counter.value;
      nonce_valid <-- vdd;
    ];
  ];
  
  { O.
    nonce = nonce_reg.value
  ; nonce_valid = nonce_valid.value
  }