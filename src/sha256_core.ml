(* sha256_core.ml - SHA-256 hash core

   Iterative SHA-256 implementation: 66 cycles per 512-bit block.
   - Cycle 0: init/next (load state, start W schedule)
   - Cycles 1-64: 64 compression rounds
   - Cycle 65: done (accumulate H values, set digest_valid)

   Interface:
   - init: start new hash (loads SHA-256 IV)
   - next: process next block (continues from current H state)
   - block: 512-bit message block (must be pre-padded by caller)
   - ready: high when idle, can accept init/next
   - digest: 256-bit hash output
   - digest_valid: high when digest is valid

   Ported from secworks/sha256 (Joachim Strombergson, Secworks Sweden AB).
*)

open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; init : 'a
    ; next : 'a
    ; block : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; digest : 'a [@bits 256]
    ; digest_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

(* SHA-256 initial hash values *)
let h0_iv = [|
  0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al;
  0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
|]

(* Right rotate a 32-bit signal by n bits *)
let rotr32 s n =
  let lo = s.:[(n - 1), 0] in
  let hi = s.:[31, n] in
  lo @: hi

(* Sigma1(e) = ROTR6(e) ^ ROTR11(e) ^ ROTR25(e) *)
let big_sigma1 e =
  (rotr32 e 6) ^: (rotr32 e 11) ^: (rotr32 e 25)

(* Sigma0(a) = ROTR2(a) ^ ROTR13(a) ^ ROTR22(a) *)
let big_sigma0 a =
  (rotr32 a 2) ^: (rotr32 a 13) ^: (rotr32 a 22)

(* Ch(e,f,g) = (e & f) ^ (~e & g) *)
let ch e f g =
  (e &: f) ^: ((~:e) &: g)

(* Maj(a,b,c) = (a & b) ^ (a & c) ^ (b & c) *)
let maj a b c =
  (a &: b) ^: (a &: c) ^: (b &: c)

module State = struct
  type t =
    | Idle
    | Rounds
    | Done
  [@@deriving sexp_of, compare, equal, enumerate]
end

let create scope (i : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create (module State) spec ~enable:vdd in

  (* Round counter *)
  let t_ctr = Variable.reg spec ~width:6 in

  (* Working variables a-h *)
  let a = Variable.reg spec ~width:32 in
  let b = Variable.reg spec ~width:32 in
  let c = Variable.reg spec ~width:32 in
  let d = Variable.reg spec ~width:32 in
  let e = Variable.reg spec ~width:32 in
  let f = Variable.reg spec ~width:32 in
  let g = Variable.reg spec ~width:32 in
  let h = Variable.reg spec ~width:32 in

  (* Hash value registers H0-H7 *)
  let h_regs = Array.init 8 ~f:(fun _ ->
    Variable.reg spec ~width:32
  ) in

  let digest_valid_reg = Variable.reg spec ~width:1 in

  (* W memory submodule *)
  let w_init = Variable.wire ~default:gnd in
  let w_next = Variable.wire ~default:gnd in

  let w_mem = Sha256_w_mem.create (Scope.sub_scope scope "w_mem")
    { Sha256_w_mem.I.
      clock = i.clock
    ; clear = i.clear
    ; block = i.block
    ; round = t_ctr.value
    ; init = w_init.value
    ; next = w_next.value
    }
  in

  (* K constants *)
  let k_data = Sha256_k_constants.lookup ~round:t_ctr.value in

  (* Compression function: T1 and T2 *)
  let t1 = h.value +: (big_sigma1 e.value) +: (ch e.value f.value g.value)
            +: w_mem.w +: k_data in
  let t2 = (big_sigma0 a.value) +: (maj a.value b.value c.value) in

  (* IV constants as signals *)
  let iv = Array.map h0_iv ~f:(fun v -> of_int32 ~width:32 v) in

  (* State machine *)
  compile [
    sm.switch [
      State.Idle, [
        when_ i.init [
          (* Initialize with SHA-256 IV *)
          a <-- iv.(0); b <-- iv.(1); c <-- iv.(2); d <-- iv.(3);
          e <-- iv.(4); f <-- iv.(5); g <-- iv.(6); h <-- iv.(7);
          h_regs.(0) <-- iv.(0); h_regs.(1) <-- iv.(1);
          h_regs.(2) <-- iv.(2); h_regs.(3) <-- iv.(3);
          h_regs.(4) <-- iv.(4); h_regs.(5) <-- iv.(5);
          h_regs.(6) <-- iv.(6); h_regs.(7) <-- iv.(7);
          w_init <-- vdd;
          t_ctr <--. 0;
          digest_valid_reg <--. 0;
          sm.set_next Rounds;
        ];
        when_ i.next [
          (* Continue from current H state *)
          a <-- h_regs.(0).value; b <-- h_regs.(1).value;
          c <-- h_regs.(2).value; d <-- h_regs.(3).value;
          e <-- h_regs.(4).value; f <-- h_regs.(5).value;
          g <-- h_regs.(6).value; h <-- h_regs.(7).value;
          w_init <-- vdd;
          t_ctr <--. 0;
          digest_valid_reg <--. 0;
          sm.set_next Rounds;
        ];
      ];

      State.Rounds, [
        (* Execute one round per cycle *)
        w_next <-- vdd;
        a <-- (t1 +: t2);
        b <-- a.value;
        c <-- b.value;
        d <-- c.value;
        e <-- (d.value +: t1);
        f <-- e.value;
        g <-- f.value;
        h <-- g.value;
        t_ctr <-- (t_ctr.value +:. 1);
        when_ (t_ctr.value ==:. 63) [
          sm.set_next Done;
        ];
      ];

      State.Done, [
        (* Accumulate: H[i] += working[i] *)
        h_regs.(0) <-- (h_regs.(0).value +: a.value);
        h_regs.(1) <-- (h_regs.(1).value +: b.value);
        h_regs.(2) <-- (h_regs.(2).value +: c.value);
        h_regs.(3) <-- (h_regs.(3).value +: d.value);
        h_regs.(4) <-- (h_regs.(4).value +: e.value);
        h_regs.(5) <-- (h_regs.(5).value +: f.value);
        h_regs.(6) <-- (h_regs.(6).value +: g.value);
        h_regs.(7) <-- (h_regs.(7).value +: h.value);
        digest_valid_reg <--. 1;
        sm.set_next Idle;
      ];
    ];
  ];

  (* Output *)
  let digest = Array.fold h_regs ~init:(Signal.empty) ~f:(fun acc r ->
    if Signal.is_empty acc then r.value else acc @: r.value
  ) in

  { O.
    ready = sm.is Idle
  ; digest
  ; digest_valid = digest_valid_reg.value
  }
