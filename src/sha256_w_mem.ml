(* sha256_w_mem.ml - SHA-256 W message schedule

   16-register sliding window implementation of the W message schedule.
   For rounds 0-15, W[t] comes directly from the input block.
   For rounds 16-63, W[t] = sigma1(W[t-2]) + W[t-7] + sigma0(W[t-15]) + W[t-16].

   Ported from secworks/sha256 (Joachim Strombergson, Secworks Sweden AB).
*)

open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; block : 'a [@bits 512]
    ; round : 'a [@bits 6]
    ; init : 'a
    ; next : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { w : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

(* Right rotate a 32-bit signal by n bits *)
let rotr32 s n =
  let lo = s.:[(n - 1), 0] in
  let hi = s.:[31, n] in
  lo @: hi

(* Right shift a 32-bit signal by n bits *)
let shr32 s n =
  let hi = zero n in
  let lo = s.:[31, n] in
  hi @: lo

(* sigma0: used in W expansion *)
let sigma0 w =
  (rotr32 w 7) ^: (rotr32 w 18) ^: (shr32 w 3)

(* sigma1: used in W expansion *)
let sigma1 w =
  (rotr32 w 17) ^: (rotr32 w 19) ^: (shr32 w 10)

let create _scope (i : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  (* 16 x 32-bit registers for the sliding window *)
  let w_regs = Array.init 16 ~f:(fun _ ->
    Variable.reg spec ~width:32
  ) in

  (* Extract 32-bit words from the 512-bit input block (big-endian) *)
  let block_words = Array.init 16 ~f:(fun idx ->
    i.block.:[(511 - (idx * 32)), (512 - ((idx + 1) * 32))]
  ) in

  (* Compute w_new for rounds >= 16 *)
  let w_0 = w_regs.(0).value in
  let w_1 = w_regs.(1).value in
  let w_9 = w_regs.(9).value in
  let w_14 = w_regs.(14).value in
  let w_new = (sigma1 w_14) +: w_9 +: (sigma0 w_1) +: w_0 in

  (* Select output: direct from register for rounds 0-15, computed for 16+ *)
  let w_from_reg = mux (i.round.:[3, 0])
    (Array.to_list (Array.map w_regs ~f:(fun r -> r.value)))
  in
  let w_out = mux2 (i.round <:. 16) w_from_reg w_new in

  (* Register update logic *)
  compile [
    when_ i.init [
      (* Load block words into registers *)
      proc (Array.to_list (Array.mapi block_words ~f:(fun idx word ->
        w_regs.(idx) <-- word
      )));
    ];
    when_ (i.next &: (i.round >=:. 16)) [
      (* Slide window: shift left by 1, insert w_new at position 15 *)
      proc (List.init 15 ~f:(fun idx ->
        w_regs.(idx) <-- w_regs.(idx + 1).value
      ));
      w_regs.(15) <-- w_new;
    ];
  ];

  { O. w = w_out }
