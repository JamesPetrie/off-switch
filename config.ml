open Base
open Hardcaml
open Signal

let width = 256
let num_registers = 32
let reg_addr_width = 5

(* secp256k1 curve parameters *)
let prime_p_z = Z.of_string 
  "115792089237316195423570985008687907853269984665640564039457584007908834671663"
let order_n_z = Z.of_string
  "115792089237316195423570985008687907852837564279074904382605163141518161494337"

let g_x_z = Z.of_string
  "55066263022277343669578718895168534326250603453777594175500187360389116729240"
let g_y_z = Z.of_string
  "32670510020758816978083085130507043184471273380659243275938904335757337482424"

(* Convert Z.t to Hardcaml constant *)
let z_to_constant z =
  let hex_str = Z.format "%x" z in
  let padded = String.make (Int.max 0 ((width / 4) - String.length hex_str)) '0' ^ hex_str in
  of_constant (Constant.of_hex_string ~width ~signedness:Unsigned padded)

let prime_p () = z_to_constant prime_p_z
let order_n () = z_to_constant order_n_z
let g_x () = z_to_constant g_x_z
let g_y () = z_to_constant g_y_z

(* Register addresses *)
module Reg = struct
  let e = 0
  let r = 1
  let s = 2
  let q_x = 3
  let q_y = 4
  let qpg_x = 5
  let qpg_y = 6
  let g_x = 7
  let g_y = 8
  let zero = 9
  let one = 10
  let two = 11
  let three = 12
  let w = 13
  let u1 = 14
  let u2 = 15
  let acc_x = 16
  let acc_y = 17
  let tmp1 = 18
  let tmp2 = 19
  let tmp3 = 20
  let tmp4 = 21
  let tmp5 = 22
  let lambda = 23
  let lambda_sq = 24
  let p_x = 25
  let p_y = 26
  let add_x = 27
  let add_y = 28
end

(* Arithmetic operation codes *)
module ArithOp = struct
  let add = 0
  let sub = 1
  let mul = 2
  let inv = 3
end

(* Test modes *)
module TestMode = struct
  let normal = 0
  let point_double = 1
  let point_add = 2
  let scalar_mult = 3
end
