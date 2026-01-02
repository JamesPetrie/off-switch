open Base

let prime_p = Ecdsa.Config.prime_p_z
let order_n = Ecdsa.Config.order_n_z
let g_x = Ecdsa.Config.g_x_z
let g_y = Ecdsa.Config.g_y_z
let width = Ecdsa.Config.width

(* Modular arithmetic *)
let mod_add a b p = Z.((a + b) mod p)
let mod_sub a b p = Z.(erem (a - b) p)
let mod_mul a b p = Z.((a * b) mod p)
let mod_inv a p = Z.invert a p

(* Point doubling: 2P *)
let point_double (px, py) =
  if Z.equal py Z.zero then None
  else
    let lambda = mod_mul 
      (mod_mul (Z.of_int 3) (mod_mul px px prime_p) prime_p)
      (mod_inv (mod_mul (Z.of_int 2) py prime_p) prime_p)
      prime_p
    in
    let xr = mod_sub (mod_mul lambda lambda prime_p) (mod_mul (Z.of_int 2) px prime_p) prime_p in
    let yr = mod_sub (mod_mul lambda (mod_sub px xr prime_p) prime_p) py prime_p in
    Some (xr, yr)

(* Point addition: P + Q *)
let point_add (px, py) (qx, qy) =
  if Z.equal px qx then
    if Z.equal py qy then point_double (px, py)
    else None  (* P = -Q, result is point at infinity *)
  else
    let lambda = mod_mul (mod_sub qy py prime_p) (mod_inv (mod_sub qx px prime_p) prime_p) prime_p in
    let xr = mod_sub (mod_sub (mod_mul lambda lambda prime_p) px prime_p) qx prime_p in
    let yr = mod_sub (mod_mul lambda (mod_sub px xr prime_p) prime_p) py prime_p in
    Some (xr, yr)

(* Scalar multiplication: k*P *)
let scalar_mult k (px, py) =
  let rec loop acc bit_idx =
    if bit_idx < 0 then acc
    else
      let acc' = match acc with
        | None -> None
        | Some p -> point_double p
      in
      let acc'' = 
        if Z.testbit k bit_idx then
          match acc' with
          | None -> Some (px, py)
          | Some a -> point_add a (px, py)
        else acc'
      in
      loop acc'' (bit_idx - 1)
  in
  loop None 255

(* Conversion utilities *)
let z_to_bits z =
  let hex_str = Z.format "%x" z in
  let padded = String.pad_left hex_str ~len:(width / 4) ~char:'0' in
  Hardcaml.Bits.of_hex ~width padded

let bits_to_z bits =
  Z.of_string_base 2 (Hardcaml.Bits.to_bstr bits)

(* Compute multiples of G for testing *)
let compute_g_multiples n =
  Array.init n ~f:(fun i ->
    if i = 0 then None
    else scalar_mult (Z.of_int i) (g_x, g_y))
