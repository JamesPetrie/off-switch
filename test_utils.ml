open Base

let prime_p = Config.prime_p_z
let order_n = Config.order_n_z
let g_x = Config.g_x_z
let g_y = Config.g_y_z
let width = Config.width

(* Modular arithmetic *)
let mod_add a b p = Z.((a + b) mod p)
let mod_sub a b p = Z.(erem (a - b) p)
let mod_mul a b p = Z.((a * b) mod p)
let mod_inv a p = Z.invert a p

(* Point doubling: 2P *)
let point_double (px, py) =
  if Z.equal py Z.zero then None
  else
    let three = Z.of_int 3 in
    let two = Z.of_int 2 in
    let px_sq = mod_mul px px prime_p in
    let numerator = mod_mul three px_sq prime_p in
    let denominator = mod_mul two py prime_p in
    let lambda = mod_mul numerator (mod_inv denominator prime_p) prime_p in
    let lambda_sq = mod_mul lambda lambda prime_p in
    let two_px = mod_mul two px prime_p in
    let xr = mod_sub lambda_sq two_px prime_p in
    let px_minus_xr = mod_sub px xr prime_p in
    let lambda_diff = mod_mul lambda px_minus_xr prime_p in
    let yr = mod_sub lambda_diff py prime_p in
    Some (xr, yr)

(* Point addition: P + Q *)
let point_add (px, py) (qx, qy) =
  if Z.equal px qx then
    if Z.equal py qy then point_double (px, py)
    else None  (* P = -Q, result is point at infinity *)
  else
    let numerator = mod_sub qy py prime_p in
    let denominator = mod_sub qx px prime_p in
    let lambda = mod_mul numerator (mod_inv denominator prime_p) prime_p in
    let lambda_sq = mod_mul lambda lambda prime_p in
    let xr = mod_sub (mod_sub lambda_sq px prime_p) qx prime_p in
    let px_minus_xr = mod_sub px xr prime_p in
    let lambda_diff = mod_mul lambda px_minus_xr prime_p in
    let yr = mod_sub lambda_diff py prime_p in
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
  let padded = String.make (Int.max 0 ((width / 4) - String.length hex_str)) '0' ^ hex_str in
  Hardcaml.Bits.of_hex ~width padded

let bits_to_z bits =
  Z.of_string_base 2 (Hardcaml.Bits.to_bstr bits)

(* Compute first n multiples of G: [G, 2G, 3G, ..., nG] *)
let compute_g_multiples n =
  Array.init n ~f:(fun i ->
    scalar_mult (Z.of_int (i + 1)) (g_x, g_y))

(* Print a point for debugging *)
let print_point name pt =
  match pt with
  | Some (x, y) ->
    Stdio.printf "%s = (%s..., %s...)\n" name
      (String.prefix (Z.to_string x) 30)
      (String.prefix (Z.to_string y) 30)
  | None ->
    Stdio.printf "%s = infinity\n" name

(* Check if two points are equal *)
let points_equal p1 p2 =
  match p1, p2 with
  | Some (x1, y1), Some (x2, y2) -> Z.equal x1 x2 && Z.equal y1 y2
  | None, None -> true
  | _ -> false
