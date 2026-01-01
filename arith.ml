open Hardcaml
open Signal

module Config = struct
  let width = 256
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start_add : 'a
    ; start_sub : 'a
    ; start_mul : 'a
    ; start_inv : 'a
    ; a : 'a [@bits Config.width]
    ; b : 'a [@bits Config.width]
    ; modulus : 'a [@bits Config.width]
    ; num_bits : 'a [@bits 9]  (* For mul: number of significant bits in b *)
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { add_result : 'a [@bits Config.width]
    ; add_valid : 'a
    ; sub_result : 'a [@bits Config.width]
    ; sub_valid : 'a
    ; mul_result : 'a [@bits Config.width]
    ; mul_valid : 'a
    ; inv_result : 'a [@bits Config.width]
    ; inv_valid : 'a
    ; inv_exists : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  
  (* Modular addition: (a + b) mod modulus *)
  let mod_add_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_add")
    { Mod_add.ModAdd.I.
      clock = i.clock
    ; clear = i.clear
    ; start = i.start_add
    ; a = i.a
    ; b = i.b
    ; modulus = i.modulus
    ; subtract = gnd
    }
  in
  
  (* Modular subtraction: (a - b) mod modulus *)
  let mod_sub_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_sub")
    { Mod_add.ModAdd.I.
      clock = i.clock
    ; clear = i.clear
    ; start = i.start_sub
    ; a = i.a
    ; b = i.b
    ; modulus = i.modulus
    ; subtract = vdd
    }
  in
  
  (* Modular multiplication: (a * b) mod modulus *)
  let mod_mul_out = Mod_mul.ModMul.create (Scope.sub_scope scope "mod_mul")
    { Mod_mul.ModMul.I.
      clock = i.clock
    ; clear = i.clear
    ; start = i.start_mul
    ; x = i.a
    ; y = i.b
    ; modulus = i.modulus
    ; num_bits = i.num_bits
    }
  in
  
  (* Modular inverse: a^(-1) mod modulus *)
  let mod_inv_out = Mod_inv.ModInv.create (Scope.sub_scope scope "mod_inv")
    { Mod_inv.ModInv.I.
      clock = i.clock
    ; clear = i.clear
    ; start = i.start_inv
    ; x = i.a
    ; modulus = i.modulus
    }
  in
  
  { O.
    add_result = mod_add_out.result -- "add_result"
  ; add_valid = mod_add_out.valid -- "add_valid"
  ; sub_result = mod_sub_out.result -- "sub_result"
  ; sub_valid = mod_sub_out.valid -- "sub_valid"
  ; mul_result = mod_mul_out.result -- "mul_result"
  ; mul_valid = mod_mul_out.valid -- "mul_valid"
  ; inv_result = mod_inv_out.result -- "inv_result"
  ; inv_valid = mod_inv_out.valid -- "inv_valid"
  ; inv_exists = mod_inv_out.exists -- "inv_exists"
  }