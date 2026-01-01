open Hardcaml
open Signal

module Config = struct
  let width = 256
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; a : 'a [@bits Config.width]
    ; b : 'a [@bits Config.width]
    ; modulus : 'a [@bits Config.width]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { result : 'a [@bits Config.width]
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  
  let mod_add_out = Mod_add.ModAdd.create (Scope.sub_scope scope "mod_add")
    { Mod_add.ModAdd.I.
      clock = i.clock
    ; clear = i.clear
    ; start = i.start
    ; a = i.a
    ; b = i.b
    ; modulus = i.modulus
    ; subtract = gnd
    }
  in
  
  { O.
    result = mod_add_out.result -- "result"
  ; valid = mod_add_out.valid -- "valid"
  }
