open Base
open Hardcaml
open Signal

module Config = struct
  let width = 256
end

(* Binary Extended GCD for modular inverse computation 
   
   REQUIREMENT: This implementation assumes the modulus is an odd prime.
   - For odd prime modulus and any x coprime to it, gcd(x, modulus) = 1
   - Since modulus is odd, at most one of {x, modulus} can be even
   - Therefore we never need to handle the "both even" case or factor out common powers of 2
   - This simplifies the algorithm significantly compared to the general case
*)
module ModInv = struct
  module State = struct
    type t =
      | Idle
      | Init
      | Loop
      | Finalize
      | Done
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; x : 'a [@bits Config.width]
      ; modulus : 'a [@bits Config.width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result : 'a [@bits Config.width]
      ; valid : 'a
      ; exists : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Always in
    let ( -- ) = Scope.naming scope in
    let width = Config.width in
    let full_width = width + 4 in
    
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    
    let sm = State_machine.create (module State) spec ~enable:vdd in

    let x = Variable.reg spec ~width:full_width in
    let y = Variable.reg spec ~width:full_width in
    let s = Variable.reg spec ~width:full_width in
    let t = Variable.reg spec ~width:full_width in
    let u = Variable.reg spec ~width:full_width in
    let v = Variable.reg spec ~width:full_width in
    
    let x_orig = Variable.reg spec ~width:full_width in
    let modulus_orig = Variable.reg spec ~width:full_width in
    
    let result = Variable.reg spec ~width in
    let valid = Variable.reg spec ~width:1 in
    let exists = Variable.reg spec ~width:1 in

    compile [
      sm.switch [
        State.Idle, [
          valid <-- gnd;
          when_ i.start [
            if_ (i.x ==:. 0) [
              result <-- zero width;
              exists <-- gnd;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.modulus ==:. 0) [
              result <-- zero width;
              exists <-- gnd;
              valid <-- vdd;
              sm.set_next Done;
            ] @@ elif (i.x ==:. 1) [
              result <-- of_int ~width:width 1;
              exists <-- vdd;
              valid <-- vdd;
              sm.set_next Done;
            ] [
              x_orig <-- uresize i.x full_width;
              modulus_orig <-- uresize i.modulus full_width;
              sm.set_next Init;
            ];
          ];
        ];

        State.Init, [
          valid <-- gnd;
          
          x <-- x_orig.value;
          y <-- modulus_orig.value;
          
          s <-- of_int ~width:full_width 1;
          t <-- zero full_width;
          u <-- zero full_width;
          v <-- of_int ~width:full_width 1;
          
          exists <-- gnd;
          sm.set_next Loop;
        ];

        State.Loop, [
          valid <-- gnd;
          
          if_ (x.value ==:. 0) [
            if_ (y.value ==:. 1) [
              exists <-- vdd;
              sm.set_next Finalize;
            ] [
              exists <-- gnd;
              result <-- zero width;
              valid <-- vdd;
              sm.set_next Done;
            ];
          ] [
            if_ (~:(lsb x.value)) [
              x <-- srl x.value 1;
              
              if_ ((~:(lsb s.value)) &: (~:(lsb t.value))) [
                s <-- sra s.value 1;
                t <-- sra t.value 1;
              ] [
                s <-- sra (s.value +: modulus_orig.value) 1;
                t <-- sra (t.value -: x_orig.value) 1;
              ];
            ] @@ elif (~:(lsb y.value)) [
              y <-- srl y.value 1;
              
              if_ ((~:(lsb u.value)) &: (~:(lsb v.value))) [
                u <-- sra u.value 1;
                v <-- sra v.value 1;
              ] [
                u <-- sra (u.value +: modulus_orig.value) 1;
                v <-- sra (v.value -: x_orig.value) 1;
              ];
            ] [
              if_ (x.value >=: y.value) [
                x <-- x.value -: y.value;
                s <-- s.value -: u.value;
                t <-- t.value -: v.value;
              ] [
                y <-- y.value -: x.value;
                u <-- u.value -: s.value;
                v <-- v.value -: t.value;
              ];
            ];
          ];
        ];

        State.Finalize, [
          let u_low = sel_bottom u.value width in
          let modulus_low = sel_bottom modulus_orig.value width in
          let u_plus_mod_low = sel_bottom (u.value +: modulus_orig.value) width in
          let u_normalized = mux2 (msb u.value) u_plus_mod_low u_low in
          
          proc [
            if_ (u_normalized >=: modulus_low) [
              result <-- (u_normalized -: modulus_low);
            ] [
              result <-- u_normalized;
            ];
            valid <-- vdd;
            sm.set_next Done;
          ];
        ];

        State.Done, [
          valid <-- vdd;
          when_ i.start [
            valid <-- gnd;
            sm.set_next Idle;
          ];
        ];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    ; exists = exists.value -- "exists"
    }
end