open Base
open Hardcaml
open Signal

module ModInv = struct
  module State = struct
    type t =
      | Idle
      | Running
    [@@deriving sexp_of, enumerate]
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
    let extended_width = width + 1 in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module State) spec ~enable:vdd in
    
    let u = Variable.reg spec ~width:extended_width in
    let v = Variable.reg spec ~width:extended_width in
    let x1 = Variable.reg spec ~width:extended_width in
    let x2 = Variable.reg spec ~width:extended_width in
    let modulus_reg = Variable.reg spec ~width:extended_width in
    
    let result = Variable.reg spec ~width in
    let valid = Variable.wire ~default:gnd in
    let exists = Variable.reg spec ~width:1 in
    
    compile [
      sm.switch [
        State.Idle, [
          when_ i.start [
            u <-- uresize i.x extended_width;
            v <-- uresize i.modulus extended_width;
            x1 <-- uresize (one width) extended_width;
            x2 <-- zero extended_width;
            modulus_reg <-- uresize i.modulus extended_width;
            exists <-- vdd;
            sm.set_next Running;
          ];
        ];
        
        State.Running, [
          let u_is_one = u.value ==: uresize (one width) extended_width in
          let v_is_one = v.value ==: uresize (one width) extended_width in
          let u_is_zero = u.value ==:. 0 in
          let v_is_zero = v.value ==:. 0 in
          
          if_ (u_is_one |: v_is_one |: u_is_zero |: v_is_zero) [
            if_ u_is_one [
              result <-- sel_bottom x1.value width;
              exists <-- vdd;
            ] @@ elif v_is_one [
              result <-- sel_bottom x2.value width;
              exists <-- vdd;
            ] [
              result <-- zero width;
              exists <-- gnd;
            ];
            valid <-- vdd;
            sm.set_next Idle;
          ] @@ elif (lsb u.value ==:. 0) [
            u <-- srl u.value 1;
            let x1_odd = lsb x1.value in
            let x1_plus_mod = x1.value +: modulus_reg.value in
            let new_x1 = mux2 x1_odd (srl x1_plus_mod 1) (srl x1.value 1) in
            x1 <-- new_x1;
          ] @@ elif (lsb v.value ==:. 0) [
            v <-- srl v.value 1;
            let x2_odd = lsb x2.value in
            let x2_plus_mod = x2.value +: modulus_reg.value in
            let new_x2 = mux2 x2_odd (srl x2_plus_mod 1) (srl x2.value 1) in
            x2 <-- new_x2;
          ] @@ elif (u.value >=: v.value) [
            u <-- u.value -: v.value;
            let x1_minus_x2 = x1.value -: x2.value in
            let x1_corrected = x1_minus_x2 +: modulus_reg.value in
            let needs_correction = msb x1_minus_x2 in
            x1 <-- mux2 needs_correction x1_corrected x1_minus_x2;
          ] [
            v <-- v.value -: u.value;
            let x2_minus_x1 = x2.value -: x1.value in
            let x2_corrected = x2_minus_x1 +: modulus_reg.value in
            let needs_correction = msb x2_minus_x1 in
            x2 <-- mux2 needs_correction x2_corrected x2_minus_x1;
          ];
        ];
      ];
    ];

    { O.result = result.value -- "result"
    ; valid = valid.value -- "valid"
    ; exists = exists.value -- "exists"
    }
end
