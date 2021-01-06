(* file: env.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This code in this file implements a type environment.
*)

let iXi2i = Typ.Arrow {from = [Typ.Int;  Typ.Int];  too = Typ.Int}
let rXr2r = Typ.Arrow {from = [Typ.Real; Typ.Real]; too = Typ.Real}
let i2r   = Typ.Arrow {from = [Typ.Int];  too = Typ.Real}
let r2i   = Typ.Arrow {from = [Typ.Real]; too = Typ.Int}
let iXi2b = Typ.Arrow {from = [Typ.Int; Typ.Int]; too = Typ.boolean}
let rXr2b = Typ.Arrow {from = [Typ.Real; Typ.Real]; too = Typ.boolean}

(* NB: The order here must match the order of the operator names in
   module Basis!
 *)
let typs = [ iXi2i; rXr2r   (* +, +. *)
           ; iXi2i; rXr2r   (* -, -. *)
           ; iXi2i; rXr2r   (* *, *. *)
           ; iXi2i; rXr2r   (* /, /. *)
           ; rXr2r; iXi2i   (* ^, % *)
           ; iXi2b; iXi2b   (* =, <> *)
           ; iXi2b; iXi2b   (* <, <= *)
           ; iXi2b; iXi2b   (* >, >= *)
           ; rXr2b; rXr2b   (* =., <>. *)
           ; rXr2b; rXr2b   (* <., <=. *)
           ; rXr2b; rXr2b   (* >., >=. *)
           ;   i2r;   r2i
           ]

let env = Env.make typs

let format = Env.format Typ.format
