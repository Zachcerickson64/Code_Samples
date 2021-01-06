(* file: label.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an implementation of labels.
*)
type t = String.t

let fromString str = str
let format sym = sym
let compare = String.compare

(* val fresh : unit -> t

   Successive calls of fresh return "x0", "x1", "x2", ...
*)
let fresh () = Lib.fmt "x%d" (Lib.fresh ())
