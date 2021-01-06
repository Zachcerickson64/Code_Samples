(* file: label.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an environment.
*)
type t

val fromString : string -> t
val format : t -> string
val compare : t -> t -> int
val fresh : unit -> t
