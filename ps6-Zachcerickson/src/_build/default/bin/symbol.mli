(* file: symbol.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an API for the environment module.
*)
type t

val fromString : string -> t
val format : t -> string
val compare : t -> t -> int
val fresh : unit -> t
