(* file: static.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an API for the type environment.
*)
val env : Typ.t Env.t
val format : Typ.t Env.t -> string
