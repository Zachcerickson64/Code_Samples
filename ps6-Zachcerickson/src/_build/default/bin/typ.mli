(* file: typ.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an API for an ADT for types for CS3366 PLs.
*)
type t = Int
       | Real
       | Unit
       | Sum of {left : t; right : t}
       | Product of {left : t; right : t}
       | RecType of field list
       | Arrow of {from : t list; too : t}
and
  field = {label : Label.t; typ : t}

val boolean : t
val makeField : Label.t -> t -> field
val format : t -> string
val equal : t -> t -> bool
