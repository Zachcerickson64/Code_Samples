(* file: dynamic.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

  This file contains code relating to Values.  NB: the purpose of BinOp
  and UnOp is to enable us to carry the implementations of primitive
  operators in the value environment.
*)
type t =
  | Literal of { typ : Typ.t
               ; bits : int
               }
  | BinOp of (t -> (t -> t))
  | UnOp  of (t -> t)
  | Record of field list
  | Pair of { fst : t
            ; snd : t
            }
  | InL of { value : t
           ; typ : Typ.t
           }
  | InR of { value : t
           ; typ : Typ.t
           }
  | Closure of { code : Ast.decl
               ; env : map
               }
and
  field = { label : Label.t
          ; value : t
          }
and
  map = t Env.t

val unit : t
val true_ : t
val false_ : t

val i2r : t -> t
val r2i : t -> t
val r2f : t -> float
val f2r : float -> t

val format : t -> string

val env : t Env.t
val formatEnv : t Env.t -> string
val equal : t -> t -> bool
