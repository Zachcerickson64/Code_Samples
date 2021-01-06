(* file: ast.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains the abstract syntax for Earth.
*)
type binding = { id : Symbol.t
               ; typ : Typ.t
               }

type t =
  | Literal of { typ : Typ.t
               ; bits : int
               }
  | Var of Symbol.t
  | App of { rator : Symbol.t
           ; rands : t list
           }
  | Pair of { fst : t
            ; snd : t
            }
  | First  of t
  | Second of t
  | InL  of { expr : t
            ; typ : Typ.t
            }
  | InR  of { expr : t
            ; typ : Typ.t
            }
  | Case of { expr  : t
            ; left  : caseclause
            ; right : caseclause
            }
  | Record of (field list)
  | Proj   of { expr : t
              ; label : Label.t
              }
  | Let of { decl : decl
           ; body : t
           }
  | Top of decl

and caseclause = { bv : binding
                 ; body : t
                 }
and field = { label : Label.t
            ; expr : t
            }
and decl =
  | VarDef of { bv : binding
              ; defn : t
              }
  | FunDef of { id : Symbol.t
              ; formals : binding list
              ; typ : Typ.t
              ; body : t
              }

val unit : t
val true_ : t
val false_ : t

val i2i : int -> t
val f2r : float -> t

val format : t -> string
val formatDecl : decl -> string
