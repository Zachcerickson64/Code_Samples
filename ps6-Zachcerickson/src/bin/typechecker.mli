(* file: typechecker.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an API for a type checker for a mini-PL used
  in CSCI 3366.
*)
val typeOf : (Typ.t Env.t) -> Ast.t -> Typ.t option
val typeOfDecl : (Typ.t Env.t) -> Ast.decl -> (Typ.t Env.t) option
