(* file: eval.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains the API for an evaluator for the mini-language
  Earth.
*)
val eval : Dynamic.map -> Ast.t -> Dynamic.t
val evalDecl : Dynamic.map -> Ast.decl -> Dynamic.map
