(* file: env.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains code for environments for principles of
  programming languages.
*)
module M = Map.Make(Symbol)

type key = Symbol.t
type 'a t = 'a M.t

let add   = M.add
let find  = M.find
let empty = M.empty

(* make : 'a list -> 'a map *)
let make values =
  let folder map (key, value) = add key value map in
  let keyValuePairs = List.combine Basis.operatorNames values
  in
  List.fold_left folder empty keyValuePairs

let format valueFormatter map =
  let bindings = M.bindings map in
  let folder s (key, value) =
    Lib.fmt "%s = %s; %s" (Symbol.format key) (valueFormatter value) s
  in
  Lib.fmt "{%s}" (List.fold_left folder "" bindings)
