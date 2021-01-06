(* file: typ.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an abstract syntax for types for various
  CS3366 PLs.
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

let boolean = Sum {left = Unit; right = Unit}

(* format : t -> string *)
let rec format t =
  match t with
  | Int -> "int"
  | Real -> "real"
  | Unit -> "unit"
  | Sum {left = Unit; right = Unit} -> "bool"
  | Sum {left; right} ->
    let ls = format left in
    let rs = format right
    in
    Lib.fmt "(%s + %s)" ls rs

  | Product {left; right} ->
      let ls = format left in
      let rs = format right
      in
      Lib.fmt "(%s * %s)" ls rs

  | RecType fields -> Lib.fmt "{%s}" (formatFields fields)

  | Arrow {from=[]; too} ->
    Lib.fmt "(unit -> %s)" (format too)

  | Arrow {from; too} ->
    let concater t1s t2s = t1s ^ " * " ^ t2s in
    let froms = List.map format from in
    let fromss = List.fold_left concater (List.hd froms) (List.tl froms)
    in
    Lib.fmt "(%s -> %s)" fromss (format too)

and
  formatField {label; typ} =
  Lib.fmt "%s : %s" (Label.format label) (format typ)

and
  formatFields fields =
  match fields with
  | [] -> ""
  | [field] -> formatField field
  | field :: fields ->
    (formatField field) ^ " : " ^ (formatFields fields)

let rec sameLabels labels1 labels2 =
  match (labels1, labels2) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (label1 :: labels1, label2 :: labels2) ->
    (Label.compare label1 label2 = 0) && (sameLabels labels1 labels2)

(* makeField : Label.t -> t -> field *)
let makeField label t = {label = label; typ = t}

(* equal : t -> t -> bool

   NB: It isn't obvious how to define a compare function for types
   nor is it obvious why we would need one.
*)
let rec equal t1 t2 =
  match (t1, t2) with
  | (Int, Int) | (Real, Real) | (Unit, Unit) -> true

  | (Sum {left = l1; right = r1}, Sum {left = l2; right = r2}) ->
    (equal l1 l2) && (equal r1 r2)

  | (Product {left = l1; right = r1}, Product {left = l2; right = r2}) ->
    (equal l1 l2) && (equal r1 r2)

  | (RecType fields1, RecType fields2) ->
    let labels1 = List.map (fun f -> f.label) fields1 in
    let labels2 = List.map (fun f -> f.label) fields2 in
    let typs1 = List.map (fun f -> f.typ) fields1 in
    let typs2 = List.map (fun f -> f.typ) fields2
    in
    (sameLabels labels1 labels2) && (sameTypes typs1 typs2)

  | (Arrow {from = f1s; too = t1}, Arrow {from = f2s; too = t2}) ->
    (sameTypes f1s f2s) && (equal t1 t2)

  | _ -> false

and sameTypes typs1 typs2 =
  match (typs1, typs2) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (typ1 :: typs1, typ2 :: typs2) ->
    (equal typ1 typ2) && (sameTypes typs1 typs2)
