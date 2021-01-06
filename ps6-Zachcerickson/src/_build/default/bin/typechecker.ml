(* file: typechecker.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains a type checker for the CS3366 PL Earth.
*)

let badRator = "you're attempting to call something that isn't a function"
(* let dbg = Debug.out "typechecker" *)

(*findField : Label.t -> Typ.field list -> Typ.t option *)
let rec findField keyLabel fields =
  match fields with
  | [] -> None
  | Typ.{label; typ} :: fields ->
    (match Label.compare keyLabel label = 0 with
     | true  -> Some typ
     | false -> findField keyLabel fields)


(* filterOptions : (Typ.t option) list -> (Typ.t list) option *)
let filterOptions options =
  let rec loop options answer =
    match options with
    | [] -> Some (List.rev answer)
    | None :: _ -> None
    | (Some t) :: options -> loop options (t :: answer)
  in
  loop options []

(* checkAllEqual : Typ.t list -> Typ.t list -> bool *)
let rec checkAllEqual us vs =
  match (us, vs) with
  | ([], []) -> true
  | ([],  _) -> false
  | (_,  []) -> false
  | (u :: us, v :: vs) -> (Typ.equal u v) && (checkAllEqual us vs)



(* typeOf : Static.env -> Ast.t -> Typ.t option

   let rec typeOf tenv ast = ...
*)
let rec typeOf tenv ast =
  match ast with
  | Ast.Literal {typ; bits = _} -> Some typ
  | Ast.Var id ->
    (try Some (Env.find id tenv) with Not_found -> None)
  | Ast.App {rator; rands} ->
    (match typeOf tenv (Ast.Var rator) with
     | Some (Typ.Arrow {from; too}) ->
       let typs = List.map (typeOf tenv) rands
       in
       (match (filterOptions typs) with
        | None -> None
        | Some typs ->
          (match checkAllEqual from typs with
           | true  -> Some too
           | false -> None))
     | Some _ -> failwith ("typeOf: " ^ badRator)
     | None -> None)

  | Ast.Let {decl; body} ->
    (match typeOfDecl tenv decl with
     | Some tenv' -> typeOf tenv' body
     | None -> None)

  | Ast.Top decl ->
    (match typeOfDecl tenv decl with
    | Some _ -> Some Typ.Int
    | None -> None)
  | Ast.Pair {fst;snd} ->
    (match (typeOf tenv fst, typeOf tenv snd) with
     | (Some tau1, Some tau2) -> Some (Typ.Product{left = tau1;right = tau2})
     | _ -> None
    )
  | Ast.First typ ->
    (match typ with
     | Ast.Pair {fst;snd=_} -> typeOf tenv fst
     | _ -> None
    )
  | Ast.Second typ ->
    (match typ with
     | Ast.Pair {fst = _ ;snd} -> typeOf tenv snd
     | _ -> None
    )
  | Ast.InL{expr; typ} ->
    (match typeOf tenv expr with
     | Some tau1 -> Some (Typ.Sum{left = tau1; right = typ})
     | None -> None
    )
  | Ast.InR{expr; typ} ->
    (match typeOf tenv expr with
     | Some tau1 -> Some (Typ.Sum{left = typ; right = tau1})
     | None -> None
    )
  | Ast.Case{expr;left ={bv={id = id1;typ = typ1}; body =b1};right = {bv = {id = id2;typ = typ2}; body = b2}} ->
    (match typeOf tenv expr = Some (Typ.Sum{left = typ1; right = typ2}) with
     | true ->
       (let tenv1 = Env.add id1 typ1 tenv in
        let tenv2 = Env.add id2 typ2 tenv in
        match typeOf tenv1 b1 = typeOf tenv2 b2 with
        |true -> typeOf tenv1 expr
        |false -> None
       )
      | false -> None
    )
  | Ast.Record flist ->
    (
      let rec typeList xs =
       match xs with
       |[]->[]
       |Ast.{label;expr} :: fields ->
         (match typeOf tenv expr with
          |Some tau1-> Typ.{label = label; typ = tau1} :: typeList fields
          |None -> failwith "bad list"
         )
      in
      Some (Typ.RecType(typeList flist))
    )
  | Ast.Proj{expr;label} ->
    (match typeOf tenv expr with
     |Some Typ.RecType fList ->
       (match findField label fList with
        |Some t -> Some t
        |None -> None
       )
     | _ -> None

    )
and
  typeOfDecl tenv decl =
  match decl with
  |Ast.VarDef {bv = {id;typ}; defn} ->
    (match typeOf tenv defn with
    | Some t ->
     (match Typ.equal t typ with
      | true  ->
       let tenv' = Env.add id t tenv
       in
       Some tenv'
      | false -> None)
    | None -> None)
      (* |Ast.FunDef{id; formals; typ; body} ->
    match typeOf tenv body with
     | Some t ->
       (
         let rec loop bList tenv typ1 =
          match bList with
          |[] -> Some tenv
          |Ast.{id;typ} :: bindings ->
            match Typ.equal t typ with
            |true ->
              loop bindings (Env.add id typ1 tenv) typ1
            |false -> None
         in
        let rec productMake bList =
          match bList with
          |[] -> failwith "Bad types"
          |Typ.{label=_;typ} :: fields ->
            (Typ.Product{left = typ; right = (productMake fields)})
          in

         let tenv' = Env.add id (Typ.Arrow{from = productMake formals; too = typ}) (loop formals tenv t)
         in
         match typeOf tenv' body = Some t with
         |true -> Some (Env.add id (Typ.Arrow{from = productMake formals; too = typ}) tenv)
         |false -> None
         ) *)
    | _ -> None
