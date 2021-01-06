(* file: eval.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains an evaluator for the mini-language Earth.
*)
open Ast
open Dynamic

(* Debugging *)
let dbg = Debug.out "eval"

(* fiendField : Symbol.t -> Dynamic.field list -> Dynamic.t *)
let rec findField keyLabel fields =
  match fields with
  | [] -> failwith "eval findField: cannot happen"
  | {label; value} :: fields ->
    (match Label.compare label keyLabel = 0 with
     | true  -> value
     | false -> findField keyLabel fields)

(* eval : Dynamic.t Env.t -> Ast.t -> Dynamic.t *)
let rec eval env ast =
  match ast with
  | Ast.Literal {typ; bits} -> Dynamic.Literal {typ; bits}

  | Ast.Var id ->
    let ids = Symbol.format id in
    let envs = Dynamic.formatEnv env
    in
    dbg (Lib.fmt "In repl: id = %s\nenv = %s\n" ids envs) ;
    Env.find id env

  | Ast.App {rator; rands} ->
    let rands' = List.map (eval env) rands in
    let f = Env.find rator env
    in
    (match (f, rands') with
     | (Dynamic.UnOp  op, [operand]) -> op operand

     | (Dynamic.BinOp op, [operand1; operand2]) -> op operand1 operand2

     | (Dynamic.Closure { code = Ast.FunDef {id; formals; typ = _; body}
                        ; env
                        }, rands') ->
       let ids = List.map (fun binding -> binding.id) formals in
       let idValuePairs = List.combine ids rands' in
       let folder env (id, value) = Env.add id value env in
       let env' = List.fold_left folder env idValuePairs in
       let env'' = Env.add id f env'
       in
       eval env'' body

     | _ -> failwith "eval: bad function or operation application")

  | Ast.Pair {fst; snd} ->
    let fst = eval env fst in
    let snd = eval env snd
    in
    Dynamic.Pair {fst; snd}

  | Ast.First expr ->
    (match eval env expr with
     | Dynamic.Pair {fst; snd = _} -> fst
     | _ -> failwith "eval: cannot happen")

  | Ast.Second expr ->
    (match eval env expr with
     | Dynamic.Pair {fst = _; snd} -> snd
     | _ -> failwith "eval: cannot happen")

  | Ast.InL {expr; typ} ->
    Dynamic.InL {value = eval env expr; typ}

  | Ast.InR {expr; typ} ->
    Dynamic.InR {value = eval env expr; typ}

  | Ast.Case {expr;
              left  = {bv = bvl; body = bodyl};
              right = {bv = bvr; body = bodyr}} ->
    (match eval env expr with
     | Dynamic.InL {value; typ = _} ->
       let env' = Env.add bvl.id value env
       in
       eval env' bodyl
     | Dynamic.InR {value; typ = _} ->
       let env' = Env.add bvr.id value env
       in
       eval env' bodyr

     | _ -> failwith "eval: case clause, this cannot happen")

  | Ast.Record fields ->
    let evalField (f : Ast.field) = {label = f.label;
                                     value = eval env f.expr} in
    let fields = List.map evalField fields
    in
    Dynamic.Record fields

  | Ast.Proj {expr; label} ->
    (match eval env expr with
     | Dynamic.Record fields -> findField label fields
     | _ -> failwith "eval: projection error cannot happen")

  | Ast.Let {decl; body} ->
    let env' = evalDecl env decl
    in
    eval env' body

  | _ -> failwith "eval: didn't match any cases."

and

  (* evalDecl : Dynamic.map -> Ast.decl -> Dynamic.map
  *)
  evalDecl env decl =
  match decl with
  | Ast.VarDef {bv; defn} ->
    let value = eval env defn
    in
    Env.add bv.id value env

  | Ast.FunDef {id; formals=_; typ=_; body=_} ->
    Env.add id (Dynamic.Closure {code = decl; env}) env
