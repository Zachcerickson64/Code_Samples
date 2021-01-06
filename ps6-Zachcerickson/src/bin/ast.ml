(* file: ast.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains the abstract syntax for a PL for
  principles of programming languages.
*)
type binding = { id : Symbol.t
               ; typ : Typ.t
               }
let b2s {id; typ} =
  let id = Symbol.format id in
  let typ = Typ.format typ
  in
  Lib.fmt "%s:%s" id typ

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

and caseclause = {bv : binding; body : t}

and field = {label : Label.t; expr : t}

and
  decl =
  | VarDef of { bv : binding
              ; defn : t
              }
  | FunDef of { id : Symbol.t
              ; formals : binding list
              ; typ : Typ.t
              ; body : t
              }

let unit   = Literal { typ = Typ.Unit
                     ; bits = 0
                     }
let true_  = InL { expr = unit
                 ; typ = Typ.Unit
                 }
let false_ = InR { expr = unit
                 ; typ = Typ.Unit
                 }

(* The following two functions are used in the parser to convert OCaml
   literals to miniPL literals.

  f2r : float -> t *)
let f2r (f:float) : t =
  let bits = int_of_float (f *. 10.0 ** Util.decimalPlaces)
  in
  Literal {typ = Typ.Real; bits}

let i2i (i:int)   : t =
  Literal {typ = Typ.Int; bits = i}

(* r2f : t -> float --- The r2f function is used in l2s below. *)
let r2f literal =
  match literal with
  | (Literal {typ = Typ.Real; bits}) ->
       (float bits) *. 10.0 ** (-. Util.decimalPlaces)
  | _ -> failwith "Value.r2f: called with the wrong kind of literal."

(* val l2s : t -> string   just for literals, tracks Value. *)
let l2s literal =
  match literal with
  | Literal {typ = Typ.Int;  bits} -> string_of_int bits
  | Literal {typ = Typ.Real; bits = _} -> string_of_float (r2f literal)
  | Literal {typ = Typ.Unit; bits = _} -> "()"
  | Literal _ -> failwith "l2s: cannot happen"
  | _ -> failwith "l2s: cannot happen"

(* format : t -> string *)
let rec format ast =
  match ast with
  | Literal _ -> l2s ast

  | Var id -> Symbol.format id

  | App {rator; rands = []} -> Lib.fmt "%s()" (Symbol.format rator)

  | App {rator; rands} ->
    let ras = Symbol.format rator in
    let randss = List.map format rands in
    let folder s t = Lib.fmt "%s, %s" s t in
    let randStrings = List.fold_left folder (List.hd randss) (List.tl randss)
    in
    Lib.fmt "%s(%s)" ras randStrings

  | Pair {fst; snd} -> Lib.fmt "(%s, %s)" (format fst) (format snd)
  | First  ast -> Lib.fmt "first(%s)"  (format ast)
  | Second ast -> Lib.fmt "second(%s)" (format ast)

(* NB: following two special cases for booleans true and false.
*)
  | InL {expr = Literal {typ = Typ.Unit; bits = 0};
         typ = Typ.Unit} -> "true"

  | InR {expr = Literal {typ = Typ.Unit; bits = 0};
         typ = Typ.Unit} -> "false"

  | InL {expr; typ} ->
    Lib.fmt "inl(%s, %s)" (format expr) (Typ.format typ)

  | InR {expr; typ} ->
    Lib.fmt "inlr%s, %s)" (format expr) (Typ.format typ)

  | Case {expr; left; right} ->
    let expr = format expr in
    let left  = cc2s left  "l" in
    let right = cc2s right "r"
    in
    Lib.fmt "case %s of %s | %s" expr left right

  | Record fields -> Lib.fmt "{%s}" (fields2s fields)

  | Proj {expr; label} ->
    Lib.fmt "%s.%s" (format expr) (Label.format label)

  | Let {decl; body} ->
    let decl = formatDecl decl in
    let body = format body
    in
    Lib.fmt "let %s in %s" decl body

  | Top decl -> formatDecl decl

and
  (* formatDecl : decl -> string *)
  formatDecl decl =
  match decl with
  | VarDef {bv; defn} -> Lib.fmt "%s = %s" (b2s bv) (format defn)

  | FunDef {id; formals; typ; body} ->
    let ts = Typ.format typ
    in
    (match List.map b2s formals with
     | [] -> failwith "toString: cannot happen"
     | firstFormal :: restFormals ->
       let ids = Symbol.format id in
       let folder bv1 bv2 = Lib.fmt "%s, %s" bv1 bv2 in
       let formals = List.fold_left folder firstFormal restFormals in
       let bodys = format body
       in
       Lib.fmt "%s(%s) : %s = %s" ids formals ts bodys)
and
  fields2s fields =
  match fields with
  | [] -> ""
  | {label; expr} :: [] ->
    let ls = Label.format label in
    let es = format expr
    in
    Lib.fmt "%s = %s" ls es

  | {label; expr} :: fields ->
      let ls = Label.format label in
      let es = format expr
      in
      Lib.fmt "%s = %s; %s" ls es (fields2s fields)
and
  cc2s {bv; body} side =
  let bvs = b2s bv in
  let bodys = format body
  in
  Lib.fmt "in%s(%s) => %s" side bvs bodys
