(* file: dynamic.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains code relating to run-time values.
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

(* NB: we're representing booleans as sums in this implementation *)
let unit   = Literal {typ = Typ.Unit; bits = 0}
let true_  = InL {value = unit; typ = Typ.Unit}
let false_ = InR {value = unit; typ = Typ.Unit}

(* val i2r : Value -> Value

 The following i2r function is the implementation of the i2r function.
*)
let i2r literal =
  match literal with
  | Literal {typ = Typ.Int; bits} ->
    let bits = bits * (int_of_float (10.0 ** Util.decimalPlaces))
    in
    Literal {typ = Typ.Real; bits}
  | _ -> failwith "i2r: cannot happen."

(* val r2i : Value -> Value

 The following r2i function is the implementation of the r2i function.
*)
let r2i literal =
  match literal with
  | Literal {typ = Typ.Real; bits} ->
    let bits = int_of_float ((float bits) *. 10.0 ** -. Util.decimalPlaces)
    in
    Literal {typ = Typ.Int; bits}
  | _ -> failwith "r2i: cannot happen."

let f2r (f : float) =
  let bits = int_of_float (f *. 10.0 ** Util.decimalPlaces)
  in
  Literal {typ = Typ.Real; bits}

let r2f literal =
  match literal with
  | Literal {typ = Typ.Real; bits} ->
    (float bits) *. 10.0 ** (-. Util.decimalPlaces)
  | _ -> failwith "Value.r2f: called with the wrong kind of literal."

let l2s literal =
  match literal with
  | Literal {typ = Typ.Int; bits} -> string_of_int bits
  | Literal {typ = Typ.Real; bits=_} -> string_of_float (r2f literal)
  | Literal {typ = Typ.Unit; bits=_} -> "()"
  | Literal _ -> failwith "l2s: cannot happen"
  | _ -> failwith "l2s: cannot happen"

let rec format value =
  match value with
  | Literal _ -> l2s value

  | Record fields -> Lib.fmt "{%s}" (fields2s fields)

  (* NB: following two special cases for booleans true and false.
  *)
  | InL {value = Literal {typ = Typ.Unit; bits = 0};
         typ = Typ.Unit} -> "true"

  | InR {value = Literal {typ = Typ.Unit; bits = 0};
         typ = Typ.Unit} -> "false"

  | InL {value; typ} ->
    let values = format value in
    let typs  = Typ.format typ
    in
    Lib.fmt "inl(%s, %s)" values typs

  | InR {value; typ} ->
    let values = format value in
    let typs  = Typ.format typ
    in
    Lib.fmt "inr(%s, %s)" values typs

  | Pair {fst; snd} ->
    Lib.fmt "(%s, %s)" (format fst) (format snd)

  | Closure _ -> "format of closures isn't implemented"

  | UnOp _ -> "UnOp"
  | BinOp _ -> "BinOp"

and
  fields2s fields =
  match fields with
  | [] -> ""
  | {label; value} :: [] ->
    let ls = Label.format label in
    let es = format value
    in
    ls ^ " = " ^ es
  | {label; value} :: fields ->
    let ls = Label.format label in
    let es = format value
    in
    (ls ^ " = " ^ es ^ "; ") ^ (fields2s fields)

(**********************************************************************)

let iXi2i op =
  let theOp opnd1 opnd2 =
    match (opnd1, opnd2) with
    | (Literal {typ = Typ.Int; bits = bits1},
       Literal {typ = Typ.Int; bits = bits2}) ->
      Literal{typ = Typ.Int; bits = op bits1 bits2}
    | _ -> failwith "binaryPrimOp: cannot happen."
  in
  BinOp theOp

let rXr2r op =
  let theOp opnd1 opnd2 =
    match (opnd1, opnd2) with
    | (Literal {typ = Typ.Real; bits=_},
       Literal {typ = Typ.Real; bits=_}) ->
      let (rv1 : float) = r2f opnd1 in
      let (rv2 : float) = r2f opnd2
      in
      f2r (op rv1 rv2)
    | _ -> failwith "binaryPrimOp: cannot happen."
  in
  BinOp theOp

let iXi2b op =
  let theOp opnd1 opnd2 =
    match (opnd1, opnd2) with
    | (Literal {typ = Typ.Int; bits = bits1},
       Literal {typ = Typ.Int; bits = bits2}) ->
      (match op bits1 bits2 with
       | true  -> true_
       | false -> false_)
    | _ -> failwith "binaryPrimOp: bad types to relational operator"
  in
  BinOp theOp

let rXr2b op =
  let theOp opnd1 opnd2 =
    match (opnd1, opnd2) with
    | (Literal {typ = Typ.Real; bits = bits1},
       Literal {typ = Typ.Real; bits = bits2}) ->
      (match op bits1 bits2 with
       | true  -> true_
       | false -> false_)
    | _ -> failwith "binaryPrimOp: bad types to relational operator"
  in
  BinOp theOp

(* The implementations of division must dynamically check for a zero divisor.
*)
let divI (m : int) (n : int) : int =
  match n = 0 with
  | true  -> failwith "divide: attempting to divide by zero."
  | false -> m / n

let divR (m : float) (n : float) : float =
  match n = 0.0 with
  | true  -> failwith "divide: attempting to divide by zero."
  | false -> m /. n

(* The order here must match the order of the operator names above!
*)
let binopImplementations =
  [ iXi2i (+); rXr2r (+.)
  ; iXi2i (-); rXr2r (-.)
  ; iXi2i ( * ); rXr2r ( *. )
  ; iXi2i divI; rXr2r divR
  ; rXr2r( ** ); iXi2i (mod)
  ; iXi2b (=); iXi2b (<>); iXi2b (<)
  ; iXi2b (<=); iXi2b (>); iXi2b (>=)
  ; rXr2b (=); rXr2b (<>); rXr2b (<)
  ; rXr2b (<=); rXr2b (>); rXr2b (>=)
  ]

let unopImplementations = [UnOp i2r; UnOp r2i]

let operators = binopImplementations @ unopImplementations

let env = Env.make operators

let formatEnv = Env.format format

let equal v1 v2 =
  match (v1, v2) with
  | (Literal one, Literal two) -> one.bits = two.bits
  | _ -> failwith "Dynamic.equal: incomprable arguments"
