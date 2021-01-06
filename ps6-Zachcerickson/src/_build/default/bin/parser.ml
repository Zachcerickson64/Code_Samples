(* file: parser.ml
  author: Bob Muller

  CSCI 3366 Programming Languages

   This code implements a recursive descent parser for
   Earth --- a simple block-structured programming language.

  Terms:

   A ::= A = O | A <> O | O
   O ::= O < E | O <= E | O > E | O >= E | E

  E ::= E + T | E - T | T
  T ::= T * F | T / F | T % F | F
  F ::= G ^ F | G
  G ::= (E, E) | first E | second E
      | inl(E, t) | inr(E, t) | case E of inl(x:t) => E | inr(x:t) => E
      | if E then E else E         // syntactic sugar for case
      | let D                      // top-level let, changes environment
      | let D in E
      | ID ( ) | ID ( Es )
      | { Fields } | G . id
      | () | Integer | Real | true | false | ID | ( E )
  Es ::= E | E , Es
  Fields ::= id = E | id = E ; Fields
  D ::= x : t = E | x ( ) : t = E | x ( Ids ) : t = E
  Ids ::= x : t | x : t , Ids

  Types:

   t ::= t + r | r
   r ::= g -> r | g
   g ::= g * m | m
   m ::= unit | int | real | bool | { ms } | ( t )
   ms ::= id : t | id : t ; ms

Was rewritten to remove left recursion:

  Terms:

  A ::= O A'
  A' ::= = O A' | <> O A' | empty
  O ::= E O'
  O' ::= < E O' | <= E O' | > E O' | >= O' | empty

  E  ::= T E'
  E' ::= + T E' | - T E' | empty
  T  ::= F T'
  T' ::= * F T' | / F T' | % F T' | empty
  F  ::= G ^ F | G
  G  ::= ( E , E ) G' | first E G' | second E G'
       | inl E G' | inr E G' | case E of inl(x:t) => E | inr(x:t) => E G'
       | if E then E else E G'
       | let D G'
       | let D in E G'
       | ID ( ) G' | ID ( Es ) G'
       | { Fields } G'
       | () G' | Integer G' | Real G' | true G' | false G' | ID G' | ( E ) G'
  G' ::= . id G' | empty
  Es ::= E | E , Es

  D   ::= x : t = E | x ( ) : t = E | x ( Ids ) : t = E
  Ids ::= x : t | x : t , Ids

  Types:

  t ::= r t'
  t' ::= + r t' | empty
  r ::= g -> r | g
  g ::= m g'
  g' ::= * m g' | empty
  m ::= unit | int | real | bool | { ms } | ( t )
  ms ::= id : t | id : t ; ms
*)

(* Debugging *)
let dbg = Debug.out "parser"

(* Parsing Expressions

  E  ::= T E'
  E' ::= + T E' | - T E' | empty
  T  ::= F T'
  T' ::= * F T' | / F T' | % F T' | empty
  F  ::= G ^ F | G
  G  ::= ( E , E ) G' | first E G' | second E G'
       | { Fields } G'
       | inl E G' | inr E G' | case E of inl(x:t) => E | inr(x:t) => E G'
       | if E then E else E G'
       | let D G'
       | let D in E G'
       | ID ( ) G' | ID ( Es ) G'
       | () G' | Integer G' | Real G' | Bool G' | ID G' | ( E ) G'
  G' ::= . id G' | empty
  Es ::= E | E , Es
*)
let rec expression tokens =             (* EN  ::= O EN' *)
  let (t1Ast, tokens) = order tokens
  in
  expressionTail tokens t1Ast

and expressionTail tokens t1Ast =   (* EN' ::= = O EN' | <> O EN' | empty *)
  match tokens with
  | Token.EQ :: tokens ->
    let (t2Ast, tokens) = order tokens in
    let eq = Symbol.fromString "=" in
    let ast = Ast.App {rator = eq; rands = [t1Ast; t2Ast]}
    in
    expressionTail tokens ast

  | Token.EQR :: tokens ->
    let (t2Ast, tokens) = order tokens in
    let eq = Symbol.fromString "=." in
    let ast = Ast.App {rator = eq; rands = [t1Ast; t2Ast]}
    in
    expressionTail tokens ast

  | Token.NE :: tokens ->
    let (t2Ast, tokens) = order tokens in
    let ne = Symbol.fromString "<>" in
    let ast = Ast.App {rator = ne; rands = [t1Ast; t2Ast]}
    in
    expressionTail tokens ast

  | Token.NER :: tokens ->
    let (t2Ast, tokens) = order tokens in
    let ne = Symbol.fromString "<>." in
    let ast = Ast.App {rator = ne; rands = [t1Ast; t2Ast]}
    in
    expressionTail tokens ast

  | _ -> (t1Ast, tokens)

and
  order tokens =                       (* O  ::= E O' *)
  let (exprAst, tokens) = expr tokens
  in
  orderTail tokens exprAst

and
  (* O' ::= < E O' | <= E O' | > E O' | >= E O' | empty
  *)
  orderTail tokens expr1 =
  match tokens with
  | Token.LT :: tokens ->
    let (expr2, tokens) = expr tokens in
    let lt = Symbol.fromString "<" in
    let ast = Ast.App {rator = lt; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | Token.LTR :: tokens ->
    let (expr2, tokens) = expr tokens in
    let lt = Symbol.fromString "<." in
    let ast = Ast.App {rator = lt; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | Token.LE :: tokens ->
    let (expr2, tokens) = expr tokens in
    let le = Symbol.fromString "<=" in
    let ast = Ast.App {rator = le; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | Token.LER :: tokens ->
    let (expr2, tokens) = expr tokens in
    let le = Symbol.fromString "<=." in
    let ast = Ast.App {rator = le; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | Token.GT :: tokens ->
    let (expr2, tokens) = expr tokens in
    let gt = Symbol.fromString ">" in
    let ast = Ast.App {rator = gt; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | Token.GTR :: tokens ->
    let (expr2, tokens) = expr tokens in
    let gt = Symbol.fromString ">." in
    let ast = Ast.App {rator = gt; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | Token.GE :: tokens ->
    let (expr2, tokens) = expr tokens in
    let ge = Symbol.fromString ">=" in
    let ast = Ast.App {rator = ge; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | Token.GER :: tokens ->
    let (expr2, tokens) = expr tokens in
    let ge = Symbol.fromString ">=." in
    let ast = Ast.App {rator = ge; rands = [expr1; expr2]}
    in
    orderTail tokens ast

  | _ -> (expr1, tokens)

and
  expr tokens =                       (* E  ::= T E' *)
  let (t1Ast, tokens) = term tokens
  in
  exprTail tokens t1Ast

and exprTail tokens t1Ast =           (* E' ::= + T E' | - T E' | empty *)
  match tokens with
  | Token.PLUS :: tokens ->
    let (t2Ast, tokens) = term tokens in
    let plus = Symbol.fromString "+" in
    let ast = Ast.App {rator = plus; rands = [t1Ast; t2Ast]}
    in
    exprTail tokens ast

  | Token.RPLUS :: tokens ->
    let (t2Ast, tokens) = term tokens in
    let plus = Symbol.fromString "+." in
    let ast = Ast.App {rator = plus; rands = [t1Ast; t2Ast]}
    in
    exprTail tokens ast

  | Token.MINUS :: tokens ->
    let (t2Ast, tokens) = term tokens in
    let minus = Symbol.fromString "-" in
    let ast = Ast.App {rator = minus; rands = [t1Ast; t2Ast]}
    in
    exprTail tokens ast

  | Token.RMINUS :: tokens ->
    let (t2Ast, tokens) = term tokens in
    let minus = Symbol.fromString "-." in
    let ast = Ast.App {rator = minus; rands = [t1Ast; t2Ast]}
    in
    exprTail tokens ast

  | _ -> (t1Ast, tokens)

and
  term tokens =                       (* T  ::= F T' *)
  let (t1Ast, tokens) = factor tokens
  in
  termTail tokens t1Ast

and
  termTail tokens t1Ast = (* T' ::= * F T' | / F T' | % F T' | empty *)
  match tokens with
  | Token.TIMES :: tokens ->
    let (t2Ast, tokens) = factor tokens in
    let times = Symbol.fromString "*" in
    let ast = Ast.App {rator = times; rands = [t1Ast; t2Ast]}
    in
    termTail tokens ast

  | Token.RTIMES :: tokens ->
    let (t2Ast, tokens) = factor tokens in
    let times = Symbol.fromString "*." in
    let ast = Ast.App {rator = times; rands = [t1Ast; t2Ast]}
    in
    termTail tokens ast

  | Token.DIV :: tokens ->
    let (t2Ast, tokens) = factor tokens in
    let div = Symbol.fromString "/" in
    let ast = Ast.App {rator = div; rands = [t1Ast; t2Ast]}
    in
    termTail tokens ast

  | Token.RDIV :: tokens ->
    let (t2Ast, tokens) = factor tokens in
    let div = Symbol.fromString "/." in
    let ast = Ast.App {rator = div; rands = [t1Ast; t2Ast]}
    in
    termTail tokens ast

  | Token.MOD :: tokens ->
    let (t2Ast, tokens) = factor tokens in
    let md = Symbol.fromString "%" in
    let ast = Ast.App {rator = md; rands = [t1Ast; t2Ast]}
    in
    termTail tokens ast

  | _ -> (t1Ast, tokens)

and
  factor tokens =                      (* F  ::= G ^ F | G *)
  let (t1Ast, tokens) = gafter tokens
  in
  match tokens with
  | Token.POW :: tokens ->
    let (t2Ast, tokens) = factor tokens in
    let pow = Symbol.fromString "^"
    in
    (Ast.App {rator = pow; rands = [t1Ast; t2Ast]}, tokens)

  | _ -> (t1Ast, tokens)

(*
  G ::= ( E , E ) G' | first E G' | second E G'
      | { Fields } G'
      | inl E G' | inr E G' | case E of inl(x:t) => E | inr(x:t) => E G'
      | if E then E else E G'
      | let D G'
      | let D in E G'
      | ID ( ) G' | ID ( Es ) G'
      | () G' | Integer G' | Real G' | Bool G' | ID G' | ( E ) G'

  G' ::= . id G' | empty
  *)
and
  gafter tokens =
  let (ast, tokens) =
    match tokens with
    | Token.LPAR :: tokens ->
      let (expr1, tokens) = expression tokens
      in
      (match tokens with
       | Token.RPAR :: tokens -> (expr1, tokens)  (* ( expr ) *)
       | Token.COMMA :: tokens ->
         let (expr2, tokens) = expression tokens
         in
         (match tokens with
          | Token.RPAR :: tokens ->            (* ( expr1 , expr2 ) *)
            (Ast.Pair {fst = expr1; snd = expr2}, tokens)
          | _ -> failwith "bad pair, missing rparen")
       | _ -> failwith "( expr followed by neither , nor )")

    | Token.FIRST :: tokens ->
      let (ast, tokens) = expression tokens
      in
      (Ast.First ast, tokens)

    | Token.SECOND :: tokens ->
      let (ast, tokens) = expression tokens
      in
      (Ast.Second ast, tokens)

    | Token.LBRACE :: tokens ->
      let (fields, tokens) = parseTermFields tokens []
      in
      (match tokens with
       | Token.RBRACE :: tokens -> (Ast.Record fields, tokens)
       | _ -> failwith "bad record expression missing }")

    | Token.INL :: tokens ->
      (match tokens with
       | Token.LPAR :: tokens ->
         let (ast, tokens) = expression tokens
         in
         (match tokens with
          | Token.COMMA :: tokens ->
            let (typ, tokens) = parseTyp tokens
            in
            (match tokens with
             | Token.RPAR :: tokens ->
               (Ast.InL {expr =ast; typ = typ}, tokens)
             | _ -> failwith "missing rparen in inL injection")
          | _ -> failwith "missing , in inL injection")
       | _ -> failwith "missing lparen in INL injection")

    | Token.INR :: tokens ->
      (match tokens with
       | Token.LPAR :: tokens ->
         let (ast, tokens) = expression tokens
         in
         (match tokens with
          | Token.COMMA :: tokens ->
            let (typ, tokens) = parseTyp tokens
            in
            (match tokens with
             | Token.RPAR :: tokens ->
               (Ast.InR {expr = ast; typ = typ}, tokens)
             | _ -> failwith "missing rparen in inR injection")
          | _ -> failwith "missing , in inR injection")
       | _ -> failwith "missing lparen in inR injection")

    | Token.CASE :: tokens -> parseCase (Token.CASE :: tokens)

    | Token.IF :: tokens ->
      let (test, tokens) = expression tokens
      in
      (match tokens with
       | Token.THEN :: tokens ->
         let (thn, tokens) = expression tokens
         in
         (match tokens with
          | Token.ELSE :: tokens ->
            let (els, tokens) = expression tokens
            in
            let id = Symbol.fresh () in
            let left  = {Ast.bv = {id = id; typ = Typ.Unit}; body = thn} in
            let right = {Ast.bv = {id = id; typ = Typ.Unit}; body = els}
            in
            (Ast.Case {expr = test; left = left; right = right}, tokens)
          | _ -> failwith "missing else in if-expression")
       | _ -> failwith "missing then in if-expression")

    | Token.LET :: tokens ->
      let (decl, tokens) = parseDecl tokens
      in
      (match tokens with
       | Token.IN :: tokens ->
         let (body, tokens) = expression tokens
         in
         (Ast.Let {decl = decl; body = body}, tokens)
       | _ -> (Ast.Top decl, tokens))

    | (Token.ID x) :: tokens ->
      (match tokens with
       | Token.LPAR :: tokens ->
         (match tokens with
          | Token.RPAR :: tokens ->                   (* ID() *)
              (Ast.App {rator = x; rands = []}, tokens)
          | _ ->
            let (args, tokens) = exprList tokens (* ID (expr, ..., expr) *)
            in
            (Ast.App {rator = x; rands = args}, tokens))
       | _ -> (Ast.Var x, tokens))                    (* ID *)

    | Token.UNIT :: tokens -> (Ast.unit,  tokens)

    | (Token.INTEGER i) :: tokens  -> (Ast.i2i i, tokens)

    | (Token.FLOATING f) :: tokens -> (Ast.f2r f, tokens)

    | (Token.TRUE) :: tokens -> (Ast.true_, tokens)

    | (Token.FALSE) :: tokens -> (Ast.false_, tokens)

    | _ -> failwith "gafter: bad input"
  in
  gafterTail tokens ast

and
  gafterTail tokens ast =         (* G' ::= . id G' | empty *)
  match tokens with
  | Token.DOT :: (Token.ID label) :: tokens ->
    let label = Label.fromString (Symbol.format label)
    in
    gafterTail tokens (Ast.Proj {expr = ast; label = label})
  | _ -> (ast, tokens)

  (* Parse a list of expressions
  *)
and
  exprList tokens =
  let (ast, tokens) = expression tokens
  in
  match tokens with
  | Token.COMMA :: tokens ->
    let (asts, tokens) = exprList tokens
    in
    (ast :: asts, tokens)

  | Token.RPAR :: tokens -> ([ast], tokens)
  | _ -> failwith "exprList: bad list of expressions."

and
  (* Fields ::= id = E | id = E ; Fields | empty
  *)
  parseTermFields tokens fields =
  match tokens with
  | Token.RBRACE :: _ -> (List.rev fields, tokens)
  | (Token.ID label) :: Token.EQ :: tokens ->
    let (ast, tokens) = expression tokens in
    let field = {label = Label.fromString (Symbol.format label);
                 Ast.expr = ast}
    in
    (match tokens with
     | Token.SEMI :: tokens ->
       parseTermFields tokens (field :: fields)
     | Token.RBRACE :: _ ->
       (List.rev (field :: fields), tokens)
     | _ -> failwith "bad input missing ;")
  | _ -> failwith "bad input missing label and/or :"

(* Parse a declaration

  D   ::= x : t = E | x ( ) : t = E | x ( Ids ) : t = E
  Ids ::= x : t | x : t , Ids
*)
and parseDecl tokens =
  match tokens with
  | (Token.ID x) :: tokens ->
    (match tokens with
     | Token.LPAR :: Token.RPAR :: Token.COLON :: tokens ->
       let (typ, tokens) = parseTyp tokens
       in
       (match tokens with
        | Token.EQ :: tokens ->
          let (body, tokens) = expression tokens
          in
          (Ast.FunDef {id = x;
                       formals = [];
                       typ = typ;
                       body = body}, tokens)
        | _ -> failwith "bad function definition missing =")

     | Token.LPAR :: tokens ->
       let (formals, tokens) = parseBVs tokens
       in
       (match tokens with
        | Token.COLON :: tokens ->
          let (typ, tokens) = parseTyp tokens
          in
          (match tokens with
           | Token.EQ :: tokens ->
             let (body, tokens) = expression tokens
             in
             (Ast.FunDef {id = x;
                          formals = formals;
                          typ = typ;
                          body = body}, tokens)
           | _ -> failwith "bad function definition missing =")
        | _ -> failwith "bad function definition missing :")

     | Token.COLON :: tokens ->
       let (typ, tokens) = parseTyp tokens
       in
       (match tokens with
        | Token.EQ :: tokens ->
          let (defn, tokens) = expression tokens
          in
          (Ast.VarDef {bv = {id = x; Ast.typ = typ}; defn = defn}, tokens)
        | _ -> failwith "bad declaration, missing =")
     | _ -> failwith "bad declaration, nothing reasonable after id")
  | _ -> failwith "bad declaration, all declarations start with an id"

and parseDecls tokens =
  match tokens with
  | [] -> []
  | Token.LET :: tokens ->
    let (decl, tokens) = parseDecl tokens
    in
    decl :: parseDecls tokens
  | _ ->
    dbg (Lib.fmt "parseDecls: tokens=%s\n" (Token.formatTokens tokens));
    failwith "parseDecls: doesn't start with let, this isn't what we want..."

and parseBV tokens =
  match tokens with
  | (Token.ID x) :: Token.COLON :: tokens ->
    let (typ, tokens) = parseTyp tokens
    in
    ({Ast.id = x; typ = typ}, tokens)
  | _ -> failwith "parseBV: bad formal parameter"

and parseBVs tokens =
  let (formal, tokens) = parseBV tokens
  in
  match tokens with
  | Token.COMMA :: tokens ->
    let (formals, tokens) = parseBVs tokens
    in
    (formal :: formals, tokens)
  | Token.RPAR :: tokens  -> ([formal], tokens)
  | _ -> failwith "parseFormals: bad formals!"

and
  parseCase tokens = (* case E of inl(x:t) => E | inr(x:t) => E *)
  match tokens with
  | Token.CASE :: tokens ->
    let (exp, tokens) = expression tokens
    in
    (match tokens with
     | Token.OF :: tokens ->
       let (left, tokens) = parseCaseClause Token.INL tokens
       in
       (match tokens with
        | Token.BAR :: tokens ->
          let (right, tokens) = parseCaseClause Token.INR tokens in
          let ast = Ast.Case{expr = exp; left = left; right = right}
          in
          exprTail tokens ast
        | _ -> failwith "bad case syntax, missing vertical bar")
     | _ -> failwith "bad case syntax, missing of")
  | _ -> failwith "bad case, this cannot happen"

and
  parseCaseClause inj tokens =
    match tokens with
    | lr :: Token.LPAR :: tokens ->
      (match lr = inj with
       | true  ->
         let (bv, tokens) = parseBV tokens
         in
         (match tokens with
          | Token.RPAR :: Token.DARROW :: tokens ->
            let (clause, tokens) = expression tokens
            in
            ({Ast.bv = bv; body = clause}, tokens)
          | _ -> failwith "bad case clause missing ) => ")
       | false -> failwith "bad case clause misuse of inl or inr")
    | _ -> failwith "bad case expression, missing clauses"

and
  (* Parsing Type Expressions
  *)
  parseTyp tokens =           (* t ::= r t'  *)
  let (rho, tokens) = parseRho tokens
    in
    parseTypTail tokens rho

and
  parseTypTail tokens rho1 =      (* t' ::= + r t' | empty *)
  match tokens with
  | Token.PLUS :: tokens ->
    let (rho2, tokens) = parseRho tokens
    in
    parseTypTail tokens (Typ.Sum {left = rho1; right = rho2})
  | _ -> (rho1, tokens)

and
  parseRho tokens =              (* r ::= g -> r | g *)
  let (gamma, tokens) = parseGamma tokens
  in
  match tokens with
  | Token.ARROW :: tokens ->
    let (rho, tokens) = parseRho tokens
    in
    (Typ.Arrow{from = [gamma]; too = rho}, tokens)
  | _ -> (gamma, tokens)

and
  parseGamma tokens =             (* g ::= m g' *)
  let (mu, tokens) = parseMu tokens
  in
  parseGammaTail tokens mu

and
  parseGammaTail tokens mu1 =     (* g' ::= * m g' | empty *)
  match tokens with
  | Token.TIMES::tokens ->
    let (mu2, tokens) = parseMu tokens
    in
    parseGammaTail tokens (Typ.Product {left = mu1; right = mu2})
  | _ -> (mu1, tokens)

and parseMu tokens = (* m ::= unit | int | real | bool | { ms } | ( t ) *)
  match tokens with
  | Token.UNIT_TYPE :: tokens -> (Typ.Unit, tokens)
  | Token.INT_TYPE :: tokens  -> (Typ.Int,  tokens)
  | Token.REAL_TYPE :: tokens -> (Typ.Real, tokens)
  | Token.BOOL_TYPE :: tokens -> (Typ.boolean, tokens)
  | Token.LPAR :: tokens ->
    let (typ, tokens) = parseTyp tokens
    in
    (match tokens with
     | Token.RPAR :: tokens -> (typ, tokens)
     | _ -> failwith "bad type missing )")

  | Token.LBRACE :: tokens ->
    let (fields, tokens) = parseFields tokens []
    in
    (match tokens with
     | Token.RBRACE :: tokens -> (Typ.RecType fields, tokens)
     | _ -> failwith "bad type missing }")

  | _ -> failwith "really bad type"

and
  parseFields tokens fields =        (* ms ::= id : t | id : t ; ms | empty *)
  match tokens with
  | Token.RBRACE :: _ -> (List.rev fields, tokens)
  | (Token.ID label) :: Token.COLON :: tokens ->
    let (typ, tokens) = parseTyp tokens in
    let field = { label = Label.fromString (Symbol.format label)
                ; Typ.typ = typ}
    in
    (match tokens with
     | Token.SEMI :: tokens -> parseFields tokens (field :: fields)
     | Token.RBRACE :: _ -> (List.rev (field :: fields), tokens)
     | _ -> failwith "bad input missing ;")
  | _ -> failwith "bad input missing label and/or :"

let parser tokens =
  dbg (Lib.fmt "tokens = %s" (Token.formatTokens tokens));
  match expression tokens with
  | (ast, []) -> ast
  | _ -> failwith "bad syntax, found a parse but there are leftover tokens."
