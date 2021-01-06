(* file: repl.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains a REPL for the mini-language Earth.

   Debugging
*)
let dbg = Debug.out "eval"
let error = Lib.pfmt
let prompt = Tokenizer.Repl "\nEarth> "

let rec repl tenv env : unit =
  let tokens = Tokenizer.tokenizer prompt
  in
  match tokens with
  | Token.QUIT :: [] -> ()
  | _ ->
    let ast = Parser.parser tokens in
    let astString = (Ast.format ast)
    in
    (match ast with
     | Ast.Top decl ->
       (match Typechecker.typeOfDecl tenv decl with
        | Some tenv' ->
          let env' = Eval.evalDecl env decl
          in
          dbg (Lib.fmt "tenv=%s\n\nenv=%s\n" (Static.format tenv')
                 (Dynamic.formatEnv env'));
          repl tenv' env'
        | None ->
          error "repl: decl %s is ill-typed\n" astString
        ; repl tenv env)

     | _ ->
       (match Typechecker.typeOf tenv ast with
        | None ->
          error "repl: %s is ill-typed\n" astString
          ; repl tenv env
        | Some t ->
          let value = Eval.eval env ast
          in
          let valueString = Dynamic.format value in
          let typeString = Typ.format t
          in
          Printf.printf "ast = %s\n" astString
          ; Printf.printf "value = %s : %s\n" valueString typeString
          ; repl tenv env)
    )

(* switches processes compiler options and returns the position
  in Sys.argv where the source file ought to be. NB: Sys.argv.(0)
  contains the string showing the program invocation.
*)
let switches () =
  match Array.length(Sys.argv) with
  | 1 -> 0                         (* just run the repl *)
  | 2 ->
    (match Sys.argv.(1) with
     | "-d" -> Debug.on := true; 0 (* run repl with debugging *)
     | _ -> 1)                     (* run repl afte reading from file *)
  | 3 ->
    (match Sys.argv.(1) with
     | "-d" -> Debug.on := true; 2 (* run in debug after reading file *)
     | _ -> failwith "the only switch is -d")
  | _ -> failwith "strange input"

let prologue filename =
  let inch = open_in filename in
  let tokens = Tokenizer.tokenizer (Tokenizer.File inch) in
  let decls = (Parser.parseDecls tokens) in (*List.rev ??? *)
  let rec repeat decls tenv env =
    match decls with
    | [] -> (tenv, env)
    | decl :: decls ->
      match Typechecker.typeOfDecl tenv decl with
      | Some tenv' ->
        let env' = Eval.evalDecl env decl
        in
        repeat decls tenv' env'
      | None ->
         dbg (Lib.fmt "\ndecl %s is ill-typed.\n" (Ast.formatDecl decl));
        failwith ("prologue: some decl in " ^ filename ^ " was ill-typed")
  in
  repeat decls Static.env Dynamic.env

let go () =
  let n = switches ()                      (* n has pos of file name *)
  in
  match n = 0 with
  | true  -> repl Static.env Dynamic.env   (* there's no input file  *)
  | false ->                               (* decls in an input file *)
    let filename = Sys.argv.(n) in
    let (tenv, env) = prologue filename
    in
    repl tenv env   (* NB: tenv & env have the decls from input file *)

let _ =
  if (Array.length Sys.argv = 2 && Sys.argv.(1) = "test") then
    Test.run ()
  else
    go ()
