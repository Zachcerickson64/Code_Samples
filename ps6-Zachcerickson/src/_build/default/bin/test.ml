(* file: test.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This is a test harness for testing the evaluator for the
   mini-language Earth.

 ******************************************************
 *)
let makeTest input expected =
  (fun () ->
     let tokens = Tokenizer.tokenizer input in
     let ast = Parser.parser tokens
     in
     match Typechecker.typeOf Static.env ast with
     | Some typ -> Typ.equal typ expected
     | None -> false)

   let makeFailTest input =
     (fun () ->
        let tokens = Tokenizer.tokenizer input in
        let ast = Parser.parser tokens
        in
        match Typechecker.typeOf Static.env ast with
        | Some _ -> false
        | None   -> true)

(* Tests that should succeed.

   Test 1
*)
let input1 =
  Tokenizer.Test "343"
let expected1 = Typ.Int
let test1 = ( "test simple integer"
            , makeTest input1 expected1
            )

let input2 =
  Tokenizer.Test
    "let i : int = 343
     in
     (i * 2, 3.14)"
let expected2 = Typ.Product { left = Typ.Int
                            ; right = Typ.Real
                            }
let test2 = ( "test simple let & product"
            , makeTest input2 expected2
            )

let input3 =
  Tokenizer.Test
    "let f(i : int, x : real) : real = i2r(i) +. x
     in
     f(r2i(3.14), 3.14)"
let expected3 = Typ.Real
let test3 = ( "test function call & type conversions"
            , makeTest input3 expected3
            )

let input4 =
  Tokenizer.Test
    "let fact(n : int) : int =
       if (n = 0) then 1
       else n * fact(n - 1)
     in fact(5)"
let expected4 = Typ.Int
let test4 = ( "test recursion"
            , makeTest input4 expected4
            )

let input5 =
  Tokenizer.Test
    "if (true) then 1 else 3.14"
let test5 = ( "test bad branches of conditional"
            , makeFailTest input5
            )

let input6 =
  Tokenizer.Test
    "{a = 2 + 3; b = false}.a"
let expected6 = Typ.Int
let test6 = ( "test record projection"
            , makeTest input6 expected6
            )

let input7 =
  Tokenizer.Test
    "let f(x : int) : int = x + 2
     in f(3, 4)"
let test7 = ( "test wrong number of arguments"
            , makeFailTest input7
            )

let input8 =
  Tokenizer.Test
    "let f(x : int) : int =
       let g(y : int) : int = x + y
       in g(12)
     in f(4)"
let expected8 = Typ.Int
let test8 = ( "test nested scope"
            , makeTest input8 expected8
            )

let run () =
  let tests = [ test1
              ; test2
              ; test3
              ; test4
              ; test5
              ; test6
              ; test7
              ; test8
              ]
  in
  List.iter (fun (name, test) -> Lib.run_test name test) tests
