(* file: debug.ml
   author: Bob Muller

   CS3366 Programming Languages
*)
let on = ref false

let out id msg =
  match !on with
  | true  -> Lib.pfmt "%s: %s\n" id msg
  | false -> ()
