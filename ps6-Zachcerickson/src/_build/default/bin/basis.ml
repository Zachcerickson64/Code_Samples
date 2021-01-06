(* file: basis.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This file contains support code for building environments
   (static and dyanmic).

   These are the names of the built-ins. The left-to-right
   order definitely matters! When symbols are added or subtracted
   from the following list, corresponding changes are required in
   the Static and Dynamics modules.
*)
let operatorNames =
  List.map Symbol.fromString ["+"; "+.";
                              "-"; "-.";
                              "*"; "*.";
                              "/"; "/.";
                              "^"; "%";
                              "="; "<>";
                              "<"; "<=";
                              ">"; ">=";
                              "=."; "<>.";
                              "<."; "<=.";
                              ">."; ">=.";
                             "i2r"; "r2i";]
