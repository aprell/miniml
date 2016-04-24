open Ast
open Miniml

let pass = ref true

let check (a, b) =
  let c = eval a in
  try assert (c = b)
  with Assert_failure _ ->
    pass := false;
    Printf.eprintf "%s\n" a;
    Printf.eprintf "\027[1m\027[31m";
    Printf.eprintf "=> %s " (string_of_value c);
    Printf.eprintf "(expected %s)" (string_of_value b);
    Printf.eprintf "\027[0m\n\n%!"

let try_check (a, b) =
  try check (a, b)
  with Not_found -> ()

let tests = [|
"let x = 1 in
 let y = x + 1 in
 let z = x + y + 1 in
 x + (if z - y <= 3 then y * z else z) + 1"
, VInt 10;

(* Dynamic scope => 7 *)
(* Lexical scope => 8 *)
"let x = 5 in
 let f = fun y -> x + y in
 let x = 4 in
 f 3"
, VInt 8;

(* Names of local variables matter! *)
(* Dynamic scope => 6 *)
(* Lexical scope => 8 *)
"let x = 5 in
 let f = fun y -> x + y in
 let g = fun x -> f x in
 let x = 4 in
 g 3"
, VInt 8;

(* Names of local variables matter! *)
(* Dynamic scope => 7 *)
(* Lexical scope => 8 *)
"let x = 5 in
 let f = fun y -> x + y in
 let g = fun y -> f y in
 let x = 4 in
 g 3"
, VInt 8;

(* Dynamic scope => 5 *)
(* Lexical scope => Error: Unbound value x *)
"let f = fun y -> x + y in
 let x = 3 in
 let y = 4 in
 f 2"
, VInt 5;

"let rec fact =
   fun x ->
     if x = 0 then 1
     else x * fact (x-1)
 in
 fact 5"
, VInt 120;
|]

let test () =
  Array.iter try_check tests;
  if !pass = true then begin
    Printf.printf "\027[1m\027[32m";
    Printf.printf "All tests passed";
    Printf.printf "\027[0m\n%!"
  end
