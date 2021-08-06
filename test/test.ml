open Miniml.Ast
open Miniml.Core

let pass = ref true

let fail test ~got ~expected =
  Printf.eprintf "%s\n" test;
  Printf.eprintf "\027[1m\027[31m";
  Printf.eprintf "=> %s " got;
  Printf.eprintf "(expected %s)" expected;
  Printf.eprintf "\027[0m\n\n%!";
  pass := false

let test a b () =
  try let c = eval a in
    try assert (c = b)
    with Assert_failure _ ->
      fail a
        ~got:(string_of_value c)
        ~expected:(string_of_value b)
  with e ->
    fail a
      ~got:(Printexc.to_string e)
      ~expected:(string_of_value b)

let test_exn a (e : exn) () =
  try let c = eval a in
    fail a
      ~got:(string_of_value c)
      ~expected:(Printexc.to_string e)
  with x ->
    if x <> e then
      fail a
        ~got:(Printexc.to_string x)
        ~expected:(Printexc.to_string e)

let tests = [

test
"let x = 1 in
 let y = x + 1 in
 let z = x + y + 1 in
 x + (if z - y <= 3 then y * z else z) + 1"
 (VInt 10);

test
(* Dynamic scope => 7 *)
(* Lexical scope => 8 *)
"let x = 5 in
 let f = fun y : int => x + y in
 let x = 4 in
 f 3"
 (VInt 8);

test
(* Names of local variables matter! *)
(* Dynamic scope => 6 *)
(* Lexical scope => 8 *)
"let x = 5 in
 let f = fun y : int => x + y in
 let g = fun x : int => f x in
 let x = 4 in
 g 3"
 (VInt 8);

test
(* Names of local variables matter! *)
(* Dynamic scope => 7 *)
(* Lexical scope => 8 *)
"let x = 5 in
 let f = fun y : int => x + y in
 let g = fun y : int => f y in
 let x = 4 in
 g 3"
 (VInt 8);

test_exn
(* Dynamic scope => 5 *)
(* Lexical scope => Error: Unbound value x *)
"let f = fun y : int => x + y in
 let x = 3 in
 let y = 4 in
 f 2"
 (Failure "Value of `x' not found");

test
"let rec fact : int -> int =
   fun x : int =>
     if x = 0 then 1
     else x * fact (x-1)
 in
 fact 5"
 (VInt 120);

]

let () =
  let run test = test () in
  List.iter run tests;
  if !pass = true then begin
    Printf.printf "\027[1m\027[32m";
    Printf.printf "All tests passed";
    Printf.printf "\027[0m\n%!"
  end
