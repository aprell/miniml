open Utils

let tests = [

test {|
  let x = 1 in
  let y = x + 1 in
  let z = x + y + 1 in
  x + (if z - y <= 3 then y * z else z) + 1 |}
  (Int 10);

(* Dynamic scope => 7
   Lexical scope => 8 *)
test {|
  let x = 5 in
  let f = fun y : int => x + y in
  let x = 4 in
  f 3 |}
  (Int 8);

(* Names of local variables matter:
   Dynamic scope => 6
   Lexical scope => 8 *)
test {|
  let x = 5 in
  let f = fun y : int => x + y in
  let g = fun x : int => f x in
  let x = 4 in
  g 3 |}
  (Int 8);

(* Names of local variables matter:
   Dynamic scope => 7
   Lexical scope => 8 *)
test {|
  let x = 5 in
  let f = fun y : int => x + y in
  let g = fun y : int => f y in
  let x = 4 in
  g 3 |}
  (Int 8);

(* Dynamic scope => 5
   Lexical scope => Error: Value of `x' not found *)
test_exn {|
  let f = fun y : int => x + y in
  let x = 3 in
  let y = 4 in
  f 2 |}
  (Failure "Value of `x' not found");

test {|
  let rec fact : int -> int =
    fun x : int =>
      if x = 0 then 1
      else x * fact (x-1)
  in
  fact 5 |}
  (Int 120);

]

let () =
  let run test = test () in
  List.iter run tests;
  report ()
