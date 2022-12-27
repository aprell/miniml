open Miniml.Core
open Miniml.Value

let pass = ref true

let fail test ~got ~expected =
  Printf.eprintf "%s\n" test;
  Printf.eprintf "\027[1m\027[31m";
  Printf.eprintf "=> %s " got;
  Printf.eprintf "(expected %s)" expected;
  Printf.eprintf "\027[0m\n\n%!";
  pass := false

let test a b () =
  try let c, _ = interpret a in
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
  try let c, _ = interpret a in
    fail a
      ~got:(string_of_value c)
      ~expected:(Printexc.to_string e)
  with x ->
    if x <> e then
      fail a
        ~got:(Printexc.to_string x)
        ~expected:(Printexc.to_string e)

let report () =
  if !pass then (
    Printf.printf "\027[1m\027[32m";
    Printf.printf "All tests passed";
    Printf.printf "\027[0m\n%!"
  ) else (
    Printf.printf "\027[1m\027[31m";
    Printf.printf "Some tests failed";
    Printf.printf "\027[0m\n%!"
  )
