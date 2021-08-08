open Core

let repl ?(prompt = "miniml> ") () =
  let rec loop () =
    print_string prompt;
    try
      read_line () |> interpret |> print_value; loop ()
    with
    | Lexer.Error e -> Printf.eprintf "Syntax error: %s\n%!" e; loop ()
    | Parser.Error -> Printf.eprintf "Parser error\n%!"; loop ()
    | Types.Error e -> Printf.eprintf "Type error: %s\n%!" e; loop ()
    | Failure e -> Printf.eprintf "%s\n%!" e; loop ()
    | End_of_file -> print_endline "Bye"
  in
  loop ()
