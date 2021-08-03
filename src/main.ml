open Miniml.Core
open Miniml.Utils

let use_repl = ref true

let eval filename =
  filename |> read_file |> eval |> print_value

let compile filename =
  filename |> read_file |> lua_of_miniml |> print_endline

let parse_args () =
  let name = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s [-c | -e]" name in
  let options = [
    "-c", Arg.(Tuple [Clear use_repl; String compile]), "Compile";
    "-e", Arg.(Tuple [Clear use_repl; String eval]), "Evaluate"]
  in
  (* Ignore anonymous arguments *)
  Arg.parse options ignore usage

let () =
  parse_args ();
  if !use_repl then repl ()
