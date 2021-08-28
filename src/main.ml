open Miniml.Core
open Miniml.Repl
open Miniml.Utils

let use_repl = ref true

let compile filename =
  filename |> read_file |> compile |> print_endline

let interpret filename =
  let open Miniml.Value in
  filename |> read_file |> interpret |> fst |> string_of_value |> print_endline

let parse_args () =
  let name = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s [-c | -e]" name in
  let options = [
    "-c", Arg.(Tuple [Clear use_repl; String compile]), "Compile";
    "-e", Arg.(Tuple [Clear use_repl; String interpret]), "Evaluate"]
  in
  (* Ignore anonymous arguments *)
  Arg.parse options ignore usage

let () =
  parse_args ();
  if !use_repl then repl ()
