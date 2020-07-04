open Miniml.Core

let use_repl = ref false

let parse_args () =
  let name = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s [-c | -e | -r]" name in
  let options = [
    "-c", Arg.String (fun f -> f |> read_file |> lua_of_miniml |> print_endline), "Compile";
    "-e", Arg.String (fun f -> f |> read_file |> eval |> print_value), "Evaluate";
    "-r", Arg.Set use_repl, "REPL"] in
  (* Ignore anonymous arguments *)
  Arg.parse options ignore usage

let () =
  parse_args ();
  if !use_repl then repl ()
