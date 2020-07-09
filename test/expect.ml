open Miniml.Ast
open Miniml.Core
open Miniml.Utils

let () =
  read_file (
    match Sys.argv with
    | [| _; filename |] -> filename
    | _ -> failwith "Input file required"
  )
  |> parse
  |> pprint_prog
