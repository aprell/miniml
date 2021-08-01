open Miniml.Ast
open Miniml.Core
open Miniml.Optim
open Miniml.Types
open Miniml.Utils

let print ast =
  let _ = typecheck [] ast in
  pprint_prog (optimize ast)

let () =
  read_file (
    match Sys.argv with
    | [| _; filename |] -> filename
    | _ -> failwith "Input file required"
  )
  |> parse
  |> print
