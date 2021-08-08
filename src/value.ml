type t =
  | Int of int
  | Bool of bool
  | Unit
  | Fun of name * Ast.expr * env

and env = (name * t ref) list

and name = string

let string_of_value = function
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Unit -> "()"
  | Fun _ -> "<fun>"

let print value =
  value |> string_of_value |> print_endline
