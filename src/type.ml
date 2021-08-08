type t =
  | Int
  | Bool
  | Unit
  | Fun of t * t

let rec string_of_type = function
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | Fun (t1, t2) ->
    Printf.sprintf
      "%s -> %s"
      (string_of_type t1)
      (string_of_type t2)
