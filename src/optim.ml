open Ast

let map f = function
  | Int _ | Bool _ | Unit | Var _ as e -> e
  | Binop (op, e1, e2) ->
    Binop (op, f e1, f e2)
  | Let (name_and_ty, e1, e2)  ->
    Let (name_and_ty, f e1, f e2)
  | Letrec (name_and_ty, e1, e2)  ->
    Letrec (name_and_ty, f e1, f e2)
  | If (cond, then_, else_) ->
    If (f cond, f then_, f else_)
  | Fun (name_and_ty, e) ->
    Fun (name_and_ty, f e)
  | App (e1, e2) ->
    App (f e1, f e2)

let rec constant_fold = function
  | Binop ("+",  Int a, Int b) -> Int (a + b)
  | Binop ("-",  Int a, Int b) -> Int (a - b)
  | Binop ("*",  Int a, Int b) -> Int (a * b)
  | Binop ("/",  Int a, Int b) -> Int (a / b)
  | Binop ("=",  Int a, Int b) -> Bool (a = b)
  | Binop ("<>", Int a, Int b) -> Bool (a <> b)
  | Binop ("<",  Int a, Int b) -> Bool (a < b)
  | Binop (">",  Int a, Int b) -> Bool (a > b)
  | Binop ("<=", Int a, Int b) -> Bool (a <= b)
  | Binop (">=", Int a, Int b) -> Bool (a >= b)
  | e -> map constant_fold e

let rec eliminate_redundant_let = function
  | Let ((x, _), e, Var y) when x = y ->
    eliminate_redundant_let e
  | e -> map eliminate_redundant_let e

let rec eliminate_unreachable_code = function
  | If (Bool true, e1, _) -> eliminate_unreachable_code e1
  | If (Bool false, _, e2) -> eliminate_unreachable_code e2
  | e -> map eliminate_unreachable_code e

let optimize ast =
  let rec loop ast =
    let ast' =
      ast
      |> constant_fold
      |> eliminate_redundant_let
      |> eliminate_unreachable_code
    in
    if ast' <> ast then loop ast' else ast'
  in
  loop ast
