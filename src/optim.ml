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

let rec simplify = function
  (* e + 0 = e | 0 + e = e *)
  | Binop ("+", e, Int 0)
  | Binop ("+", Int 0, e) -> simplify e
  (* e - 0 = e *)
  | Binop ("-", e, Int 0) -> simplify e
  (* x - x = 0 *)
  | Binop ("-", Var x, Var y) when x = y -> Int 0
  (* e * 0 = 0 | 0 * e = 0 *)
  | Binop ("*", _, Int 0)
  | Binop ("*", Int 0, _) -> Int 0
  (* e * 1 = e | 1 * e = e *)
  | Binop ("*", e, Int 1)
  | Binop ("*", Int 1, e) -> simplify e
  (* e / 1 = e *)
  | Binop ("/", e, Int 1) -> simplify e
  (* x / x = 1 *)
  | Binop ("/", Var x, Var y) when x = y -> Int 1
  | e -> e

let constant_fold first_pass next_pass = function
  | Binop ("+",  Int a, Int b) -> first_pass (Int (a + b))
  | Binop ("-",  Int a, Int b) -> first_pass (Int (a - b))
  | Binop ("*",  Int a, Int b) -> first_pass (Int (a * b))
  | Binop ("/",  Int a, Int b) -> first_pass (Int (a / b))
  | Binop ("=",  Int a, Int b) -> first_pass (Bool (a = b))
  | Binop ("<>", Int a, Int b) -> first_pass (Bool (a <> b))
  | Binop ("<",  Int a, Int b) -> first_pass (Bool (a < b))
  | Binop (">",  Int a, Int b) -> first_pass (Bool (a > b))
  | Binop ("<=", Int a, Int b) -> first_pass (Bool (a <= b))
  | Binop (">=", Int a, Int b) -> first_pass (Bool (a >= b))
  | e -> next_pass (simplify e)

let eliminate_redundant_let first_pass next_pass = function
  | Let ((x, _), e, Var y) when x = y -> first_pass e
  | e -> next_pass e

let eliminate_unreachable_code first_pass next_pass = function
  | If (Bool true, e1, _) -> first_pass e1
  | If (Bool false, _, e2) -> first_pass e2
  | e -> next_pass e

let rec pass ast =
  constant_fold pass (
    eliminate_redundant_let pass (
      eliminate_unreachable_code pass (
        (map pass)
      )
    )
  ) ast

let optimize ast =
  let rec traverse ast =
    let ast' = pass ast in
    if ast' <> ast then traverse ast' else ast'
  in
  traverse ast
