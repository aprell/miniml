open Ast
open Typecheck
open Optim

let apply = function
  | "+",  Value.Int a, Value.Int b -> Value.Int (a + b)
  | "-",  Value.Int a, Value.Int b -> Value.Int (a - b)
  | "*",  Value.Int a, Value.Int b -> Value.Int (a * b)
  | "/",  Value.Int a, Value.Int b -> Value.Int (a / b)
  | "=",  Value.Int a, Value.Int b -> Value.Bool (a = b)
  | "<>", Value.Int a, Value.Int b -> Value.Bool (a <> b)
  | "<",  Value.Int a, Value.Int b -> Value.Bool (a < b)
  | ">",  Value.Int a, Value.Int b -> Value.Bool (a > b)
  | "<=", Value.Int a, Value.Int b -> Value.Bool (a <= b)
  | ">=", Value.Int a, Value.Int b -> Value.Bool (a >= b)
  | _ -> failwith "Invalid expression"

let lookup x env =
  try List.assoc x env with
    Not_found -> failwith (Printf.sprintf "Value of `%s' not found" x)

let rec eval' env = function
  | Int n -> Value.Int n
  | Bool n -> Value.Bool n
  | Unit -> Value.Unit
  | Var x -> !(lookup x env)
  | Binop (op, e1, e2) ->
    let e1' = eval' env e1 in
    let e2' = eval' env e2 in
    apply (op, e1', e2')
  | Let ((x, _), e1, e2) ->
    let e1' = eval' env e1 in
    eval' ((x, ref e1') :: env) e2
  | Letrec ((x, _), e1, e2) ->
    (* Bind x to dummy value *)
    let env' = (x, ref (Value.Bool false)) :: env in
    let e1' = eval' env' e1 in
    (* Backpatch x with function closure *)
    lookup x env' := e1';
    eval' env' e2
  | If (e1, e2, e3) ->
    let e1' = eval' env e1 in
    begin match e1' with
      | Value.Bool true -> eval' env e2
      | _ -> eval' env e3
    end
  | Fun ((x, _), e) -> Value.Fun (x, e, env)
  | App (e1, e2) ->
    let e1' = eval' env e1 in
    let e2' = eval' env e2 in
    begin match e1' with
      | Value.Fun (x, e, env) -> eval' ((x, ref e2') :: env) e
      | _ -> failwith "Not a function"
    end

let rec emit = function
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Unit -> "nil"
  | Var x -> x
  | Binop (op, e1, e2) ->
    let e1' = emit e1 in
    let e2' = emit e2 in
    Printf.sprintf "(%s %s %s)"
      e1' (match op with | "=" -> "==" | "<>" -> "~=" | _ -> op) e2'
  | Let ((x, _), e1, e2) ->
    let e1' = emit e1 in
    let e2' = emit e2 in
    Printf.sprintf "(function (%s) return %s end)(%s)" x e2' e1'
  | Letrec ((x, _), e1, e2) ->
    let e1' = emit e1 in
    let e2' = emit e2 in
    Printf.sprintf "(function (%s_) %s = %s_ return %s end)(%s)" x x x e2' e1'
  | If (e1, e2, e3) ->
    let e1' = emit e1 in
    let e2' = emit e2 in
    let e3' = emit e3 in
    Printf.sprintf "(%s and %s or %s)" e1' e2' e3'
  | Fun ((x, _), e) ->
    let e' = emit e in
    Printf.sprintf "function (%s) return %s end" x e'
  | App (e1, e2) ->
    let e1' = emit e1 in
    let e2' = emit e2 in
    Printf.sprintf "(%s)(%s)" e1' e2'

let print_error input Lexing.{ lex_curr_pos = pos; _ } =
  let sol = (* start of line *)
    try String.rindex_from input (pos - 1) '\n' + 1 with
      Not_found -> 0
  in
  let eol = (* end of line *)
    try String.index_from input pos '\n' with
      Not_found -> String.length input
  in
  Printf.eprintf "%s\n%s\n%!"
    (String.sub input sol (eol - sol))
    (String.make (pos - sol - 1) ' ' ^ "^")

let parse input =
  let lexbuf = Lexing.from_string input in
  try Parser.prog Lexer.read lexbuf with
  | Lexer.Error msg as e ->
    let line, col = Lexer.position lexbuf in
    Printf.eprintf "Syntax error in line %d, column %d: %s:\n%!" line col msg;
    print_error input lexbuf;
    raise e
  | Parser.Error as e ->
    let line, col = Lexer.position lexbuf in
    Printf.eprintf "Parse error in line %d, column %d:\n%!" line col;
    print_error input lexbuf;
    raise e

let typecheck ast =
  try typecheck ast with
    Typecheck.Error msg as e ->
    Printf.eprintf "Type error: %s\n%!" msg;
    raise e

let eval ast =
  try eval' [] ast with
    Failure msg as e ->
    Printf.eprintf "Runtime error: %s\n%!" msg;
    raise e

let interpret input =
  let ast = parse input in
  let ty = typecheck ast in
  eval (optimize ast), ty

let compile input =
  let ast = parse input in
  let _ = typecheck ast in
  "print(" ^ emit (optimize ast) ^ ")"
