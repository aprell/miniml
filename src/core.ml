open Ast
open Types
open Optim

let apply = function
  | "+",  VInt a, VInt b -> VInt (a + b)
  | "-",  VInt a, VInt b -> VInt (a - b)
  | "*",  VInt a, VInt b -> VInt (a * b)
  | "/",  VInt a, VInt b -> VInt (a / b)
  | "=",  VInt a, VInt b -> VBool (a = b)
  | "<>", VInt a, VInt b -> VBool (a <> b)
  | "<",  VInt a, VInt b -> VBool (a < b)
  | ">",  VInt a, VInt b -> VBool (a > b)
  | "<=", VInt a, VInt b -> VBool (a <= b)
  | ">=", VInt a, VInt b -> VBool (a >= b)
  | _ -> failwith "Invalid expression"

let lookup = List.assoc

let rec interpret env = function
  | Int n -> VInt n
  | Bool n -> VBool n
  | Unit -> VUnit
  | Var x -> !(lookup x env)
  | Binop (op, e1, e2) ->
    let e1' = interpret env e1 in
    let e2' = interpret env e2 in
    apply (op, e1', e2')
  | Let ((x, _), e1, e2) ->
    let e1' = interpret env e1 in
    interpret ((x, ref e1') :: env) e2
  | Letrec ((x, _), e1, e2) ->
    (* Bind x to dummy value *)
    let env' = (x, ref (VBool false)) :: env in
    let e1' = interpret env' e1 in
    (* Backpatch x with function closure *)
    lookup x env' := e1';
    interpret env' e2
  | If (e1, e2, e3) ->
    let e1' = interpret env e1 in
    begin match e1' with
      | VBool true -> interpret env e2
      | _ -> interpret env e3
    end
  | Fun ((x, _), e) -> VFun (x, e, env)
  | App (e1, e2) ->
    let e1' = interpret env e1 in
    let e2' = interpret env e2 in
    begin match e1' with
      | VFun (x, e, env) -> interpret ((x, ref e2') :: env) e
      | _ -> failwith "Not a function"
    end

let rec compile = function
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Unit -> "nil"
  | Var x -> x
  | Binop (op, e1, e2) ->
    let e1' = compile e1 in
    let e2' = compile e2 in
    Printf.sprintf "(%s %s %s)"
      e1' (match op with | "=" -> "==" | "<>" -> "~=" | _ -> op) e2'
  | Let ((x, _), e1, e2) ->
    let e1' = compile e1 in
    let e2' = compile e2 in
    Printf.sprintf "(function (%s) return %s end)(%s)" x e2' e1'
  | Letrec ((x, _), e1, e2) ->
    let e1' = compile e1 in
    let e2' = compile e2 in
    Printf.sprintf "(function (%s_) %s = %s_ return %s end)(%s)" x x x e2' e1'
  | If (e1, e2, e3) ->
    let e1' = compile e1 in
    let e2' = compile e2 in
    let e3' = compile e3 in
    Printf.sprintf "(%s and %s or %s)" e1' e2' e3'
  | Fun ((x, _), e) ->
    let e' = compile e in
    Printf.sprintf "function (%s) return %s end" x e'
  | App (e1, e2) ->
    let e1' = compile e1 in
    let e2' = compile e2 in
    Printf.sprintf "(%s)(%s)" e1' e2'

let parse input =
  input |> Lexing.from_string |> Parser.prog Lexer.read

let eval input =
  let ast = parse input in
  let _ = typecheck [] ast in
  interpret [] (optimize ast)

let lua_of_miniml input =
  let ast = parse input in
  let _ = typecheck [] ast in
  "print(" ^ compile (optimize ast) ^ ")"

let print_value value =
  value |> string_of_value |> print_endline

let repl ?(prompt = "miniml> ") () =
  let rec loop () =
    print_string prompt;
    try
      read_line () |> eval |> print_value; loop ()
    with
    | Lexer.Error e -> Printf.eprintf "Syntax error: %s\n%!" e; loop ()
    | Parser.Error -> Printf.eprintf "Parser error\n%!"; loop ()
    | Failure e -> Printf.eprintf "%s\n%!" e; loop ()
    | End_of_file -> print_endline "Bye"
  in
  loop ()
