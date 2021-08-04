open Ast

exception Error of string

let type_error msg = raise (Error msg)

let check ty ~expect =
  if ty <> expect then
    let msg = Printf.sprintf
        "Expected %s, got %s"
        (string_of_type expect)
        (string_of_type ty)
    in
    type_error msg
  else ()

let lookup = List.assoc

let rec typecheck env = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Unit -> TUnit
  | Var x -> lookup x env
  | Binop (op, e1, e2) ->
    let ty_e1 = typecheck env e1 in
    let ty_e2 = typecheck env e2 in
    check ~expect:TInt ty_e1;
    check ~expect:TInt ty_e2;
    begin match op with
      | "+" | "-" | "*" | "/" -> TInt
      | "=" | "<>" | "<" | ">" | "<=" | ">=" -> TBool
      | _ -> assert false
    end
  | Let ((x, Some ty), e1, e2) ->
    let ty_e1 = typecheck env e1 in
    check ~expect:ty ty_e1;
    typecheck ((x, ty) :: env) e2
  | Let ((x, None), e1, e2) ->
    let ty_e1 = typecheck env e1 in
    typecheck ((x, ty_e1) :: env) e2
  | Letrec ((x, ty), e1, e2) ->
    let ty_e1 = typecheck ((x, ty) :: env) e1 in
    check ~expect:ty ty_e1;
    typecheck ((x, ty) :: env) e2
  | If (e1, e2, e3) ->
    let ty_e1 = typecheck env e1 in
    let ty_e2 = typecheck env e2 in
    let ty_e3 = typecheck env e3 in
    check ~expect:TBool ty_e1;
    check ~expect:ty_e2 ty_e3;
    ty_e2
  | Fun ((x, ty), e) ->
    let ty_e = typecheck ((x, ty) :: env) e in
    TFun (ty, ty_e)
  | App (e1, e2) ->
    let ty_e1 = typecheck env e1 in
    let ty_e2 = typecheck env e2 in
    match ty_e1 with
    | TFun (ty_a, ty_b) ->
      check ~expect:ty_a ty_e2;
      ty_b
    | _ ->
      type_error "Expected function"
