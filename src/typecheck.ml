open Ast

exception Error of string

let type_error msg = raise (Error msg)

let check ty ~expect =
  if ty <> expect then
    let msg = Printf.sprintf
        "Expected %s, got %s"
        (Type.string_of_type expect)
        (Type.string_of_type ty)
    in
    type_error msg
  else ()

let binops =
  [ ("+",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
    ("-",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
    ("*",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
    ("/",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
    ("=",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
    ("<>", Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
    ("<",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
    (">",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
    ("<=", Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
    (">=", Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool))); ]

let lookup = List.assoc

let rec typecheck' env = function
  | Int _ -> Type.Int
  | Bool _ -> Type.Bool
  | Unit -> Type.Unit
  | Var x -> lookup x env
  | Binop (op, e1, e2) -> (
      match lookup op binops with
      | Type.Fun (ty1, Type.Fun (ty2, ty3)) ->
        let ty_e1 = try typecheck' env e1 with Not_found -> ty1 in
        let ty_e2 = try typecheck' env e2 with Not_found -> ty2 in
        check ~expect:ty1 ty_e1;
        check ~expect:ty2 ty_e2;
        ty3
      | exception Not_found ->
        type_error (Printf.sprintf "Type of `%s' not found" op)
      | _ -> assert false
    )
  | Let ((x, Some ty), e1, e2) ->
    let ty_e1 = typecheck' env e1 in
    check ~expect:ty ty_e1;
    typecheck' ((x, ty) :: env) e2
  | Let ((x, None), e1, e2) ->
    let ty_e1 = typecheck' env e1 in
    typecheck' ((x, ty_e1) :: env) e2
  | Letrec ((x, ty), e1, e2) ->
    let ty_e1 = typecheck' ((x, ty) :: env) e1 in
    check ~expect:ty ty_e1;
    typecheck' ((x, ty) :: env) e2
  | If (e1, e2, e3) ->
    let ty_e1 = typecheck' env e1 in
    let ty_e2 = typecheck' env e2 in
    let ty_e3 = typecheck' env e3 in
    check ~expect:Type.Bool ty_e1;
    check ~expect:ty_e2 ty_e3;
    ty_e2
  | Fun ((x, ty), e) ->
    let ty_e = typecheck' ((x, ty) :: env) e in
    Type.Fun (ty, ty_e)
  | App (e1, e2) ->
    let ty_e1 = typecheck' env e1 in
    let ty_e2 = typecheck' env e2 in
    match ty_e1 with
    | Type.Fun (ty1, ty2) ->
      check ~expect:ty1 ty_e2;
      ty2
    | _ ->
      type_error "Expected function"

let typecheck = typecheck' []
