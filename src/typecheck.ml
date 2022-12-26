open Ast

exception Error of string

let type_error msg = raise (Error msg)

let check ty ~expect =
  if ty <> expect then
    let msg = Printf.sprintf
        "Expected `%s', but computed `%s'"
        (Type.string_of_type expect)
        (Type.string_of_type ty)
    in
    type_error msg
  else ()

let lookup x env =
  try List.assoc x env with
    Not_found -> type_error (Printf.sprintf "Type of `%s' not found" x)

let rec typecheck' env = function
  | Int _ -> Type.Int
  | Bool _ -> Type.Bool
  | Unit -> Type.Unit
  | Var x -> lookup x env
  | Let ((x, ty), e1, e2) ->
    let ty_e1 = typecheck' env e1 in
    Option.iter (fun ty -> check ~expect:ty ty_e1) ty;
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
    match ty_e1 with
    | Type.Fun (ty1, ty2) ->
      let ty_e2 = try typecheck' env e2 with Error _ -> ty1 in
      check ~expect:ty1 ty_e2;
      ty2
    | _ ->
      type_error "Expected function"

let typecheck = typecheck' [
  ("+",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
  ("-",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
  ("*",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
  ("/",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Int)));
  ("=",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
  ("<>", Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
  ("<",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
  (">",  Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
  ("<=", Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
  (">=", Type.Fun (Type.Int, Type.Fun (Type.Int, Type.Bool)));
]
