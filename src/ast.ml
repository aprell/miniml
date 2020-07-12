type expr =
  | Int of int
  | Bool of bool
  | Var of name
  | Binop of name * expr * expr
  | Let of (name * ty option) * expr * expr
  | Letrec of (name * ty) * expr * expr
  | If of expr * expr * expr
  | Fun of (name * ty) * expr
  | App of expr * expr

and ty =
  | TInt
  | TBool
  | TFun of ty * ty

and name = string

type value =
  | VInt of int
  | VBool of bool
  | VFun of name * expr * env

and env = (name * value ref) list

let rec desugar = function
  | `Fun (params, body) ->
    begin match params with
    | (param, ty) :: [] ->
      Fun ((param, ty), body)
    | (param, ty) :: params ->
      Fun ((param, ty), desugar (`Fun (params, body)))
    | [] -> failwith "Empty parameter list"
    end
  | _ -> assert false

let string_of_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VFun _ -> "<fun>"

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (t1, t2) ->
    Printf.sprintf
      "%s -> %s"
      (string_of_type t1)
      (string_of_type t2)

let print ~indent =
  print_string indent;
  fun args -> Printf.printf args

let rec pprint_expr ~indent = function
  | Int i ->
    print ~indent "Int: %d\n" i
  | Bool b ->
    print ~indent "Bool: %s\n" (if b then "true" else "false")
  | Var x ->
    print ~indent "Var: %s\n" x
  | Binop (op, e1, e2) -> (
      print ~indent "Binop\n";
      let indent = "    " ^ indent in
      print ~indent "(%s)\n" op;
      pprint_expr ~indent e1;
      pprint_expr ~indent e2
    )
  | Let ((x, Some ty), e1, e2) -> (
      print ~indent "Let\n";
      let indent = "    " ^ indent in
      print ~indent "%s: %s\n" x (string_of_type ty);
      pprint_expr ~indent e1;
      pprint_expr ~indent e2
    )
  | Let ((x, None), e1, e2) -> (
      print ~indent "Let\n";
      let indent = "    " ^ indent in
      print ~indent "%s\n" x;
      pprint_expr ~indent e1;
      pprint_expr ~indent e2
    )
  | Letrec ((x, ty), e1, e2) -> (
      print ~indent "Letrec\n";
      let indent = "    " ^ indent in
      print ~indent "%s: %s\n" x (string_of_type ty);
      pprint_expr ~indent e1;
      pprint_expr ~indent e2
    )
  | If (e1, e2, e3) -> (
      print ~indent "If\n";
      let indent = "    " ^ indent in
      pprint_expr ~indent e1;
      pprint_expr ~indent e2;
      pprint_expr ~indent e3
    )
  | Fun ((x, ty), e) -> (
      print ~indent "Fun\n";
      let indent = "    " ^ indent in
      print ~indent "%s: %s\n" x (string_of_type ty);
      pprint_expr ~indent e
    )
  | App (e1, e2) -> (
      print ~indent "App\n";
      let indent = "    " ^ indent in
      pprint_expr ~indent e1;
      pprint_expr ~indent e2
    )

let pprint_prog expr =
  print_endline "Program";
  pprint_expr ~indent:"└── " expr
