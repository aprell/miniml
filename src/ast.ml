type expr =
  | Int of int
  | Bool of bool
  | Var of name
  | Binop of name * expr * expr
  | Let of name * expr * expr
  | Letrec of name * expr * expr
  | If of expr * expr * expr
  | Fun of name * expr
  | App of expr * expr

and name = string

type value =
  | VInt of int
  | VBool of bool
  | VFun of name * expr * env

and env = (name * value ref) list

let string_of_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VFun _ -> "<fun>"

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
  | Let (x, e1, e2) -> (
      print ~indent "Let\n";
      let indent = "    " ^ indent in
      print ~indent "%s\n" x;
      pprint_expr ~indent e1;
      pprint_expr ~indent e2
    )
  | Letrec (x, e1, e2) -> (
      print ~indent "Letrec\n";
      let indent = "    " ^ indent in
      print ~indent "%s\n" x;
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
  | Fun (x, e) -> (
      print ~indent "Fun\n";
      let indent = "    " ^ indent in
      print ~indent "%s\n" x;
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
