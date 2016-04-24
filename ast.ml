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
