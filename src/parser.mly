%{
  open Ast
%}

%token <int> INT
%token <string> VAR
%token PLUS MINUS
%token TIMES DIV
%token LPAREN RPAREN
%token EQ NE LT GT LE GE
%token LET REC IN
%token IF THEN ELSE
%token TRUE FALSE
%token FUN
%token ARROW DARROW
%token COLON COMMA
%token EOF

%nonassoc IN
%nonassoc LET
%nonassoc FUN
%nonassoc IF
%right ARROW
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIV
%nonassoc VAR INT FALSE TRUE LPAREN
%nonassoc APP

%start <Ast.expr> prog

%%

prog:
  | expr EOF { $1 }
  ;

expr:
  | INT                                        { Int $1 }
  | VAR                                        { Var $1 }
  | TRUE                                       { Bool true }
  | FALSE                                      { Bool false }
  | LPAREN RPAREN                              { Unit }
  | expr binop expr                            { Binop ($2, $1, $3) }
  | LET VAR option(type_annot) EQ expr IN expr { Let (($2, $3), $5, $7) }
  | LET REC VAR type_annot EQ expr IN expr     { Letrec (($3, $4), $6, $8) }
  | IF expr THEN expr ELSE expr %prec IF       { If ($2, $4, $6) }
  | FUN param_list DARROW expr %prec FUN       { `Fun ($2, $4) |> desugar }
  | expr expr %prec APP                        { App ($1, $2) }
  | LPAREN expr RPAREN                         { $2 }
  ;

param_list:
  | separated_list(COMMA, param) { $1 }
  ;

param:
  | VAR type_annot { ($1, $2) }
  ;

type_annot:
  | COLON type_name { $2 }
  ;

type_name:
  | VAR
    { match $1 with
      | "int" -> TInt
      | "bool" -> TBool
      | "unit" -> TUnit
      | name -> failwith ("Unknown type name " ^ name)
    }
  | type_name ARROW type_name { TFun ($1, $3) }
  ;

%inline binop:
  | PLUS  { "+" }
  | MINUS { "-" }
  | TIMES { "*" }
  | DIV   { "/" }
  | EQ    { "=" }
  | NE    { "<>" }
  | LT    { "<" }
  | GT    { ">" }
  | LE    { "<=" }
  | GE    { ">=" }
  ;
