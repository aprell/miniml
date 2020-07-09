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
%token FUN DARROW
%token COLON ARROW
%token TINT
%token TBOOL
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
%left APP
%nonassoc VAR INT FALSE TRUE LPAREN

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
  | expr binop expr                            { Binop ($2, $1, $3) }
  | LET VAR option(type_annot) EQ expr IN expr { Let (($2, $3), $5, $7) }
  | LET REC VAR type_annot EQ expr IN expr     { Letrec (($3, $4), $6, $8) }
  | IF expr THEN expr ELSE expr %prec IF       { If ($2, $4, $6) }
  | FUN VAR type_annot DARROW expr %prec FUN   { Fun (($2, $3), $5) }
  | expr expr %prec APP                        { App ($1, $2) }
  | LPAREN expr RPAREN                         { $2 }
  ;

type_annot:
  | COLON type_name { $2 }
  ;

type_name:
  | TINT  { TInt }
  | TBOOL { TBool }
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
