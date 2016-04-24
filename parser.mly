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
%token FUN ARROW
%token EOF

%nonassoc IN
%nonassoc LET
%nonassoc FUN
%nonassoc IF
%left EQ LT GT LE GE
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
  | INT                                  { Int $1 }
  | VAR                                  { Var $1 }
  | TRUE                                 { Bool true }
  | FALSE                                { Bool false }
  | expr binop expr                      { Binop ($2, $1, $3) }
  | LET VAR EQ expr IN expr              { Let ($2, $4, $6) }
  | LET REC VAR EQ expr IN expr          { Letrec ($3, $5, $7) }
  | IF expr THEN expr ELSE expr %prec IF { If ($2, $4, $6) }
  | FUN VAR ARROW expr %prec FUN         { Fun ($2, $4) }
  | expr expr %prec APP                  { App ($1, $2) }
  | LPAREN expr RPAREN                   { $2 }
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
