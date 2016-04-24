{
  open Parser
  exception Error of string
}

let whitespace = [' ' '\t' '\n']+
let letter = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let int = digit+
let ident = ('_' | letter) ('_' | letter | digit)*

rule read = parse
  | whitespace  { read lexbuf }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "/"         { DIV }
  | "="         { EQ }
  | "<>"        { NE }
  | "<"         { LT }
  | ">"         { GT }
  | "<="        { LE }
  | ">="        { GE }
  | "let"       { LET }
  | "rec"       { REC }
  | "in"        { IN }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "fun"       { FUN }
  | "->"        { ARROW }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | ident as id { VAR id }
  | int as i    { INT (int_of_string i) }
  | eof         { EOF }
  | _           { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
