{
  open Parser

  exception Error of string

  let position lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    pos.pos_lnum, pos.pos_cnum - pos.pos_bol

  let lexing_error lexbuf =
    let input = Lexing.lexeme lexbuf in
    let msg = Printf.sprintf "Unexpected `%s'" input in
    raise (Error msg)
}

let whitespace = [' ' '\t']+
let comment = '#' [^ '\n']*
let newline = '\n'
let alpha = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let integer = digit+
let boolean = "true" | "false"
let identifier = ('_' | alpha) ('_' | alpha | digit)*

rule read = parse
  | whitespace
  | comment            { read lexbuf }
  | newline            { Lexing.new_line lexbuf; read lexbuf }
  | "~"                { UNARY_MINUS }
  | "+"                { PLUS }
  | "-"                { MINUS }
  | "*"                { TIMES }
  | "/"                { DIV }
  | "="                { EQ }
  | "<>"               { NE }
  | "<"                { LT }
  | ">"                { GT }
  | "<="               { LE }
  | ">="               { GE }
  | "let"              { LET }
  | "rec"              { REC }
  | "in"               { IN }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "fun"              { FUN }
  | "->"               { ARROW }
  | "=>"               { DARROW }
  | ":"                { COLON }
  | ","                { COMMA }
  | "("                { LPAREN }
  | ")"                { RPAREN }
  | integer as i       { INT (int_of_string i) }
  | boolean as b       { BOOL (bool_of_string b) }
  | identifier as n    { VAR n }
  | eof                { EOF }
  | _                  { lexing_error lexbuf }
