(* Ocamllex scanner for CompA *)

{ open Parser }

let ascii = [' '-'!' '#'-'[' ']'-'~']
let string_literal = '"' ((ascii)* as s) '"' 

rule token = parse
(* Whitespace *)
  [' ' '\t' '\r' '\n'] { token lexbuf }

(* Comments *)
| "/*"     { comment lexbuf }

(* Delimeters *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQRBR }
| ']'      { RSQRBR }
| ';'      { SEMI }
| ','      { COMMA }

(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }

(* Logical Operators *)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }

(* Control Flow *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Data Types *)
| "int"    { INT }
| "bool"   { BOOL }
| "string" { STRING }
| "mx"     { MATRIX }

(* Data Values *)
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| string_literal { STRLIT(s) }
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }

(* Identifiers *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* End of File and Invalid Characters *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
