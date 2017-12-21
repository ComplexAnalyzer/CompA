(* Ocamllex scanner for CompA *)

{ open Parser }

let digit = ['0'-'9']
let ascii = [' '-'!' '#'-'[' ']'-'~']
let string_literal = '"' ((ascii)* as s) '"' 
let float = (digit+)['.'](digit+)

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

(* Reference Dereference *)(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
| '%' { PERCENT } | '#' { OCTOTHORP }

(* Matrices *)(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
|  "len"	{ LEN }		|  	"height" { HEIGHT } |	"width" { WIDTH }

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
| "float"  { FLOAT }
| "cx"     { COMPLEX }


(* Data Values *)
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "PI"     { PI }
| string_literal { STRLIT(s) }
| float as lxm { FLOATLIT(float_of_string lxm) }
| digit+ as lxm { INTLIT(int_of_string lxm) }
| digit*'.'digit+ as lxm { FLOATLIT(float_of_string lxm) }

(* Identifiers *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* End of File and Invalid Characters *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
