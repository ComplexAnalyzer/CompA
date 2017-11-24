(* Ocamllex scanner for CompA *)

{ open Parser }

let ascii = [' '-'!' '#'-'[' ']'-'~']
let string_literal = '"' ((ascii)* as s) '"' 
let digit = ['0'-'9']
let float = (digit+) ['.'] digit+ 
let complex = ['(']float[',']float[')']



rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
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
<<<<<<< HEAD
| "cx"     { COMPLEX } 
| "float"  { FLOAT} 
=======
(*| "cx"     { COMPLEX } *)
| "float"  { FLOAT}
>>>>>>> b089bb1e49fcdf9fccd537a13d878bacee1551b0

(* Data Values *)
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| string_literal { STRLIT(s) }
<<<<<<< HEAD
| float as lxm { FLOATLIT(float_of_string lxm) } 
| complex as lxm { CXLIT(complex_of_string lxm)} 
=======
| float as lxm { FLOATLIT(float_of_string lxm) } (*#add*) 
(*| complex as lxm { CXLIT(complex_of_string lxm)}*) 
>>>>>>> b089bb1e49fcdf9fccd537a13d878bacee1551b0
| digit+ as lxm { INTLIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
