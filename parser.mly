
%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LSQRBR RSQRBR
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE PI AND OR
%token RETURN IF ELSE FOR WHILE INT FLOAT BOOL STRING VOID COMPLEX 
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID STRLIT
%token EOF
%token LEN ROW COL PERCENT OCTOTHORP

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc NOLSQRBR
%nonassoc LSQRBR
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | STRING { String }
  | BOOL { Bool }
  | VOID { Void }
  | COMPLEX { Complex }
  | matrix1D_typ { $1 }
  | matrix2D_typ { $1 }
  | matrix1D_pointer_typ { $1 }
  | matrix2D_pointer_typ { $1 }

matrix1D_typ:
    typ LSQRBR INTLIT RSQRBR %prec NOLSQRBR  { Matrix1DType($1, $3) }

matrix2D_typ:
    typ LSQRBR INTLIT RSQRBR LSQRBR INTLIT RSQRBR  { Matrix2DType($1, $3, $6) }

matrix1D_pointer_typ:
  typ LSQRBR RSQRBR %prec NOLSQRBR { Matrix1DPointer($1)}

matrix2D_pointer_typ:
  typ LSQRBR RSQRBR LSQRBR RSQRBR { Matrix2DPointer($1) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    primitives       { $1 }
  | STRLIT           { StrLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | PI               { FloatLit(3.1415926535897932384626433832795)}
  | ID LSQRBR expr RSQRBR { ComplexAccess($1, $3) }
  | ID LSQRBR expr RSQRBR ASSIGN expr { Cxassign($1, $3, $6) }
  | LPAREN expr COMMA expr RPAREN { Cx($2,$4) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | LSQRBR matrix_literal RSQRBR                  { MatrixLiteral(List.rev $2) }
  | ID LSQRBR expr  RSQRBR %prec NOLSQRBR         { Matrix1DAccess($1, $3)}
  | ID LSQRBR expr  RSQRBR LSQRBR expr  RSQRBR    { Matrix2DAccess($1, $3, $6)}
  | PERCENT ID                                    { Matrix1DReference($2)}
  | PERCENT PERCENT ID                            { Matrix2DReference($3)}
  | OCTOTHORP ID                                  { Dereference($2)}
  | PLUS PLUS ID                                  { PointerIncrement($3) }
  | LEN LPAREN ID RPAREN                          { Len($3) }
  | ROW LPAREN ID RPAREN                          { Row($3) }
  | COL LPAREN ID RPAREN                          { Col($3) }

primitives:
    INTLIT           { IntLit($1) }
  | FLOATLIT         { FloatLit($1) }

matrix_literal:
    primitives                      { [$1] }
  | matrix_literal COMMA primitives { $3 :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
