(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or 

type uop = Neg | Not

type typ = Int | Bool | Void | String | Float | Complex | Illegal(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
    | Matrix1DType of typ * int
    | Matrix2DType of typ * int * int
    | Matrix1DPointer of typ
    | Matrix2DPointer of typ

type bind = typ * string

type expr =
    IntLit of int
  | FloatLit of float
  | StrLit of string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of string * expr list
  | Cx of expr * expr
  | ComplexAccess of string * expr
  | Cxassign of string * expr * expr
  | Noexpr
  | PointerIncrement of string(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
    | MatrixLiteral of expr list
    | Matrix1DAccess of string * expr 
    | Matrix2DAccess of string * expr * expr
    | Matrix1DReference of string
    | Matrix2DReference of string
    | Dereference of string
    | Len of string
    | Height of string
    | Width of string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"


let string_of_matrix m =(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  let rec string_of_matrix_lit = function
      [] -> "]"
    | [hd] -> (match hd with
                IntLit(i) -> string_of_int i
              | FloatLit(i) -> string_of_float i
              | BoolLit(i) -> string_of_bool i
              | Id(s) -> s
              | _ -> raise( Failure("Illegal expression in matrix literal") )) ^ string_of_matrix_lit []
    | hd::tl -> (match hd with
                    IntLit(i) -> string_of_int i ^ ", "
                  | FloatLit(i) -> string_of_float i ^ ", "
                  | BoolLit(i) -> string_of_bool i ^ ", "
                  | Id(s) -> s
                  | _ -> raise( Failure("Illegal expression in matrix literal") )) ^ string_of_matrix_lit tl
  in
  "[" ^ string_of_matrix_lit m


let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | StrLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(3.1415926535897932384626433832795) -> string_of_float 3.1415926535897932384626433832795
  | Id(s) -> s
  | Cx(e1, e2) -> "(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | ComplexAccess(s, e1) -> s ^ "[" ^ string_of_expr e1 ^ "]" 
  | Cxassign(v, e1, e2) -> v ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(e1, e2) -> (string_of_expr e1) ^ " = " ^ (string_of_expr e2) 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

  | PointerIncrement(s) -> "++" ^ s (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  | MatrixLiteral(m) -> string_of_matrix m
    | Matrix1DAccess(s, r1) -> s ^ "[" ^ (string_of_expr r1) ^ "]"
    | Matrix2DAccess(s, r1, r2) -> s ^ "[" ^ (string_of_expr r1) ^ "]" ^ "[" ^ (string_of_expr r2) ^ "]"
    | Matrix1DReference(s) -> "%" ^ s
    | Matrix2DReference(s) -> "%%" ^ s
    | Dereference(s) -> "#" ^ s
    | Len(s) -> "len(" ^ s ^ ")"
    | Height(s) -> "height(" ^ s ^ ")"
    | Width(s) -> "width(" ^ s ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s 

let rec string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"
  | Complex -> "cx"
  | Matrix1DType(t, i1) -> string_of_typ t ^ "[" ^ string_of_int i1 ^ "]"
  | Matrix2DType(t, i1, i2) -> string_of_typ t ^ "[" ^ string_of_int i1 ^ "]" ^ "[" ^ string_of_int i2 ^ "]"
  | Matrix1DPointer(t) -> string_of_typ t ^ "[]"
  | Matrix2DPointer(t) -> string_of_typ t ^ "[][]"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
