open Ast

type texpr = 
          TIntLit of int * typ
        | TFloatLit of float * typ
        | TStrLit of string * typ
        | TBoolLit of bool * typ
        | TId of string * typ
        | TUnop of unop * texpr * typ
        | TBinop of texpr * op * texpr * typ
        | TCall of string * texpr list * typ
        | TCx of texpr * texpr * typ
        | TNoexpr

   

and tstmt =
    TBlock of tstmt list
  | TExpr of texpr 
  | TReturn of texpr 
  | TAssign of string * texpr 
  | TIf of texpr * tstmt * tstmt
  | TFor of texpr * texpr * texpr * tstmt
  | TWhile of texpr * tstmt

type tfdecl = {
    ttyp : typ;
    tfname : string;
    tlocals : bind list;
    tformals : bind list;
    tbody : tstmt list; 
}

type tglobal = string * texpr 

type tprogram = tglobal list * fdecl list

let tstring_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  

let tstring_of_uop = function
    Neg -> "-"
  | Not -> "!"


let rec tstring_of_typ = function
    Int -> "int"
  | String -> "string"
  | Bool -> "bool" 
  | Void -> "void"
  | Float -> "float"
  | Complex -> "cx"

let rec tstring_of_expr = function
    TIntLit(l,_)           -> string_of_int l
  | TFloatLit(l,_)         -> string_of_float l
  | TStrLit(s, _)           -> s
  | TBoolLit(true, _)       -> "true"
  | TBoolLit(false, _)      -> "false"
  | Cx(e1,e2,_) -> "(" ^ tstring_of_expr e1 ^","^ tstring_of_expr e2^")"
  | TId(s, _)               -> s
  | TBinop(e1, o, e2, _)    ->
      tstring_of_expr e1 ^ " " ^ tstring_of_op o ^ " " ^ tstring_of_expr e2
  | TUnop(o, e, _)          -> tstring_of_uop o ^ tstring_of_expr e
  | TCall(f, el, _)         -> f ^ "(" ^ String.concat ", " (List.map tstring_of_expr el) ^ ")"
  | TNoexpr                 -> "noexpr"
 


let rec tstring_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map tstring_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> tstring_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ tstring_of_expr expr ^ ";\n";
  | If(e, s, TBlock([])) -> "if (" ^ tstring_of_expr e ^ ")\n" ^ tstring_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ tstring_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ tstring_of_stmt s2
  | For(e1, e2, e3, s) -> "for (" ^ tstring_of_expr e1  ^ " ; " ^ tstring_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ tstring_of_stmt s
  | While(e, s) -> "while (" ^ tstring_of_expr e ^ ") " ^ tstring_of_stmt s 


let tstring_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Void -> "void"
  | Complex -> "cx"
  

let tstring_of_vdecl (t, id) = tstring_of_typ t ^ " " ^ id ^ ";\n"

let tstring_of_fdecl fdecl =
  tstring_of_typ fdecl.ttyp ^ " " ^
  fdecl.tfname ^ "(" ^ String.concat ", " (List.map snd fdecl.tformals) ^
  ")\n{\n" ^
  String.concat "" (List.map tstring_of_vdecl (snd fdecl.tlocals)) ^
  String.concat "" (List.map tstring_of_stmt fdecl.tbody) ^
  "}\n"

let tstring_of_program (vars, funcs) =
  String.concat "" (List.map tstring_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map tstring_of_fdecl funcs)




