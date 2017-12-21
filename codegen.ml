(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
module Semant = Semant 
open Exceptions(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "CompA"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and str_t  = L.pointer_type (L.i8_type context)
  and pointer_t = L.pointer_type(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
  and array_t   = L.array_type
  and void_t = L.void_type context
  and f32_t   = L.float_type context in
  let f32x4_t = L.vector_type f32_t 4 
  and float_t = L.double_type context in
  let cx_t = L.array_type float_t 2 in

  (*let cx_pointer_t = L.pointer_type float_t in*)
  (*let cx_fst = L.extractvalue cx_t 1 in*)
  (*let cx_snd = L.extractvalue cx_t 2 in*)

let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> float_t
    | A.String -> str_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.Matrix1DType(typ, size) -> (match typ with(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                                            A.Int -> array_t i32_t size
                                          | A.Float -> array_t float_t size
                                          | A.Bool -> array_t i1_t size
                                          | A.Matrix2DType(typ, size1, size2) -> (match typ with
                                                                                  A.Int -> array_t (array_t i32_t size2) size1
                                                                                | A.Float -> array_t (array_t float_t size2) size1
                                                                                | _ -> raise ( UnsupportedMatrixType )
                                                                              )
                                          | _ -> raise ( UnsupportedMatrixType )
                                         ) 
    | A.Matrix2DType(typ, size1, size2) -> (match typ with
                                            A.Int -> array_t (array_t i32_t size2) size1
                                          | A.Float -> array_t (array_t float_t size2) size1
                                          | A.Matrix1DType(typ1, size3) -> (match typ1 with
                                                                         | A.Int -> array_t (array_t (array_t i32_t size3) size2) size1
                                                                         | A.Float -> array_t (array_t (array_t float_t size3) size2) size1
                                                                         | _ -> raise (UnsupportedMatrixType)
                                                                        )
                                          | _ -> raise ( UnsupportedMatrixType )
                                         )
    | A.Matrix1DPointer(t) -> (match t with
                                   A.Int -> pointer_t i32_t
                                 | A.Float -> pointer_t float_t
                                 | _ -> raise (IllegalPointerType))
    | A.Matrix2DPointer(t) -> (match t with
                                   A.Int -> pointer_t i32_t
                                 | A.Float -> pointer_t float_t
                                 | _ -> raise (IllegalPointerType))
    | A.Complex -> cx_t in
  
  (*
  let pointer_wrapper =
    List.fold_left (fun m name -> StringMap.add name (L.named_struct_type context name) m)
    StringMap.empty ["string"; "int"; "void"; "bool"]
  in
  (* Set the struct body (fields) for each of the pointer struct types *)
  List.iter2 (fun n l -> let t = StringMap.find n pointer_wrapper in
  ignore(L.struct_set_body t (Array.of_list(l)) true))
  ["int"; "string"; "void"; "bool"]
  [[L.pointer_type i32_t; i32_t; i32_t];
  [L.pointer_type str_t; i32_t; i32_t];
  [L.pointer_type void_t; i32_t; i32_t]; [L.pointer_type i1_t; i32_t; i32_t]];
  *)
  
  (* Declare each global variable; remember its value in a map *)
  
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in


  
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let sqrtps = L.declare_function "llvm.qrt.f64"
     (L.function_type float_t [|float_t|]) the_module in

  let sinps = L.declare_function "llvm.sin.f64"
     (L.function_type float_t [|float_t|]) the_module in

  let cosps = L.declare_function "llvm.cos.f64"
     (L.function_type float_t [|float_t|]) the_module in

  let powips = L.declare_function "llvm.powi.f64"
     (L.function_type float_t [|float_t; i32_t |]) the_module in

  let powps = L.declare_function "llvm.pow.f64"
     (L.function_type float_t [|float_t; float_t |]) the_module in


  let expps = L.declare_function "llvm.exp.f64"
     (L.function_type float_t [|float_t |]) the_module in

  let logps = L.declare_function "llvm.log.f64"
     (L.function_type float_t [|float_t |]) the_module in

  let log10ps = L.declare_function "llvm.log10.f64"
     (L.function_type float_t [|float_t |]) the_module in

  let fabsps = L.declare_function "llvm.fabs.f64"
     (L.function_type float_t [|float_t |]) the_module in

  let minps = L.declare_function "llvm.minnum.f64"
     (L.function_type float_t [|float_t;float_t  |]) the_module in

  let maxps = L.declare_function "llvm.maxnum.f64"
     (L.function_type float_t [|float_t;float_t|]) the_module in

  let roundps = L.declare_function "llvm.trunc.f64"
     (L.function_type float_t [|float_t|]) the_module in






  

  (*let printcx_t = L.var_arg_function_type i32_t [| cx_fst;cx_snd |] in
  let printcx_func = L.declare_function "printcx" printf_t the_module in*)
  
  (*let s = build_global_stringptr "Hello, world!\n" "" builder in
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep s [| zero |] "" builder in
  let _ = build_call printf [| s |] "" builder in
  let _ = build_ret (const_int i32_t 0) builder in
  *)

  
  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    let cx_format_str = L.build_global_stringptr "(%f,%f)\n" "fmt" builder in
    let intl_format_str = L.build_global_stringptr "%d" "fmt" builder in
    let floatl_format_str = L.build_global_stringptr "%f" "fmt" builder in
    let strl_format_str = L.build_global_stringptr "%s" "fmt" builder in
    let cxl_format_str = L.build_global_stringptr "(%f,%f)" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in




    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars in
    let check_func = 
          List.fold_left(fun m(t,n) -> StringMap.add n t m )
          StringMap.empty(globals@fdecl.A.formals@fdecl.A.locals)
    in
    let type_of_identifier s = 
        let symbols = check_func in StringMap.find s symbols in



  let build_complex_argument s builder =(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
            L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0|] s builder
    in

    let build_complex_access s i1 i2 builder  =
            L.build_load (L.build_gep (lookup s) [| i1; i2|] s builder) s builder
    in 

    let build_complex_real s builder  =
            L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0;L.const_int i32_t 0 |] s builder) s builder
    in



    let build_1D_matrix_argument s builder =
      L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0 |] s builder
    in

    let build_2D_matrix_argument s builder =
      L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0; L.const_int i32_t 0 |] s builder
    in


    let build_1D_matrix_access s i1 i2 builder isAssign =
      if isAssign
        then L.build_gep (lookup s) [| i1; i2 |] s builder
      else
         L.build_load (L.build_gep (lookup s) [| i1; i2 |] s builder) s builder
    in

    let build_2D_matrix_access s i1 i2 i3 builder isAssign =
      if isAssign
        then L.build_gep (lookup s) [| i1; i2; i3 |] s builder
      else
         L.build_load (L.build_gep (lookup s) [| i1; i2; i3 |] s builder) s builder
    in

    let build_pointer_dereference s builder isAssign =
      if isAssign
        then L.build_load (lookup s) s builder
      else
        L.build_load (L.build_load (lookup s) s builder) s builder
    in

    let build_pointer_increment s builder isAssign =
      if isAssign
        then L.build_load (L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 1 |] s builder) s builder
      else
        L.build_in_bounds_gep (L.build_load (L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder) [| L.const_int i32_t 1 |] s builder
    in

    let rec matrix_expression e =
       match e with
       | A.IntLit i -> i
       | A.Binop (e1, op, e2) -> (match op with
              A.Add     -> (matrix_expression e1) + (matrix_expression e2)
            | A.Sub     -> (matrix_expression e1) - (matrix_expression e2)
            | A.Mult    -> (matrix_expression e1) * (matrix_expression e2)
            | A.Div     -> (matrix_expression e1) / (matrix_expression e2)
            | _ -> 0)
       | _ -> 0
    in

    let find_matrix_type matrix =
      match (List.hd matrix) with
        A.IntLit _ -> ltype_of_typ (A.Int)
      | A.FloatLit _ -> ltype_of_typ (A.Float)
      | _ -> raise (UnsupportedMatrixType) in


  
  let rec check_type = function
   A.IntLit _ -> A.Int
  |A.FloatLit _-> A.Float
  |A.StrLit _-> A.String
      | A.BoolLit _ -> A.Bool
      | A.Id s -> type_of_identifier s
      | A.Cx(e1,e2) -> let t1 =  check_type e1 and t2 = check_type e2 in ( match t1 with A.Float when t2= A.Float -> A.Complex)
      | A.Binop(e1, op, e2) -> let t1 = check_type e1 and t2 = check_type e2 in
  (match op with
          Add | Sub | Mult | Div when t1 = A.Int && t2 = A.Int -> A.Int
  | Add | Sub | Mult | Div when t1 = A.Complex && t2 = A.Complex -> A.Complex
  | Add | Sub | Mult | Div when t1 = A.Float && t2 = A.Float -> A.Float
  | Equal | Neq when t1 = t2 -> A.Bool
  | Less | Leq | Greater | Geq when t1 = A.Int && t2 = A.Int -> A.Bool
  | And | Or when t1 = A.Bool && t2 = A.Bool -> A.Bool
        | _ -> A.Illegal
  )
  | A.Unop(op, e)  -> let t = check_type e in
   (match op with
     Neg when t = A.Int -> A.Int
   | Not when t = A.Bool -> A.Bool
   | Neg when t = A.Complex -> A.Complex
   | Neg when t = A.Float -> A.Float
   | _ -> Illegal)
   | A.Noexpr -> A.Void
   | A.Assign(_, e)  -> check_type e                            
   | A.Call(fname, actuals) as call -> A.Illegal 
   | A.ComplexAccess(s, c) -> A.Float
      | PointerIncrement(s) -> A.Int
      | MatrixLiteral s -> A.Int
      | Matrix1DAccess(s, e1) -> A.Int
      | Matrix2DAccess(s, e1, e2) -> A.Int
      | Len(s) -> A.Int
      | Height(s) -> A.Int
      | Width(s) -> A.Int
      | Dereference(s) -> A.Int
      | Matrix1DReference(s) -> A.Int
      | Matrix2DReference(s) -> A.Int    
    in


    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	(*TODO*)
    (* A.Literal i -> L.const_int i32_t i*)
    A.IntLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float float_t f
      | A.StrLit s -> L.build_global_stringptr s "string" builder
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Cx(e1,e2) -> 
      let e1' = expr builder e1
      and e2' = expr builder e2 in L.const_array float_t [|e1';e2'|] 
      | A.ComplexAccess (s, e) -> let i1 = expr builder e in (match (type_of_identifier s) with
                                            A.Complex -> (build_complex_access s (L.const_int i32_t 0) i1 builder)
                                            | _ -> build_complex_access s (L.const_int i32_t 0) i1 builder)

      | A.MatrixLiteral s -> L.const_array (find_matrix_type s) (Array.of_list (List.map (expr builder) s))(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
      | A.Matrix1DReference (s) -> build_1D_matrix_argument s builder
      | A.Matrix2DReference (s) -> build_2D_matrix_argument s builder
      | A.Len s -> (match (type_of_identifier s) with A.Matrix1DType(_, l) -> L.const_int i32_t l 
                                                      | _ -> L.const_int i32_t 0 )
      | A.Height s -> (match (type_of_identifier s) with A.Matrix2DType(_, l, _) -> L.const_int i32_t l
                                                      | _ -> L.const_int i32_t 0 )
      | A.Width s -> (match (type_of_identifier s) with A.Matrix2DType(_, _, l) -> L.const_int i32_t l
                                                      | _ -> L.const_int i32_t 0 )
      | A.Matrix1DAccess (s, e1) -> let i1 = expr builder e1 in (match (type_of_identifier s) with 
                                                      A.Matrix1DType(_, l) -> (
                                                        if (matrix_expression e1) >= l then raise(MatrixOutOfBounds)
                                                        else build_1D_matrix_access s (L.const_int i32_t 0) i1 builder false)
                                                      | _ -> build_1D_matrix_access s (L.const_int i32_t 0) i1 builder false )
      | A.Matrix2DAccess (s, e1, e2) -> let i1 = expr builder e1 and i2 = expr builder e2 in (match (type_of_identifier s) with 
                                                      A.Matrix2DType(_, l1, l2) -> (
                                                        if (matrix_expression e1) >= l1 then raise(MatrixOutOfBounds)
                                                        else if (matrix_expression e2) >= l2 then raise(MatrixOutOfBounds)
                                                        else build_2D_matrix_access s (L.const_int i32_t 0) i1 i2 builder false)
                                                      | _ -> build_2D_matrix_access s (L.const_int i32_t 0) i1 i2 builder false )
      | A.PointerIncrement (s) ->  build_pointer_increment s builder false
      | A.Dereference (s) -> build_pointer_dereference s builder false
      | A.Binop (e1, op, e2) ->
       let e1' = expr builder e1
       and e2' = expr builder e2 
       and typ = check_type e1 in
       (if typ = A.Float then
	  (match op with
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
    | A.Div     -> L.build_fdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Ult
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  ) e1' e2' "tmp" builder else (match op with
      A.Add     -> L.build_add
    | A.Sub     -> L.build_sub
    | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Equal   -> L.build_icmp L.Icmp.Eq
    | A.Neq     -> L.build_icmp L.Icmp.Ne
    | A.Less    -> L.build_icmp L.Icmp.Slt
    | A.Leq     -> L.build_icmp L.Icmp.Sle
    | A.Greater -> L.build_icmp L.Icmp.Sgt
    | A.Geq     -> L.build_icmp L.Icmp.Sge
    ) e1' e2' "tmp" builder)
| A.Unop(op, e) ->
    let e' = expr builder e in
    let t = check_type e in
    (match op with
        A.Neg when t = A.Int -> L.build_neg
      | A.Neg when t = A.Float -> L.build_fneg
      | A.Not     -> L.build_not) e' "tmp" builder

      | A.Assign (e1, e2) -> let e1' = (match e1 with(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                                            A.Id s -> lookup s
                                          | A.Matrix1DAccess (s, e1) -> let i1 = expr builder e1 in (match (type_of_identifier s) with 
                                                      A.Matrix1DType(_, l) -> (
                                                        if (matrix_expression e1) >= l then raise(MatrixOutOfBounds)
                                                        else build_1D_matrix_access s (L.const_int i32_t 0) i1 builder true)
                                                      | _ -> build_1D_matrix_access s (L.const_int i32_t 0) i1 builder true )
                                          | A.Matrix2DAccess (s, e1, e2) -> let i1 = expr builder e1 and i2 = expr builder e2 in (match (type_of_identifier s) with 
                                                      A.Matrix2DType(_, l1, l2) -> (
                                                        if (matrix_expression e1) >= l1 then raise(MatrixOutOfBounds)
                                                        else if (matrix_expression e2) >= l2 then raise(MatrixOutOfBounds)
                                                        else build_2D_matrix_access s (L.const_int i32_t 0) i1 i2 builder true)
                                                      | _ -> build_2D_matrix_access s (L.const_int i32_t 0) i1 i2 builder true )
                                          | A.PointerIncrement(s) -> build_pointer_increment s builder true
                                          | A.Dereference(s) -> build_pointer_dereference s builder true
                                          | _ -> raise (IllegalAssignment)
                                          )
                            and e2' = expr builder e2 in
                     ignore (L.build_store e2' e1' builder); e2'
      (*| A.Assign (s, e) -> let e' = expr builder e in             
	                   ignore (L.build_store e' (lookup s) builder); e'*)

      | A.Cxassign (s,e1,e2) ->  let e1' = expr builder e1
                                 and e2' = expr builder e2 in                       
                                 let comp = L.build_gep (lookup s) [| L.const_int i32_t 0 ; e1'|] s builder in 
                                 ignore (L.build_store e2' comp builder); e2'
      | A.Call ("println", [e]) | A.Call ("printb", [e]) -> (match check_type e with 
      A.Float -> L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      |A.Int -> L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
      |A.String -> L.build_call printf_func [| str_format_str ; (expr builder e) |]
      "printf" builder
      |A.Complex  ->
       L.build_call printf_func [| cx_format_str ; (expr builder e)|]
      "printf" builder
      )
      |A.Call ("print", [e]) ->(match check_type e with 
       A.Float -> L.build_call printf_func [| floatl_format_str ; (expr builder e) |]
      "printf" builder
      |A.Int -> L.build_call printf_func [| intl_format_str ; (expr builder e) |]
      "printf" builder
      |A.String -> L.build_call printf_func [| strl_format_str ; (expr builder e) |]
      "printf" builder
      |A.Complex  ->
       L.build_call printf_func [| cxl_format_str ; (expr builder e)|]
      "printf" builder
      )
      |A.Call ("sqrt", [e1])  -> L.build_call sqrtps [| (expr builder e1)|] "sqrt" builder 
      |A.Call ("sin", [e1])  -> L.build_call sinps [| (expr builder e1)|] "sin" builder
      |A.Call ("cos", [e1])  -> L.build_call cosps [| (expr builder e1)|] "cos" builder
      |A.Call ("powi", [e1;e2])  -> L.build_call powips [| (expr builder e1);(expr builder e2)|] "powi" builder
      |A.Call ("pow", [e1;e2])  -> L.build_call powps [| (expr builder e1);(expr builder e2)|] "pow" builder   
      |A.Call ("exp", [e1])  -> L.build_call expps [| (expr builder e1)|] "exp" builder 
      |A.Call ("log", [e1])  -> L.build_call logps [| (expr builder e1)|] "log" builder 
      |A.Call ("log10", [e1])  -> L.build_call log10ps [| (expr builder e1)|] "log10" builder 
      |A.Call ("fabs", [e1])  -> L.build_call fabsps [| (expr builder e1)|] "fabs" builder 
      |A.Call ("min", [e1;e2])  -> L.build_call minps [| (expr builder e1);(expr builder e2)|] "fabs" builder 
      |A.Call ("max", [e1;e2])  -> L.build_call maxps [| (expr builder e1);(expr builder e2)|] "max" builder                                                                               
      |A.Call ("rnd", [e1])  -> L.build_call roundps [| (expr builder e1)|] "rnd" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | A.Int -> L.build_ret (L.const_int i32_t 0)
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | A.Bool -> L.build_ret (L.const_int i1_t 0)
      | A.Complex -> L.build_ret (L.const_array float_t [|(L.const_float float_t 0.0);(L.const_float float_t 0.0)|] )
      | _ -> L.build_ret (L.const_int i32_t 0))


  in

  List.iter build_function_body functions;
  the_module
