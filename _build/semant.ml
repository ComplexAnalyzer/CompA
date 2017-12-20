(*Semantic checking for the CompA *)

open Ast
module StringMap = Map.Make(String)
(* module E = Exceptions *)


(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)
let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in

  let check_cxassign lvaluec index rvaluec err =
     if lvaluec == Complex && index== Int && rvaluec == Float then lvaluec else raise err
  in
  
  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);


   
  (* Function declaration for a named function *)
     let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [( Float, "x")];
       locals = []; body = [] } (StringMap.add "sqrt"
     { typ = Float; fname = "sqrt"; formals = [(Float,"x")];
       locals = [];body =  []   } (StringMap.add "sin"
     { typ = Float; fname = "sin"; formals = [(Float,"x")];
       locals = [];body =  []   } (StringMap.add "cos"
     { typ = Float; fname = "cos"; formals = [(Float,"x")];
       locals = [];body =  []   } (StringMap.add "powi"
     { typ = Float; fname = "powi"; formals = [(Float,"x");(Int,"y")];
       locals = [];body =  []   } (StringMap.add "pow"
     { typ = Float; fname = "pow"; formals = [(Float,"x");(Float,"y")];
       locals = [];body =  []   } (StringMap.add "exp"
     { typ = Float; fname = "exp"; formals = [(Float,"x")];
       locals = [];body =  []   } (StringMap.add "log"
     { typ = Float; fname = "log"; formals = [(Float,"x")];
       locals = [];body =  []   } (StringMap.add "log10"
     { typ = Float; fname = "log10"; formals = [(Float,"x")];
       locals = [];body =  []   } (StringMap.add "fabs"
     { typ = Float; fname = "fabs"; formals = [(Float,"x")];
       locals = []; body =  []   } (StringMap.add "min"
     { typ = Float; fname = "min"; formals = [(Float,"x");(Float,"y")];
       locals = [];body =  []   } (StringMap.add "max"
     { typ = Float; fname = "max"; formals = [(Float,"x");(Float,"y")];
       locals = []; body =  []   } (StringMap.add "rnd"
     { typ = Float; fname = "rnd"; formals = [(Float,"x")];
       locals = []; body =  []   }(StringMap.singleton "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] })))))))))))))
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
	      IntLit _ -> Int
      | FloatLit _-> Float
      | StrLit _-> String
      | BoolLit _ -> Bool
      | Id s -> type_of_identifier s
      | ComplexAccess (s, e) -> let _ = (match (expr e) with
                                          Int -> Int
                                        | _ -> raise (Failure ("Complex index should be integer"))) in
                                         (type_of_identifier s)
      | Cx(e1,e2) -> let t1 =  expr e1 and t2 = expr e2 in 
                      ( match t1 with 
                        Float -> (match t2 with 
                                  Float -> Complex
                                | _ -> raise (Failure ("illegal element type of Complex number " ^
                                       string_of_typ t2 ^" in " ^ string_of_expr e2)))
                        |_ -> raise (Failure ("illegal element type of Complex number " ^
                              string_of_typ t1 ^" in " ^ string_of_expr e1))
                      )

      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	(match op with
          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
  | Add | Sub | Mult | Div when t1 = Complex && t2 = Complex -> Complex
  | Add | Sub | Mult | Div when t1 = Float && t2 = Float -> Float
	| Equal | Neq when t1 = t2 -> Bool
	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
  | Less | Leq | Greater | Geq when t1 = Float && t2 = Float -> Bool
	| And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
   | Neg when t = Complex -> Complex
   | Neg when t = Float -> Float
   | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
   | Noexpr -> Void
   | Assign(var, e) as ex ->   let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex))

   | Cxassign(var,e1,e2) as ex -> let lt = type_of_identifier var
                                  and index = expr e1 
                                  and num = expr e2 in
        check_cxassign lt index num (Failure ("illegal assignment of complex" ^ string_of_typ lt ^
             " = " ^ string_of_typ num ^ " in " ^
             string_of_expr ex  ^ "with" ^ string_of_typ index ))

   | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 ( fun (ft, _) e -> let et = expr e in
              if et = Complex then ignore (check_assign ft Float
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e)))
              else 
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
    and

    check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () 



     (*match two ast*)
  and  expr_to_texpr e  = match e with    
    IntLit(i)           -> Int
  | FloatLit(b)         -> Float
  | StrLit(s)           -> String
  | BoolLit(b)          -> Bool
  | Id(s)               -> expr e
  | Noexpr              -> Void
  | Unop(op, e)         -> expr e
  | Binop(e1, op, e2) as e   -> expr e
  | Call(_, el) as call        -> expr call
  | Cx(_,_)           -> Complex
in
  (* Library functions *)

(* and texpr_to_type texpr = match texpr with  
    TIntLit(_, typ)                  -> typ
  | TFloatLit(_, typ)                -> typ
  | TStrLit(_, typ)                  -> typ
  | TBoolLit(_, typ)                 -> typ
  | TId(_, typ)                      -> typ
  | TBinop(_, _, _, typ)             -> typ
  | TUnop(_, _, typ)                 -> typ
  | TCall(_, _, typ)                 -> typ
  | TCx(_,_,typ)                     -> typ
  | TNoexpr                          -> "void" in *)

    (* Verify a statement or throw an exception *)
  let rec stmt = function
	Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions

  
