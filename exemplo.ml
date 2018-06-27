(*
	Trabalho 1+2 - L1 sem/com recursao
	Data de entrega: 07/06/2015
	Grupo: Andrey Blazejuk & Luiza Hagemann
*)

(*
	Grammar:
	e ::= 
		n 
		| b 
		| e1 op e2 
		| if e1 then e2 else e3
		| x 
		| e1 e2 
		| fn x ⇒ e 
		| let x = e1 in e2
		| let rec f = fn x => e1 in e2
  
*)
	type variable = string;;
	type operator =
	    Sum 
	  | Diff 
	  | Mult
	  | Div
	  | Mod
	  | Equal
	  | NotEqual
	  | Less
	  | Greater
	  | LessOrEqual
	  | GreaterOrEqual
	  
	  ;;
		
	type expression =
	    Num of int 
	  | Bool of bool 
	  | BinOp of expression * operator * expression
	  | If of expression * expression * expression 
	  | Var of variable 
	  | Applic of expression * expression
	  | Function of variable * expression 
	  | Let of variable * expression * expression
	  | LetRec of variable * expression * expression
	  ;;
	
	type value = 
	    VNum of int 
	  | VBool of bool 
	  | VClosure of variable * expression * environment
	  | VRecClosure of variable * variable * expression * environment
	and
	    environment = (variable * value) list;;



(* função auxiliar para remover tupla de lista: *)
let remove_tuple var list =
	List.filter (fun (k, _) -> k <> var) list 

(* environment control	
   EMPTY *)
let empty_env : environment = []


(* UPDATE *)
let update_environment var v1 env : environment = match env with

| [] -> [(var,v1)]
| hd::tl -> 
    if (List.exists (fun (k, _) -> k = var) env) 
    then List.append (remove_tuple var env) [(var,v1)]
    else List.append env [(var,v1)]


(* LOOKUP *)
let rec lookup_environment var env : value = match env with

| [] -> raise Not_found
| (k,v)::tl ->
  if (k = var)
  then v
  else lookup_environment var tl


(* EVALUATION *)  
  
let rec eval (environment:environment) (e:expression) : value = match e with

(* VALUES *)
  Num(n) -> VNum(n)
| Bool(b) -> VBool(b)


(* OPERATIONS *)

| BinOp(e1,op,e2) ->
    let n1 = eval environment e1 in
    let n2 = eval environment e2 in
    (match n1, op, n2 with
        VNum(n1), Sum, VNum(n2) -> VNum(n1 + n2)
      | VNum(n1), Diff, VNum(n2) -> VNum(n1 - n2)
      | VNum(n1), Mult, VNum(n2) -> VNum(n1 * n2)
      | VNum(n1), Div, VNum(n2) -> VNum(n1 / n2)
      | VNum(n1), Mod, VNum(n2) -> VNum(n1 mod n2)

      | VNum(n1), Equal, VNum(n2) -> VBool(n1 = n2)
      | VBool(n1),Equal, VBool(n2) -> VBool(n1 = n2)

      | VNum(n1), NotEqual, VNum(n2) -> VBool(n1 <> n2)
      | VBool(n1), NotEqual, VBool(n2) -> VBool(n1 <> n2)

      | VNum(n1), Less, VNum(n2) -> VBool(n1 < n2)
      | VNum(n1), Greater, VNum(n2) -> VBool(n1 > n2)
      | VNum(n1), LessOrEqual, VNum(n2) -> VBool(n1 <= n2)
      | VNum(n1), GreaterOrEqual, VNum(n2) -> VBool(n1 >= n2)
      | _ -> failwith "unimplemented"
    )


(* IF *)

| If(e1,e2,e3) when ((eval environment e1) = VBool(true)) -> eval environment e2
| If(e1,e2,e3) when ((eval environment e1) = VBool(false)) -> eval environment e3


(* VARIABLE  *)

| Var(variable) -> lookup_environment variable environment


(* FUNCTION  *)

| Function(variable,e) -> VClosure(variable, e, environment)


(* APPLICATION *)

| Applic(e1,e2) ->
    let v1 = eval environment e1  in
    let v2 = eval environment e2 in
    (match v1, v2 with
        VClosure(var,e,env), v -> eval (update_environment var v env) e
      | VRecClosure(f,x,e,env), v -> eval (update_environment f (VRecClosure(f,x,e,env)) (update_environment x v env)) e
      | _ -> failwith "unimplemented"
    )


(* LET *)

| Let(var,e1,e2) -> 
    let v1 = eval environment e1 in
    eval (update_environment var v1 environment) e2


(* LETREC *)

| LetRec(f,e1, e2) -> 
    let v1 = eval environment e1 in
    ( match v1 with
        VClosure(x,e,env) -> eval (update_environment f (VRecClosure(f,x,e,environment)) env) e2
      | _ -> failwith "unimplemented"
    )


(* else (everything else fails) *)

| _ -> failwith "unimplemented"

;;


(* TESTS *)

let environment = empty_env;;

(* simple language values *)

let numPass = VNum(10);;
(* let numFail = Num(true);; ocaml doesn't let it pass *)
let boolPass = VBool(true);;
(* let boolFail = Bool(2);; ocaml doesn't let it pass *)


(* map structure for environment *)

let env = update_environment "numPass" numPass environment;;
lookup_environment "numPass" env;;
lookup_environment "numFail" env;;
let env' = update_environment "one" (VNum(1)) env;;
let env'' = update_environment "numPass" (VNum(2)) env';;
lookup_environment "numPass" env';;


(* EXPRESSIONS
   binary operators *)

let sumPass = BinOp(Num(1), Sum, Num(1));;
let sumFail = BinOp(Num(1), Sum, Bool(true));;

let diffPass = BinOp(Num(2), Diff, Num(1));;
let diffFail = BinOp(Bool(false), Diff, Num(1));;

let multPass = BinOp(Num(2), Mult, Applic(Function("x", BinOp(Var("x"),Sum,Num(1))),Num(1)));;
let multFail = BinOp(Num(2), Mult, Bool(true));;

let divPass = BinOp(Num(4), Div, Num(2));;
let divFail = BinOp(Bool(false), Div, Num(2));;
let divRaise = BinOp(Num(3), Div, Num(0));;

let equalNumTruePass = BinOp(Num(2), Equal, Num(2));;
let equalNumFalsePass = BinOp(Num(1), Equal, Num(2));;
let equalNumFail = BinOp(Num(1), Equal, Bool(true));;

let notEqualTruePass = BinOp(Num(1), NotEqual, Num(2));;
let notEqualFalsePass = BinOp(Num(1), NotEqual, Num(1));;

let leOrEqTruePass = BinOp(Num(1), LessOrEqual, Num(2));;
let leOrEqFalsePass = BinOp(Num(2), LessOrEqual, Num(1));;
let leOrEqFail = BinOp(Num(1), LessOrEqual, Bool(false));;

let greOrEqTruePass = BinOp(Num(1), GreaterOrEqual, Num(0));;
let greOrEqFalsePass = BinOp(Num(0), GreaterOrEqual, Num(1));;


(* if *)

let ifTruePass = If(BinOp(Num(1), Equal, Num(1)), Bool(true), Bool(false));;
let ifFalsePass = If(BinOp(Num(2), Equal, Num(1)), Bool(true), Bool(false));;
let ifFail = If(BinOp(Num(1), Sum, Num(1)), Num(2), Num(3));;


(* function *)

let funcPass = Function("x",BinOp(Var("x"),Sum,Num(1)));;


(* application *)

let varX = Var("x");;
let applicPass = Applic(Function("x", BinOp(varX,Sum,Num(1))),Num(1));;
let applicFail = Applic(Var("x"),Num(1));


(* let *)

let letPass1 = Let("x", Num(1), BinOp(Var("x"),Sum,Num(1)));;
let letPass2 = Let("x", Num(2), Let("y", Num(2), BinOp(Var("x"),Mult,Var"y")));;
let letPass3 = Let("x", Num(0), If(BinOp(Var("x"),Equal,Num(0)), Bool(true), Bool(false)));;


(* letrec *)

let letRecPass1 = LetRec("fat", 
			Function(
			"x", 
			If(BinOp(Var("x"), Equal, Num(0)), 
			   Num(1), 
			   BinOp(Var("x"), Mult, Applic(Var("fat"), BinOp(Var("x"), Diff, Num(1))))
			  )), 
			Applic(Var("fat"),Num(5))
		       );;

let letRecPass2 = LetRec("fib",
			 Function("x",
				  If(BinOp(Var("x"),
					       Less,
					       Num(2)
					  ),
				     Var("x"),
				     BinOp(Applic(Var("fib"),
						  BinOp(Var("x"),
							Diff,
							Num(1)
						        )
						  ),
					   Sum,
					   Applic(Var("fib"),
						  BinOp(Var("x"),
							Diff,
							Num(2)
						        )
						  )
					   )
				      )
				    )
			   ,
			 Applic(Var("fib"),Num(6)));;


(* EVAL *)

(* binary operations *)

let evalSumPass = eval environment sumPass;;
let evalSumFail = eval environment sumFail;;

let evalDiffPass = eval environment diffPass;;
let evalDiffFail = eval environment diffFail;;

let evalMultPass = eval environment multPass;;
let evalMultFail = eval environment multFail;;

let evalDivPass = eval environment divPass;;
let evalDivFail = eval environment divFail;;
let evalDivRaise = eval environment divRaise;;

let evalEqualNumTruePass = eval environment equalNumTruePass;;
let evalEqualNumFalsePass = eval environment equalNumFalsePass;;
let evalEqualNumFail = eval environment equalNumFail;;

let evalNotEqualTruePass = eval environment notEqualTruePass;;
let evalNotEqualFalsePass = eval environment notEqualFalsePass;;

let evalLeOrEqTruePass = eval environment leOrEqTruePass;;
let evalLeOrEqFalsePass = eval environment leOrEqFalsePass;;
let evalLeOrEqFail = eval environment leOrEqFail;;

let evalGreOrEqTruePass = eval environment greOrEqTruePass;;
let evalGreOrEqFalsePass = eval environment greOrEqFalsePass;;

(* if *)

let evalIfTruePass = eval environment ifTruePass;;
let evalIfFalsePass = eval environment ifFalsePass;;
let evalIfFail = eval environment ifFail;;

(* function *)

let evalFuncPass = eval environment funcPass;;
(*let evalFuncFail = eval environment funcFail;; ocaml doesn't let it pass*)

(* application *)

let evalApplicPass = eval environment applicPass;;
let evalApplicFail = eval environment applicFail;;

(* let *)

let evalLetPass1 = eval environment letPass1;;
let evalLetPass2 = eval environment letPass2;;
let evalLetPass3 = eval environment letPass3;;

(* letrec *)

let evalLetRecPass1 = eval environment letRecPass1;;
let evalLetRecPass2 = eval environment letRecPass2;;