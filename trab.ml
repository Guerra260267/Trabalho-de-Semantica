(* Grammar:
e ::= n
  | b
  | e1 op e2
  | if e1 then e2 else e3
  | x
  | e1 e2
  | fn x:T ⇒ e
  | let x:T = e1 in e2
  | let rec f:T1 → T2 = (fn y:T1 ⇒ e1) in e2
  | nil
  | e1 :: e2
  | isempty e
  | hd e
  | tl e
  | raise
  | try e1 with e2 
*)

type variable = string ;;

type operator = 
    Sum 
  | Diff 
  | Mult 
  | Div 
  | Eq 
  | Neq 
  | Less
  | Leq 
  | Greater 
  | Geq 
  | And 
  | Or 
  | Not;;

(*
  Aqui são definidos os tipos básicos:
  Inteiro: TyInt
  Boleano: TyBool
  Função: TyFn of tipo * tipo (definição recursiva de tipo)
  Lista: TyList of tipo
  Variável de Tipo: TyX of string
*)
type tipo  = TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo | TyX of string;;

type expr = Num of int 
          | Bool of bool 
          | Bop of operator * expr * expr
          | If of expr * expr * expr 
          | Var of variable 
          | App of expr * expr 
          | Lam of variable * tipo * expr 
          | Let of variable * expr * expr
          | Lrec of variable * tipo * expr
          | Nil
          | Cons of expr * expr
          | IsEmpty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | TryWith of expr * expr
          | LamImpl of variable * expr 
          | LetImpl of variable * expr * expr
          | LrecImpl of variable * variable * expr * expr
          ;;

type value = Vnum of int 
           | Vbool of bool 
           | Vclos of variable * expr * env (* ATENTAR AQUI: uma closure é uma esp. de dict com variavel, expressão e ambiente onde aquela amarração funciona! *)
           | Vrclos of variable * variable * expr * env (*AQUI: a meisma coisa*)
           | Vnil
           | Vcons of value * value
           | Raise     
and  
    env = (variable * value) list ;; 

(* -------------------------- GERÊNCIA DE AMBIENTE ------------------------------- *)

(* Função auxiliar para remover tuplas de uma lista*)
let remove_tuple var list =
  List.filter (fun (k, _) -> k <> var) list 

(* ambiente vazio *)
let empty_env : env = []

(* Função para  remover a amarração de uma variável em um dado ambiente *)
let remove_env_binding var list = 
  List.filter(fun (k,_) -> k <> var) list ;;

(* Função que adiciona amarraçao  *)
let add_env_biding var v e : env = match e with
  | [] -> [(var,v)]
  | hd::tl -> 
    if (List.exists (fun (k, _) -> k = var) e) 
    then List.append (remove_tuple var e) [(var,v)]
    else List.append e [(var,v)]

(* Função que retorna o valor de uma variável em um dado ambiente  *)
let rec look_env_biding var e : value = match e with
    [] -> raise Not_found
  | (v, value)::tl ->
      if (v == var) then value else look_env_biding var tl

(* ---------------------------------------------------------------------------- *)

(* Temos que levar, sempre, em consideração o ambiente onde cada aplicação está 
acontecendo e coletar as constraints*)

(* --------------- Avaliador Big-Step de Programas L1 --------------- *)

let rec eval (environment : env) (e : expr) : value = 
  match e with
  (* valores *)
  | Num(n) -> Vnum(n)
  | Bool(b) -> Vbool(b)
  
  (* operações *)
  | Bop (op,e1,e2) -> 
    let n1 = eval environment e1 in
    let n2 = eval environment e2 in
    (match n1, op, n2 with
      | Vnum(n1), Sum, Vnum(n2) -> Vnum(n1+n2)
      | Vnum(n1), Diff, Vnum(n2) -> Vnum(n1-n2)
      | Vnum(n1), Mult, Vnum(n2) -> Vnum(n1*n2)
      | Vnum(n1), Div, Vnum(n2) -> 
          if n2 == 0 then Raise else Vnum(n1/n2)
      | Vnum(n1), Eq, Vnum(n2) -> Vbool(n1=n2)
      | Vnum(n1), Neq, Vnum(n2) -> Vbool(n1<>n2)
      | Vnum(n1), Less, Vnum(n2) -> Vbool(n1<n2)
      | Vnum(n1), Leq, Vnum(n2) -> Vbool(n1<=n2)
      | Vnum(n1), Greater, Vnum(n2) -> Vbool(n1>n2)
      | Vnum(n1), Geq, Vnum(n2) -> Vbool(n1>=n2)
      | Vbool(n1), And, Vbool(n2) -> Vbool(n1&&n2)
      | Vbool(n1), Or, Vbool(n2) -> Vbool(n1||n2)
      | _ -> failwith "unimplemented")
  
  (* if *)
  | If (e1,e2,e3) ->
    let b1 = eval environment e1 in
    (match b1 with
      | Vbool(true) -> eval environment e2
      | Vbool(false) -> eval environment e3
      | _ -> Raise )
 
  (* variáveis  *)
  | Var(variable) ->
    look_env_biding variable environment
 
  (* aplicação *)
  | App(e1,e2) -> 
  let v1 = eval environment e1 in 
  let v2 = eval environment e2 in
  (match v1,v2 with
    (*atualiza o ambiente para adicionar a amarração*)
    | Vclos(var, e, envi), v -> eval (add_env_biding var v envi) e
    | Vrclos(f, x, e, envi), v -> 
        eval (add_env_biding f (Vrclos(f, x, e, envi)) (add_env_biding x v envi)) e
    | _ -> failwith "unimplemented")
 
  (* fn *)

  (* let *)
  | Let(var,e1,e2) ->
    let v1 = eval environment e1 in
    (
      eval (add_env_biding var v1 environment) e2
    )

  (* let implícito*)

  (* letrec *)
  (* | Lrec (f, e1, e2) ->
    let v1 = eval environment e2 in
    (
      match v1 with
        |  VClosure(x,e,envi) -> eval (add_env_biding f (VRecClosure (f,x,e,environment)) envi) e2
        | _ -> failwith "unimplemented"
    )  *)

  (* let rec implícito *)

  (* nil *)
  | Nil -> Vnil

  (* cons*)
  | Cons(e1, e2) -> (
    let v1 = eval environment e1 in 
    let v2 = eval environment e2 in 
    (match v1, v2 with
      | (Raise,_) -> Raise
      | (_, Raise) -> Raise
      | _ -> Vcons(v1, v2)
    ) 
   )

  (* is empty *)
  | IsEmpty(e) ->
    let v = eval environment e in 
    (match v with 
      |Vnil -> (Vbool true)
      |Vcons (v1, v2) -> (Vbool false)
      |Raise -> Raise
      | _ -> Raise 
    )

  (* head *)
  | Hd(e) -> 
    let v = eval environment e in
    (match v with
      | Vcons(v1,v2) -> v1
      | Vnil -> Raise
      | Raise -> Raise
      | _ -> Raise
    )
  
  (* tail *)
  | Tl(e) ->  
    let v = eval environment e in 
    (match v with
      | Vcons(v1,v2) -> v2
      | Vnil -> Raise
      | Raise -> Raise
      | _ -> Raise
    )

  (* raise *)
  | Raise -> Raise

  (* try with *)
  | TryWith(e1, e2) ->
    let v = eval environment e1 in 
    (match v with
      |Raise -> eval environment e2
      | _ -> v
    )


  | _ -> failwith "unimplemented"

;;

(* ----------- Algoritmo de Inferência de Tipos para L1  ------------ *)
(* collectTyEqs *)


(* unify *)


(* applySubs *)


(* typeInfer *)



(* ------------------------- Testes -------------------------- *)

(* Segue um exemplo de como o programa L1 abaixo pode ser representado internamente *)

(* 
let rec fat: int -> int = (fn x: int => if (x == 0) then 1 else x * (fat (x - 1)))
   in fat (5)
   end
*)
(* 
Lrec("fat", TyInt, TyInt, "x", TyInt,
If(Bop(Eq, Var("x"), Num(0)),
   Num(1),
   Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1))))),
App(Var("fat"), Num(5))) 
*)

let environment = empty_env;;
let a = Num(5) ;;
let sumPass = Bop(Sum, Num(1), Num(1));;
(* eval environment sumPass;; *) 

(* #use "trab.ml";; *)