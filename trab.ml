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
          | Let of variable * tipo * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr
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
           | Vraise
           | VClosure of variable * variable * expr * env
           | VRecClosure of variable * variable * expr * env
          
and  
    env = (variable * value) list ;; 


(* GERÊNCIA DE AMBIENTE *)
(*-------------------------------------------------------------------------------*)

(* Função auxiliar para remover tuplas de uma lista*)
let remove_tuple var list =
  List.filter (fun (k, _) -> k <> var) list 
  ;;

(* ambiente vazio *)
let empty_env : env = [] ;;

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
    ;;

(* Função que retorna o valor de uma variável em um dado ambiente  *)
let rec look_env_biding var e : env = match e with
    [] -> raise Not_found
  | (v, value)::tl ->
      if (v == var) then value else look_env_biding var tl

(*-------------------------------------------------------------------------------*)


(* Temos que levar, sempre, em consideração o ambiente onde cada aplicação está acontecendo e coletar as constraints*)
(* --------------- Avaliador Big-Step de Programas L1 --------------- *)

let rec eval (environment : env) (e : expr) : value = 
  match e with
  (* valores *)
  | Num(n) -> Vnum(n)
  | Bool(b) -> Vbool(b)
  
  (*operações *)
  | Bop (e1,op,e2) -> 
    let n1 = eval environment e1 in
    let n2 = eval environment e2 in
    (match n1, op, n2 with
      | VNum(n1), Sum, VNum(n2) -> VNum(n1+n2)
      | VNum(n1), Diff, VNum(n2) -> VNum(n1-n2)
      | VNum(n1), Mult, VNum(n2) -> VNum(n1*n2)
      | VNum(n1), Div, VNum(n2) -> 
          (match n2 with
            | Num(0) -> Vraise
            | _ -> VNum(n1/n2))
      | VNum(n1), Eq, VNum(n2) -> Vbool(n1=n2)
      | VNum(n1), Neq, VNum(n2) -> Vbool(n1<>n2)
      | VNum(n1), Less, VNum(n2) -> Vbool(n1<n2)
      | VNum(n1), Leq, VNum(n2) -> Vbool(n1<=n2)
      | VNum(n1), Greater, VNum(n2) -> Vbool(n1>n2)
      | VNum(n1), Geg, VNum(n2) -> Vbool(n1>=n2)
      | Vbool(n1), And, Vbool(n2) -> Vbool(n1&&n2)
      | Vbool(n1), Or, Vbool(n2) -> Vbool(n1||n2)
      | _ -> failwith "unimplemented")
  (* ifs *)
  | If (e1,e2,e3) ->
    let b1 = eval environment e1 in
    (match b1 with
      | Vbool(true) -> eval environment e2
      | Vbool(false) -> eval environment e3
      | _ -> Vraise )
  (* variáveis  *)
  | Var(variable) ->
    look_env_biding variable environment
  (* aplicações *)
  | App (e1,e2) -> 
    let v1 = eval environment e1 in 
    let v2 = eval environment e2 in
    (match v1,v2 with
      (*atualiza o ambiente para adicionar a amarração*)
      VClosure(var, e, envi), v -> eval (add_env_biding var v envi) e)
  | _ -> failwith "unimplemented"

;;
  



(* ----------- Algoritmo de Inferência de Tipos para L1  ------------ *)
(* collectTyEqs *)


(* unify *)


(* applySubs *)


(* typeInfer *)





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
App(Var("fat"), Num(5))) *)

(* #use "trab.ml";; *)