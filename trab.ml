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

type operator = Sum | Diff | Mult | Div | Eq | Neq | Less| Leq | Greater | Geq | And | Or | Not;;

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
and  
    (* aqui o ambiente é definido como sendo uma lista de variaveis amarradas a um valor *)
    env = (variable * value) list ;; 


(* ENV CONTROLL *)


(* ambiente vazio *)
let empty_env : env = [] ;;

let remove_env_binding var list = List.filter(fun (k,_) -> k <> var) list ;;
   
(* Temos que levar, sempre, em consideração o ambiente onde cada aplicação está acontecendo e coletar as constraints*)

(* --------------- Avaliador Big-Step de Programas L1 --------------- *)



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