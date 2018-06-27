type variable = string

type operator = Sum | Diff | Mult | Div | Eq | Neq | Less| Leq | Greater | Geq | And | Or | Not

type tipo  = TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo | TyX of string

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

type value = Vnum of int 
           | Vbool of bool 
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
           | Vnil
           | Vcons of value * value
           | Vraise
and  
     env = (variable * value) list


(* --------------- Avaliador Big-Step de Programas L1 --------------- *)



(* ----------- Algoritmo de Inferência de Tipos para L1  ------------ *)
(* collectTyEqs *)


(* unify *)


(* applySubs *)


(* typeInfer *)











(* Segue um exemplo de como o programa L1 abaixo pode ser representado internamente *)
(* let rec fat: int -> int = (fn x: int => if (x == 0) then 1 else x * (fat (x - 1)))
   in fat (5)
   end
*)

Lrec("fat", TyInt, TyInt, "x", TyInt,
If(Bop(Eq, Var("x"), Num(0)),
   Num(1),
   Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1))))),
App(Var("fat"), Num(5)))