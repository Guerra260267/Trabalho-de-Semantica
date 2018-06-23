type term =
TmTrue
| TmFalse
| TmIf of term * term * term
| TmZero 
| TmSucc of term
| TmPred of term
| TmIsZero of term

exception NoRuleApplies

let rec isnumericval t = match t with 
TmZero -> true
| TmSucc(t1) -> isnumericval  t1
| _ -> false


(* Regra do IF True small step *)
let rec step t = match t with
TmIf (TmTrue, t2, t3) -> 
t2
(* Regra do IF False small step *)
| TmIf (TmFalse, t2, t3) -> 
t3
(* Regra da avaliação do primeiro termo do if step *)
| TmIf (t1, t2, t3) -> 
let t1' = step t1 in 
TmIf(t1', t2, t3)

(* Regra da avaliação do passo somando mais 1 em t1 *)
| TmSucc (t1) -> 
let t1' = step t1 in 
TmSucc(t1')

(* Regra da avaliação do passo subtraindo 1 pra quando é zero, rola saturação*)
| TmPred(TmZero) ->
TmZero

(* Regra da avaliação do passo subtraindo 1 pra quando não é zero*)
|TmPred(TmSucc(nv1)) when (isnumericval nv1) ->
nv1

(* Regra da avaliação do passo subtraindo 1 pra quando o termo não está avaliado*)
| TmPred(t1) ->
let t1' = step t1 in 
TmPred(t1')
(* Regra da avaliação do passopara avaliar se é zero quando recebe zero*)
| TmIsZero (TmZero) -> TmTrue
(* Regra da avaliação do isZero quando não é zero*)
| TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse
(* Regra da avaliação para avaliar o termo*)
| TmIsZero (t1) -> let t1' = step t1 in TmIsZero (t1')
| _ ->
raise NoRuleApplies

let rec eval t =
	try let t' = step t in eval t'
with NoRuleApplies -> t

let t1 = TmIsZero (TmZero)
let t2 = TmZero
let t3 = TmSucc(TmZero)
let tif = TmIf( t1, t2, t3 )
let t4 = TmIsZero( TmSucc (TmZero))
let t5 = TmIsZero (TmFalse)
let tgabson = TmIf( TmIsZero (TmSucc TmZero), TmZero, TmSucc (TmSucc TmZero) )