(*
Simple Lambda Calculus Interpreter using Ocaml
Year 4 Capstone Project
*)

type var = string

(* Binary Operation *)
type binop = 
  | Add
  | Sub
  | Mul 
  | Div

(* Expressions *)

type exp = 
  | Int of int
  | Var of var
  | Fun of var * exp
  | App of exp * exp
  | Binop of binop * exp * exp

(* Functions are values *)
type value = 
  | IntV of int
  | FunV of var * exp

(* Values are subset of expressions *)
let exp_of_values (v:value): exp =
  match v with
  | IntV e -> Int e
  | FunV (v, e) -> Fun (v, e)

(* Find free variables *)
let rec find_var (exp: exp) : var list =
  match exp with
  | Int _ -> []
  | Var x -> [x]
  | Fun (x, e) -> find_var e |> List.filter (fun z -> x <> z) (* x is bound and is not free *)
  | App (e1, e2) -> (find_var e1) @ (find_var e2)
  | Binop (_, e1, e2) -> (find_var e1) @ (find_var e2)

let unique = ref 0

(* Substitution *)

let rec subst (exp: exp) (t1: var) (t2: exp) : exp = 
  match exp with
  | Int _ -> exp
  | Var x -> if x = t1 then t2 else Var x
  | Fun (x, e) -> if x = t1 then exp (* x is bound by Fun *)
                  else 
                    if not (List.mem x (find_var t2))
                    then subst e t1 t2
                    else 
                      let x' = x ^ (string_of_int !unique) in
                      incr unique;
                      let t' = subst e x (Var x') in
                      Fun (x', subst t' t1 t2)
  | Binop (b, e1, e2) -> Binop (b, subst e1 t1 t2, subst e2 t1 t2) 
  | App (e1, e2) -> App (subst e1 t1 t2, subst e2 t1 t2)



(* Boolean examples *)

let true_exp = Fun ("tru", Fun ("fls", Var "tru"))
let false_exp = Fun ("tru", Fun ("fls", Var "fls"))
let if_then_else b e1 e2 = App(App(b, e1), e2)


(* Reducing *)
let rec reduce (e : exp) : exp =
  match e with
  | Fun (arg, body) -> Fun (arg, reduce body)
  | App (e1, e2) ->
     (match (reduce e1, reduce e2) with
      | (Var x, n2) -> App (Var x, n2)
      | (Fun (x, body), v) -> reduce (subst v x body)
      | _ -> App(e1, e2))
  | Binop (b, e1, e2) -> Binop (b, reduce e1, reduce e2)
  | _ -> failwith "can't be reduced"



(* Closure based intepreter for evaluation*)
module Eval = struct
  type value =
    | IntV of int
    | Closure of env * var * exp
  and env = (var * value) list

  let lookup x (env:env) = List.assoc x env

  let rec eval (env: env) (e: exp) : value =
    match e with
    | Int i -> IntV i
    | Binop (b, e1, e2) ->
       begin
         match (eval env e1, eval env e2) with
         | (IntV i1, IntV i2) ->
            begin
              match b with
              | Add -> IntV (i1 + i2)
              | Sub -> IntV (i1 - i2)
              | Mul -> IntV (i1 * i2)
              | Div -> IntV (i1 / i2)
            end
         | _ -> failwith "e1 and/or e2 are not integers"
       end
    | Var x -> lookup x env
    | Fun (arg, body) -> Closure (env, arg, body) (*captured env at the time func is created *)
    | App (e1, e2) ->
       begin
         match (eval env e1, eval env e2) with
         | (Closure (cenv, x, body), v) -> eval ((x,v) :: cenv) body
         | _ -> failwith "tried to apply non-function"
       end
end
