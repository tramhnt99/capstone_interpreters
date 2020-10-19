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

(* Substitution in exp of t1 with t2 *)

let rec subst (exp: exp) (t1: var) (t2: value) : exp = 
  match exp with
  | Int _ -> exp
  | Var x -> if x = t1 then exp_of_values t2 else Var x
  | Fun (x, e) -> if x = t1 then exp (* x is bound by Fun *)
                  else Fun (x, subst e t1 t2)
  | Binop (b, e1, e2) -> Binop (b, subst e1 t1 t2, subst e2 t1 t2) 
  | App (e1, e2) -> App (subst e1 t1 t2, subst e2 t1 t2)


(* Substitution based evaluator *)
let rec eval (e: exp) : value =
  match e with
  | Int i -> IntV i
  | Binop (b, e1, e2) ->
     begin
       match (eval e1, eval e2) with
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
  | Var _ -> failwith "eval free variable"
  | Fun (arg, body) -> FunV (arg, body)
  | App (e1, e2) ->
     begin
       match (eval e1, eval e2) with
       | (FunV (x, body), v) -> eval (subst body x v)
       | _ -> failwith "tried to apply non-function"
     end
