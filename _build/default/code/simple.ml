(*
Simple Lambda Calculus Interpreter using Ocaml
Year 4 Capstone Project

Author: Tram Hoang
*)

(* Variable *)
type var = string

(* Binary Operation *)
type binop = 
  | Add
  | Sub
  | Mul
  | Div

(* Terms *)
type term = 
  | Const of int
  | Var of var
  | Binop of binop * term * term
  | Lam of var * term
  | App of term * term

(* In prof notes *)
(* type exp = 
 *   | Var of var
 *   | Fun of var * exp
 *   | App of exp * exp *)

(* Find free variables *)
let rec find_var (t: term) : string list = 
  match t with
  | Var x -> [x]
  | Lam (x, y) -> find_var y |> List.filter (fun z -> x <> z)
  | App (t1, t2)
  | Binop (_, t1, t2) -> (find_var t1) @ (find_var t2)
  | _ -> []

let unique = ref 0

(* Variable substitution (t1 with t2) *)
let rec sub (exp: term) (t1: var) (t2: term): term = 
  match exp with
  | Const _ -> exp
  | Var x -> if x = t1 then t2 else exp
  | App (v1, v2) -> App (sub v1 t1 t2, sub v2 t1 t2)
  | Lam (v, term) -> if v = t1
                       then exp (* Don't change *)
                       else 
                         if not (List.mem v (find_var t2)) (*We need to convert var*)
                         then Lam (v, sub term t1 t2)
                         else 
                           let v' = v ^ (string_of_int !unique) in
                           incr unique;
                           let t' = sub term v (Var (v')) in
                           Lam (v', sub t' t1 t2)
  | Binop (b, v1, v2) -> Binop (b, sub v1 t1 t2, sub v2 t1 t2)

(*Beta Reduction*)
let rec reduce (exp: term) : term = 
  match exp with
  | App (e1, e2) ->
     begin
       match e1 with
       | Lam (x, e) -> sub e x e2
       | _ -> 
          let re1 = reduce e1 in
          if re1 != e1
          then App (re1, e2)
          else App (e1, reduce e2)
     end
  | Lam (x, e) -> Lam (x, reduce e)
  | _ -> exp

(*Evaluate to int*)
let rec eval (exp: term) : int =
  let rexp = reduce exp in
  match rexp with
  | Const x -> x
  | Binop (b, e1, e2) ->
     begin
     let re1 = reduce e1 in
     let re2 = reduce e2 in 
     match b with
       | Add -> (eval re1) + (eval re2)
       | Sub -> (eval re1) - (eval re2)
       | Mul -> (eval re1) * (eval re2)
       | Div -> (eval re1) / (eval re2)
     end
  | _ -> failwith "Can't be evaluated"
