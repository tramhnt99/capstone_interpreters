(*
Lambda Calculus Intepreter that Notes Which Functions Have Run
Year 4 Capstone Project
Tram Hoang
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
  | Error of string

(* Functions are values *)
type value = 
  | IntV of int
  | FunV of var * exp
  | ErrorV of string

(* Context that saves which functions it has passed *)
type context = (string * string) list

(* String of conversion *)
let string_of_binop b: string = 
  match b with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"

let rec string_of_exp e: string = 
  match e with
  | Int i -> "Int " ^ (string_of_int i)
  | Var v -> "Var " ^ v
  | Fun (v, e') -> Printf.sprintf "Fun (%s, %s)" v (string_of_exp e') 
  | App (e1, e2) -> Printf.sprintf  "App (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Binop (b, e1, e2) -> 
    Printf.sprintf "Binop (%s, %s, %s)" (string_of_binop b) (string_of_exp e1) (string_of_exp e2)
  | Error s -> "Error " ^ s

let string_of_value v: string = 
  match v with 
  | IntV i -> "IntV " ^ (string_of_int i)
  | FunV (v', e) -> Printf.sprintf "FunV (%s, %s)" v' (string_of_exp e)
  | ErrorV s -> "ErrorV " ^ s

(* Values are subset of expressions *)
let exp_of_values (v:value) (c: context) : exp * context =
  let new_c = ("exp_of_values", string_of_value v) :: c in
  match v with
  | IntV e -> (Int e, new_c)
  | FunV (v, e) -> (Fun (v, e), new_c)
  | ErrorV s -> (Error s, new_c)

(* Substitution in exp of t1 with t2 *)

let rec subst (exp: exp) (t1: var) (t2: value) (c: context) : exp * context = 
  let new_c = 
    ("subst", (string_of_exp exp) ^ ", Var " ^ t1 ^ ", " ^ (string_of_value t2)) 
    :: c in
  match exp with
  | Int _ -> (exp, new_c)
  | Error _ -> (exp, new_c)
  | Var x -> if x = t1 then exp_of_values t2 new_c else (Var x, new_c)
  | Fun (x, e) -> if x = t1 then (exp, new_c) (* x is bound by Fun *)
                  else 
                    let (new_exp, new_c') = subst e t1 t2 new_c in
                    (Fun (x, new_exp), new_c')
  | Binop (b, e1, e2) -> 
     let (new_exp1, new_c1) = subst e1 t1 t2 new_c in
     let (new_exp2, new_c2) = subst e2 t1 t2 new_c1 in
     (Binop (b, new_exp1, new_exp2), new_c2)
  | App (e1, e2) -> 
     let (new_exp1, new_c1) = subst e1 t1 t2 new_c in
     let (new_exp2, new_c2) = subst e2 t1 t2 new_c1 in
     (App (new_exp1, new_exp2), new_c2)


(* Substitution based evaluator *)
let rec eval (e: exp) (c: context) : value * context =
  let new_c = ("eval", (string_of_exp e)) :: c in
  match e with
  | Int i -> (IntV i, List.rev new_c)
  | Error s -> (ErrorV s, List.rev new_c)
  | Binop (b, e1, e2) ->
     begin
       let (ee1, c1) = eval e1 new_c in
       let (ee2,c2) = eval e2 c1 in
       match (ee1, ee2) with
       | (IntV i1, IntV i2) ->
          begin
            match b with
            | Add -> (IntV (i1 + i2), List.rev c2)
            | Sub -> (IntV (i1 - i2), List.rev c2)
            | Mul -> (IntV (i1 * i2), List.rev c2)
            | Div -> (IntV (i1 / i2), List.rev c2)
          end
       | (ErrorV s1, _) -> (ErrorV s1, List.rev c2)
       | (_, ErrorV s2) -> (ErrorV s2, List.rev c2)
       | _ -> (ErrorV "e1 and/or e2 are not integers", List.rev c2)
     end
  | Var _ -> (ErrorV "eval free variable", List.rev new_c)
  | Fun (arg, body) -> (FunV (arg, body), List.rev new_c)
  | App (e1, e2) ->
     begin
       let (ee1, c1) = eval e1 new_c in
       let (ee2,c2) = eval e2 c1 in
       match (ee1, ee2) with
       | (FunV (x, body), v) -> 
          let (sub, c3) = subst body x v c2 in
          eval sub c3
       | (ErrorV s1, _) -> (ErrorV s1, List.rev c2)
       | (_, ErrorV s2) -> (ErrorV s2, List.rev c2)
       | _ -> (ErrorV "tried to apply non-function", List.rev c2)
     end
