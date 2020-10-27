(*
Lambda Calculus Intepreter Using Monads To Propagate Errors
Year 4 Capstone Project
Tram Hoang
*)


(*Monad signature*)
(* module type Monad = sig
 *   type 'a t
 *   val return : 'a -> 'a t
 *   val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
 * end *)

(*Lambda Calculus Monad for propagating errors *)
module LambdaError = struct
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

  (*Functions are values *)
  type value = 
    | IntV of int
    | FunV of var * exp

  type 'a t = 'a option * string
            
  let return x = (Some x, "")

  let (>>=) (m: value option * string) 
        (f: value -> value option * string) : value option * string = 
    match m with
    | (None, s) -> (None, s)
    | (Some e, s) -> 
       let res, new_s = f e in
       res, new_s ^ s


  (* Values are subset of expressions *)
  let exp_of_values (v:value): exp =
    match v with
    | IntV e -> Int e
    | FunV (v, e) -> Fun (v, e)

  (* Substitution in exp of t1 with t2 *)
  let rec subst (exp: exp) (t1: var) (t2: value) : exp = 
    match exp with
    | Int _ -> exp
    | Var x -> if x = t1 then exp_of_values t2 else Var x
    | Fun (x, e) -> if x = t1 then exp (* x is bound by Fun *)
                    else Fun (x, subst e t1 t2)
    | Binop (b, e1, e2) -> Binop (b, subst e1 t1 t2, subst e2 t1 t2) 
    | App (e1, e2) -> App (subst e1 t1 t2, subst e2 t1 t2)


  let rec eval (e: exp) : value option * string = 
    match e with
    | Int i -> return (IntV i)
    | Binop (b, e1, e2) ->
       eval e1 >>= fun lhs ->
       eval e2 >>= fun rhs ->
       begin
       match lhs, rhs with
       | (IntV i1, IntV i2) ->
          begin
            match b with
            | Add -> IntV (i1 + i2) |> return
            | Sub -> IntV (i1 - i2) |> return
            | Mul -> IntV (i1 * i2) |> return
            | Div -> IntV (i1 / i2) |> return
          end
       | _ -> (None, "e1 and/or e2 are not integers")
       end
    | Var _ -> (None, "eval free variable")
    | Fun (arg, body) -> return (FunV (arg, body))
    | App (e1, e2) ->
       begin
         eval e1 >>= fun lhs ->
         eval e2 >>= fun rhs ->
         (match lhs, rhs with
          | (FunV(x, body), v) -> eval (subst body x v)
          | _ -> None, "tried to apply to non-function"
         )
       end
end

  
