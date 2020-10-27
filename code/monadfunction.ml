(*
Lambda Calculus Intepreter Using Monads To Propagate Errors
Year 4 Capstone Project
Tram Hoang
*)


  (*TODO: Using the monad, log every function is passes *)

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

  (* First is the expression/value, then string for an error, then string for functions*)
  type 'a t = 'a option * string * string
            
  let return x = (Some x, "")

  let (>>=) (m: 'a option * string) 
        (f: 'a -> 'b option * string) : 'b option * string = 
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
  let rec subst (t1: var) (t2: value) (exp: exp) : exp option * string = 
    match exp with
    | Int _ -> return exp
    | Var x -> if x = t1 then return (exp_of_values t2) else return (Var x)
    | Fun (x, e) -> if x = t1 then return exp (* x is bound by Fun *)
                    else subst t1 t2 e >>= fun e' ->
                         return (Fun (x, e'))
    | Binop (b, e1, e2) -> 
       subst t1 t2 e1 >>= fun lhs ->
       subst t1 t2 e2 >>= fun rhs ->
       return (Binop (b, lhs, rhs))
    | App (e1, e2) -> 
       subst t1 t2 e1 >>= fun lhs ->
       subst t1 t2 e2 >>= fun rhs ->
       return (App (lhs, rhs))

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
          | (FunV(x, body), v) -> subst x v body >>= fun e -> eval e
          | _ -> None, "tried to apply to non-function"
         )
       end
end

  
