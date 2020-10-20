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

(* Context that stores whether there has been an error and the error message *)
type context = 
  | Error of bool * string

