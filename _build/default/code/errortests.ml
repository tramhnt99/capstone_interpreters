open Catcherrors

(* Testing Lambda Calculus Intepreter that Propagates Errors *)

let func1 = Fun ("x", Var "x")
let func2 = Fun ("x", Binop (Add, Var "x", Int 1))
let func3 = Fun ("y", Fun ("x", Binop (Mul, Var ("x"), Var ("y"))))

let app1 = App (Var "x", Var "y")
let app2 = App (func1, Int 2)
let app3 = App (Int 2, Int 3)
let app4 = App (func2, func1)
let app5 = App (App (func3, Int 2), func1)

let%test "CatchErrors Intepreter App1" = eval app1 = ErrorV "eval free variable"
let%test "CatchErrors Intepreter App2" = eval app2 = IntV 2
let%test "CatchErrors Intepreter App3" =
  eval app3 = ErrorV "tried to apply non-function"
let%test "CatchErrors Intepreter App4" = 
  eval app4 = ErrorV "e1 and/or e2 are not integers"
let%test "CatchErrors Intepreter App5" = 
  eval app5 = ErrorV "e1 and/or e2 are not integers"
