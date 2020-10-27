open Monadfunction
open LambdaErrorFunction

(*Testing Monad Lambda Caluculus for Propagating Errors *)
let func1 = Fun ("x", Var "x")
let func2 = Fun ("x", Binop (Add, Var "x", Int 1))
let func3 = Fun ("y", Fun ("x", Binop (Mul, Var ("x"), Var ("y"))))

let app1 = App (Var "x", Var "y")
let app2 = App (func1, Int 2)
let app3 = App (Int 2, Int 3)
let app4 = App (func2, func1)
let app5 = App (App (func3, Int 2), func1)
let app6 = App ( func2, Int 3)
let app7 = App (App ( func3 , Int 2), Int 3)

let%test "Monadfunction Intepreter App1" = eval app1 = (None, "eval free variable", " eval  eval ")
let%test "Monadfunction Intepreter App2" = eval app2 = (Some (IntV 2), "", " eval  subst  eval  eval  eval  eval  eval ")
let%test "Monadfunction Intepreter App3" =
  eval app3 = (None, "tried to apply to non-function", " eval  eval  eval  eval  eval ")
let%test "Monadfunction Intepreter App4" = 
  eval app4 = (None, "e1 and/or e2 are not integers",
 " eval  eval  eval  eval  eval  subst  subst  subst  subst  subst  eval  eval  eval  eval  eval ")
let%test "Monadfunction Intepreter App5" = 
  eval app5 = (None, "e1 and/or e2 are not integers",
 " eval  eval  eval  eval  eval  subst  subst  subst  subst  subst  eval  eval  eval  eval  subst  subst  subst  subst  subst  subst  subst  eval  eval  eval  eval  eval  eval ")
let%test "Monadfunction Intepreter App6" = eval app6 = (Some (IntV 4), "",
 " eval  eval  eval  eval  eval  subst  subst  subst  subst  subst  eval  eval  eval  eval  eval ")
let%test "Monadfunction Intepreter App7" = eval app7 = (Some (IntV 6), "",
 " eval  eval  eval  eval  eval  subst  subst  subst  subst  subst  eval  eval  eval  eval  subst  subst  subst  subst  subst  subst  subst  eval  eval  eval  eval  eval  eval ")
