open Simple

(* Tests for the simple interpreter *)

let lambda1 = Lam ("x", Var "x")
let lambda2 = Lam ("x", Binop(Add, Var "x", Const 1))
let lambda3 = Lam ( "y", Lam ("x", Binop(Mul, Var "x", Var "y")))

let app1 = App ( Var "x", Var "y")
let app2 = App ( lambda1, Var "y")
let app3 = App ( lambda2, Const 3)
let app4 = App (App ( lambda3, Const 2), Const 3)
let app5 = App ( App (Lam ("x", Var "x"), Lam ("y", Var "y")), Var "z")


let%test "Substitute simple1" = sub lambda1 "x" (Var ("y")) = lambda1
let%test "Substitute simple2" = sub lambda2 "x" (Var ("y")) = lambda2
let%test "Substitute simple3" = sub lambda2 "x" (Binop (Add, Var "x", 3)) = 
                                  Lam ("x0", Binop (Add, Var "x0", Const 1))
let%test "Substitute simple4" = sub app1 "y" app2 =
                                  App (Var "x", App (Lam ("x", Var "x"), Var "y"))

let%test "Reduce simple1" = reduce app1 = app1
let%test "Reduce simple2" = reduce app2 = Var "y"
let%test "Reduce simple3" = reduce app3 = Binop(Add, Const 3, Const 1)


let%test "Evaluate simple1" = try eval app 1 with _ -> true
let%test "Evaluate simple2" = eval app3 = 4
let%test "Evaluate simple3" = eval app4 = 6

 (* Just let substitution in lambda happen *)
