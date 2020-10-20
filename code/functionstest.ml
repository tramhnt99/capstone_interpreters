open Functions

(* Test for interpreter that keeps track of function it passed *)

let func1 = Fun ("x", Var "x")
let func2 = Fun ("x", Binop (Add, Var "x", Int 1))
let func3 = Fun ("y", Fun ("x", Binop (Mul, Var ("x"), Var ("y"))))

let app1 = App ( Var "x", Var "y")
let app2 = App ( func1, Int 2)
let app3 = App ( func2, Int 3)
let app4 = App (App ( func3 , Int 2), Int 3)
let app5 = App (Int 2, Int 3)
let app6 = App (func2, func1)
let app7 = App (App (func3, Int 2), func1)

let%test "Functions Interpreter App1" = 
  eval app1 [] =
    (ErrorV "eval free variable",
     [("eval", "Var y"); ("eval", "App (Var x, Var y)"); ("eval", "Var x")])       
let%test "Functions Interpreter App2" =
  eval app2 [] =
    (IntV 2,
     [("eval", "Int 2"); ("eval", "App (Fun (x, Var x), Int 2)");
      ("eval", "Fun (x, Var x)"); ("subst", "Var x, Var x, IntV 2");
      ("exp_of_values", "IntV 2"); ("eval", "Int 2")])
let%test "Functions Interpreter App3" = 
  eval app3 [] =
    (IntV 4,
     [("eval", "Int 1"); ("eval", "Int 3");
      ("eval", "App (Fun (x, Binop (Add, Var x, Int 1)), Int 3)");
      ("eval", "Fun (x, Binop (Add, Var x, Int 1))");
      ("subst", "Binop (Add, Var x, Int 1), Var x, IntV 3");
      ("subst", "Var x, Var x, IntV 3"); ("exp_of_values", "IntV 3");
      ("subst", "Int 1, Var x, IntV 3"); ("eval", "Binop (Add, Int 3, Int 1)");
      ("eval", "Int 3")])
let%test "Functions Interpreter App4" = 
  eval app4 [] =
    (IntV 6,
     [("eval", "Int 2"); ("eval", "Int 3"); ("eval", "Int 2");
      ("eval",
       "App (App (Fun (y, Fun (x, Binop (Mul, Var x, Var y))), Int 2), Int 3)");
      ("eval", "App (Fun (y, Fun (x, Binop (Mul, Var x, Var y))), Int 2)");
      ("eval", "Fun (y, Fun (x, Binop (Mul, Var x, Var y)))");
      ("subst", "Fun (x, Binop (Mul, Var x, Var y)), Var y, IntV 2");
      ("subst", "Binop (Mul, Var x, Var y), Var y, IntV 2");
      ("subst", "Var x, Var y, IntV 2"); ("subst", "Var y, Var y, IntV 2");
      ("exp_of_values", "IntV 2"); ("eval", "Fun (x, Binop (Mul, Var x, Int 2))");
      ("subst", "Binop (Mul, Var x, Int 2), Var x, IntV 3");
      ("subst", "Var x, Var x, IntV 3"); ("exp_of_values", "IntV 3");
      ("subst", "Int 2, Var x, IntV 3"); ("eval", "Binop (Mul, Int 3, Int 2)");
      ("eval", "Int 3")])
let%test "Functions Interpreter App5" = 
  eval app5 [] =
    (ErrorV "tried to apply non-function",
     [("eval", "Int 3"); ("eval", "App (Int 2, Int 3)"); ("eval", "Int 2")])
let%test "Functions Interpreter App6" = 
  eval app6 [] =
    (ErrorV "e1 and/or e2 are not integers",
     [("eval", "Int 1"); ("eval", "Fun (x, Var x)");
      ("eval", "App (Fun (x, Binop (Add, Var x, Int 1)), Fun (x, Var x))");
      ("eval", "Fun (x, Binop (Add, Var x, Int 1))");
      ("subst", "Binop (Add, Var x, Int 1), Var x, FunV (x, Var x)");
      ("subst", "Var x, Var x, FunV (x, Var x)");
      ("exp_of_values", "FunV (x, Var x)");
      ("subst", "Int 1, Var x, FunV (x, Var x)");
      ("eval", "Binop (Add, Fun (x, Var x), Int 1)"); ("eval", "Fun (x, Var x)")])
let%test "Functions Interpreter App7" = 
  eval app7 [] =
    (ErrorV "e1 and/or e2 are not integers",
     [("eval", "Int 2"); ("eval", "Fun (x, Var x)"); ("eval", "Int 2");
      ("eval",
       "App (App (Fun (y, Fun (x, Binop (Mul, Var x, Var y))), Int 2), Fun (x, Var x))");
      ("eval", "App (Fun (y, Fun (x, Binop (Mul, Var x, Var y))), Int 2)");
      ("eval", "Fun (y, Fun (x, Binop (Mul, Var x, Var y)))");
      ("subst", "Fun (x, Binop (Mul, Var x, Var y)), Var y, IntV 2");
      ("subst", "Binop (Mul, Var x, Var y), Var y, IntV 2");
      ("subst", "Var x, Var y, IntV 2"); ("subst", "Var y, Var y, IntV 2");
      ("exp_of_values", "IntV 2"); ("eval", "Fun (x, Binop (Mul, Var x, Int 2))");
      ("subst", "Binop (Mul, Var x, Int 2), Var x, FunV (x, Var x)");
      ("subst", "Var x, Var x, FunV (x, Var x)");
      ("exp_of_values", "FunV (x, Var x)");
      ("subst", "Int 2, Var x, FunV (x, Var x)");
      ("eval", "Binop (Mul, Fun (x, Var x), Int 2)"); ("eval", "Fun (x, Var x)")])
