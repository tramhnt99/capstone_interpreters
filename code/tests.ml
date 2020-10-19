open Simple

(* Tests for the simple interpreter *)

let func1 = Fun ("x", Var "x")
let func2 = Fun ("x", Binop (Add, Var "x", Int 1))
let func3 = Fun ("y", Fun ("x", Binop (Mul, Var ("x"), Var ("y"))))

let app1 = App ( Var "x", Var "y")
let app2 = App ( func1, Int 2)
let app3 = App ( func2, Int 3)
let app4 = App (App ( func3 , Int 2), Int 3)


let%test "Eval app1" =
  try 
    let _ = eval app1 in
    false
  with _ -> true
let%test "Eval app2" = eval app2 = IntV 2
let%test "Eval app3" = eval app3 = IntV 4
let%test "Eval app4" = eval app4 = IntV 6
