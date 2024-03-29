(* More Test Cases *)

let ctc = update (update empty_context
    "Point" (Class {cname = "Point"; super = "Object"; fields = [(IntTy, "x"); (IntTy, "y")];
          methods = [{ret = IntTy; mname = "getx"; params = []; body = Return (GetField (Var "this", "x"))}]}))
    "Circle" (Class {cname = "Circle"; super = "Shape"; fields = [(ClassTy "Point", "center")];
               methods = []})

let gammac = update_var ctc "circle" (ClassTy "Circle")

let testc : exp = (GetField (GetField (Var "circle", "center"), "x"))
let resc = (type_of gammac testc = Some IntTy)
(* bool = true *)

let gamma3 : context = update_var gamma2 "s3" (ClassTy "Object")

let test6 : exp = (GetField (Var "s1", "side"))
let res6 = (type_of gamma2 test6 = Some IntTy)
(* bool = false *)

let test7 : exp = (GetField (Var "s2", "side"))
let res7 = (type_of gamma2 test7 = Some IntTy)
(* bool = true *)

let test8 : exp = (GetField (Var "s2", "id"))
let res8 = (type_of gamma0 test8 = Some IntTy)
(* bool = false *)

let test9 : exp = (GetField (Var "s2", "id"))
let res9 = (type_of gamma2 test9 = Some (ClassTy "s2"))
(* bool = false *)

let test10 : cmd = Assign ("s1", Var "x")
let res10 = typecheck_cmd gamma2 test10
(* bool = false *)

let test11 : cmd = Assign ("x", Var "s")
let res11 = typecheck_cmd gamma0 test11
(* bool = false *)

let test12 : cmd = Assign ("x", Var "y")
let res12 = typecheck_cmd gamma3 test12
(* bool = true *)

let test13 : cmd = Assign ("s2", Var "s1")
let res13 = typecheck_cmd gamma3 test13
(* bool = false *)

let test14 : cmd = Assign ("s3", Var "s1")
let res14 = typecheck_cmd gamma3 test14
(* bool = true *)

let test15 : cmd =
 Seq (New ("s", "Square", [Num 0; Num 2]),
       (* s = new Square(0, 2); *)
       Invoke ("x", Var "s", "area", []))
       (* s.side = s.area(); *)
let res15 = typecheck_cmd gamma1 test15
(* bool = true *)

let test16 : cmd =
 Seq (New ("s", "Square", [Num 0; Num 2]),
       (* s = new Square(0, 2); *)
       Invoke ("s", Var "s", "area", []))
       (* s = s.area(); *)
let res16 = typecheck_cmd gamma1 test16
(* bool = false *)
