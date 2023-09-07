type term = Until of term * term | Eq of string * int | Up of string * int (* add problem 2 here *)

let term1 : term = Until (Up ("x", 2), Until (Up ("y", 4), Eq ("y", 9)));;

(* answer to problem 1:  *)

(* problem 3 *)
(* let my_term : term = *)

type exp = Num of int
  | Add of exp * exp 
  | Bool of bool
  | And of exp * exp 
  | Not of exp
  | Eq of exp * exp
  | If of exp * exp * exp

type typ = IntTy | BoolTy

let rec typecheck (e : exp) (t : typ) : bool =
  match e with
  | Num _ -> t = IntTy
  | Bool _ -> t = BoolTy
  | Add (e1, e2) -> typecheck e1 IntTy && typecheck e2 IntTy && t = IntTy
  | And (e1, e2) -> typecheck e1 BoolTy && typecheck e2 BoolTy && t = BoolTy
  | Not e -> typecheck e BoolTy && t = BoolTy
  (* problems 4 and 5 *)
;;

typecheck (If (Bool true, Num 1, Num 3)) IntTy;; (* should return true *)
typecheck (If (Bool true, Bool false, Num 3)) BoolTy;; (* should return false *)