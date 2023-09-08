(* Ref: ChatGPT3.5 Free Research Preview https://chat.openai.com/?model=text-davinci-002-render-sha *)
(* Ref: OCaml Standard Libary: https://v2.ocaml.org/api/index.html *)
(* Ref: OCaml Standard Libary: https://ocaml.org/docs/if-statements-and-loops *)
(* Ref: StackOverFlow: https://stackoverflow.com/questions/13590307/whats-the-difference-between-equal-and-identical-in-ocaml *)

type term = Until of term * term 
  | Eq of string * int 
  | Up of string * int (* add problem 2 here *)
  | For of string * int * term

let term1 : term = Until (Up ("x", 2), Until (Up ("y", 4), Eq ("y", 9)));;

(* answer to problem 1:  *)

(* "term" is defined as a variant type, meaning it can have multiple constructors.
The three constructors defined are:
  - "Until of term * term" that takes two "term" values and represents an "until"
  operation which may be a loop checking a value while not some value.
  - "Eq of string * int" that takes a string and an int as arguments and 
  represents an equality comparison.
  - "Up of string * int" that takes a string and an int as arguments and 
  represents an "up" operation, which is undefined but which seems to b
  e an exponentiation operation, a string concatenantion operation, 
  or potentially a value comparison.

Starting from the inside out, we have an Up and a Eq operation nestest as
parameters for an Until operation.  The result of this is used as the second
parameter to another Until operation, with the remaining Up operation as the 
second parameter.

Assuming Up performs "+=" style addition... In english:  Keep adding 4 to "y"
to "y" equals 9.  Using this result Keep adding 2 to "x" until the nested 
operation completes and returns a result. *)


(* problem 3 *)
(* for i from 2 do i ∧ 1 until i = 10 *)
let my_term : term =
  For ("i", 2, Until (Up ("i", 1), Eq ("i", 10)))


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
  | If (e, e1, e2) -> typecheck e BoolTy && typecheck e1 t && typecheck e2 t
  | Eq (e1, e2) -> (match t with
                    | IntTy -> e1 = e2
                    | BoolTy -> e1 = e2)
;;

(* Test cases *)
let () =
  let result1 = 
    typecheck (If (Bool true, Num 1, Num 3)) IntTy in
  print_endline ("Result 1: " ^ string_of_bool result1);
  assert (result1 = true); (* should return true *)

  let result2 = 
    typecheck (If (Bool true, Bool false, Num 3)) BoolTy in
  print_endline ("Result 2: " ^ string_of_bool result2);
  assert (result2 = false); (* should return false *)

  (* problems 4 *)
  let result3 = 
    typecheck (If (Num 3, Num 1, Num 3)) IntTy in
  print_endline ("Result 3: " ^ string_of_bool result3);
  assert (result3 = false); (* should return false *)

  (* problems 5 *)
  let result4 = 
    typecheck (Eq (Num 1, Num 1)) BoolTy in
  print_endline ("Result 4: " ^ string_of_bool result4);
  assert (result4 = true); (* should return true *)

  let result5 = 
    typecheck (Eq (Bool true, Bool false)) BoolTy in
  print_endline ("Result 5: " ^ string_of_bool result5);
  assert (result5 = false); (* should return false *)

  let result6 = 
    typecheck (Eq (Num 1, Bool false)) BoolTy in
  print_endline ("Result 6: " ^ string_of_bool result6);
  assert (result6 = false) (* should return false *)
