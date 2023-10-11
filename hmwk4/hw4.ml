open List

type ident = string

(* Syntax *)
type exp = Var of ident | Num of int | Add of exp * exp | Sub of exp * exp
         | Bool of bool | And of exp * exp | Or of exp * exp
         | Eq of exp * exp

type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
           | If of exp * cmd * cmd | While of exp * cmd
           | Call of ident * ident * exp list | Return of exp

type value = IntVal of int | BoolVal of bool

type entry = Val of value
           | Fun of ident list (* list of params *) * cmd (* body *)

(* lookup s x would return:
   Some (Val (IntVal 5)) if x is a var with value 5
   Some (Fun ([y], body))  if x is a function x(y) { body }
   None if x is undefined *)

(* environment implementation *)
type env = ident -> entry option
let empty_env = fun x -> None
let lookup (r : env) (x : ident) : entry option = r x
let update (r : env) (x : ident) (e : entry) : env = fun y -> if y = x then Some e else r y

let rec add_args (r : env) (li : ident list) (lv : value list) : env =
  match li, lv with
  | i :: irest, v :: vrest -> add_args (update r i (Val v)) irest vrest
  | _, _ -> r
(* end environment implementation *)

(* Semantics *)
let rec eval_exp (e : exp) (r : env) : value option =
  match e with
  | Var x -> (match lookup r x with Some (Val v) -> Some v | _ -> None)
  | Num i -> Some (IntVal i)
  | Add (e1, e2) -> (match eval_exp e1 r, eval_exp e2 r with
                     | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 + i2))
                     | _, _ -> None)
  | Sub (e1, e2) -> (match eval_exp e1 r, eval_exp e2 r with
                     | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 - i2))
                     | _, _ -> None)
  | Bool b -> Some (BoolVal b)
  | And (e1, e2) -> (match eval_exp e1 r, eval_exp e2 r with
                     | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 && b2))
                     | _, _ -> None)
  | Or (e1, e2) -> (match eval_exp e1 r, eval_exp e2 r with
                     | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 || b2))
                     | _, _ -> None)
  | Eq (e1, e2) -> (match eval_exp e1 r, eval_exp e2 r with
                     | Some v1, Some v2 -> Some (BoolVal (v1 = v2))
                     | _, _ -> None)

let rec eval_exps (es : exp list) (r : env) : value list option =
  match es with
  | [] -> Some []
  | e :: rest -> (match eval_exp e r, eval_exps rest r with
                  | Some v, Some vs -> Some (v :: vs)
                  | _, _ -> None)

type stack = (env * ident) list

type config = cmd * stack * env

let rec step_cmd (con : config) : config option =
  let (c, k, r) = con in
  match c with
  | Assign (x, e) -> (match eval_exp e r with
                      | Some v -> Some (Skip, k, update r x (Val v))
                      | None -> None)
  | Seq (Skip, c2) -> Some (c2, k, r)
  | Seq (c1, c2) -> (match step_cmd (c1, k, r) with
                     | Some (c1', k', r') -> Some (Seq (c1', c2), k', r')
                     | None -> None)
  | Skip -> None
  | If (e, c1, c2) -> (match eval_exp e r with
                        | Some (BoolVal true) -> Some (c1, k, r)
                        | Some (BoolVal false) -> Some (c2, k, r)
                        | _ -> None)
  | While (e, c) -> Some (If (e, Seq (c, While (e, c)), Skip), k, r)
  (* | Return ... (problem 2) *)
  (* | Call ... (problem 3) *)
  | _ -> None

let rec run_config (con : config) : config =
  match step_cmd con with
  | Some con' -> run_config con'
  | None -> con

let run_prog (c : cmd) r =
  run_config (c, [], r)

(* problem 1 *)
(* let my_prog : cmd = <fill in here> *)

(* test cases *)
(* Define a helper function to convert values to strings *)
let string_of_value = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b

  (* Define a helper function to convert environment values to strings *)
let string_of_env env =
  match lookup env "x", lookup env "y" with
  | Some (Val (IntVal x)), Some (Val (IntVal y)) ->
    "x: " ^ string_of_int x ^ ", y: " ^ string_of_int y
  | _, _ -> "Not found"

let env0 = update empty_env "add" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))

let env1 = update (update env0 "x" (Val (IntVal 1)))
  "y" (Val (IntVal 2))

(* Print env0 and env1 *)
let _ = print_endline ("env0: " ^ string_of_env env0)
let _ = print_endline ("env1: " ^ string_of_env env1)
  
let ret_test1 = run_config (Return (Add (Var "x", Var "y")), [(env0, "x")], env1);;
let (res_c, res_k, res_r) = ret_test1;;
lookup res_r "x";; (* should return Some (Val (IntVal 3)) *)
lookup res_r "y";; (* should return None *)

let _ = print_endline (match lookup res_r "x" with
  | Some (Val v) -> "x: " ^ string_of_value v
  | _ -> "x: Not found")
let _ = print_endline (match lookup res_r "y" with
  | Some (Val v) -> "y: " ^ string_of_value v
  | _ -> "y: Not found")

let call_test1 = run_prog (Call ("x", "add", [Num 1; Num 2])) env0;;
let (res_c, res_k, res_r) = call_test1;;
lookup res_r "x";; (* should return Some (Val (IntVal 3)) *)
lookup res_r "y";; (* should return None *)

let _ = print_endline (match lookup res_r "x" with
  | Some (Val v) -> "x: " ^ string_of_value v
  | _ -> "x: Not found")
let _ = print_endline (match lookup res_r "y" with
  | Some (Val v) -> "y: " ^ string_of_value v
  | _ -> "y: Not found")
