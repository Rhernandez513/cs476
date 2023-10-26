open List

(* syntax *)
type ident = string

type typ = IntTy | ClassTy of ident
type exp = Num of int | Add of exp * exp | Mul of exp * exp | Var of ident
         | GetField of exp * ident

type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
         | New of ident * ident * exp list
         | Invoke of ident * exp * ident * exp list | Return of exp


type mdecl = { ret : typ; mname : ident; params : (typ * ident) list; body : cmd }

type cdecl = { cname : ident; super : ident; fields : (typ * ident) list; methods : mdecl list }

(* contexts *)
type ty_entry = Ty of typ
              | Class of cdecl

type context = ident -> ty_entry option
let empty_context = fun x -> None
let lookup (gamma : context) (x : ident) : ty_entry option = gamma x
let update (gamma : context) (x : ident) (e : ty_entry) = fun y -> if y = x then Some e else gamma y

let lookup_var (gamma : context) (x : ident) : typ option =
  match lookup gamma x with Some (Ty t) -> Some t | _ -> None
let lookup_class (gamma : context) (x : ident) : cdecl option =
  match lookup gamma x with Some (Class cd) -> Some cd | _ -> None
let update_var (gamma : context) (x : ident) (t : typ) : context = update gamma x (Ty t)
let update_class (gamma : context) (x : ident) (c : cdecl) : context = update gamma x (Class c)

(* field and method lookup *)
let rec fields (ct : context) (c : ident) : (typ * ident) list =
  if c = "Object" then [] else
    match lookup_class ct c with
    | Some cd -> fields ct cd.super @ cd.fields
    | _ -> []

let types_of_params (params : (typ * ident) list) : typ list =
  List.map fst params

let field_type_aux (l : (typ * ident) list) (f : ident) : typ option =
  match List.find_opt (fun (_, n) -> n = f) l with
  | Some (t, _) -> Some t
  | _ -> None

let field_type (ct : context) (c : ident) (f : ident) : typ option =
  field_type_aux (rev (fields ct c)) f

let rec methods (ct : context) (c : ident) : mdecl list =
  if c = "Object" then [] else
    match lookup_class ct c with
    | Some cd -> methods ct cd.super @ cd.methods
    | _ -> []

let lookup_method_aux (l : mdecl list) (m : ident) : mdecl option =
  find_opt (fun d -> d.mname = m) l

let lookup_method (ct : context) (c : ident) (m : ident) : mdecl option =
  lookup_method_aux (rev (methods ct c)) m

let rec supers (ct : context) (c : ident) : ident list =
  match c with
  | "Object" -> [c]  (* Return a list containing "Object" when c is "Object" *)
  | _ ->
    match lookup_class ct c with
    | Some cd -> c :: supers ct cd.super
    | _ -> []

let subtype (ct : context) (t1 : typ) (t2 : typ) : bool = (t1 = t2) ||
  match t1, t2 with
  | ClassTy c1, ClassTy c2 -> List.exists (fun x -> x = c2) (supers ct c1)
  | _, _ -> false
    
let rec type_of (gamma : context) (e : exp) : typ option =
  match e with
  | Num i -> Some IntTy
  | Add (e1, e2) | Mul (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _, _ -> None)
  | Var x -> lookup_var gamma x  
  | GetField(e, f) ->
      (match type_of gamma e with
       | Some (ClassTy c) ->
           (match field_type gamma c f with
            | Some t -> Some t
            | None -> None)
      | _ -> None)

let typecheck (gamma : context) (e : exp) (t : typ) : bool =
  match type_of gamma e with
  | Some t1 -> subtype gamma t1 t
  | None -> false

let rec typecheck_list (gamma : context) (es : exp list) (ts : typ list) : bool =
  List.for_all2 (typecheck gamma) es ts
  
let rec typecheck_cmd (gamma : context) (c : cmd) : bool =
  let typecheck_expr_list gamma es ts =
    let expr_types = List.map (fun e -> type_of gamma e) es in
    if List.length expr_types = List.length ts then
      List.for_all2 (fun et ft -> 
        match et with
        | Some et' -> subtype gamma et' ft
        | None -> false
      ) expr_types ts
    else
      false
  in
  match c with
  | Assign (i, e) ->
      (match lookup_var gamma i with
        | Some t -> typecheck gamma e t
        | None -> false)
  | Seq (c1, c2) -> typecheck_cmd gamma c1 && typecheck_cmd gamma c2
  | Skip -> true
  | New (x, c, es) ->
      (match lookup_var gamma x, lookup_class gamma c with
        | Some t0, Some cd ->
            let field_types = types_of_params (cd.fields) in
            typecheck gamma (Var x) (ClassTy c) &&
            typecheck_expr_list gamma es field_types
        | _ -> false)
  | Return e ->
      (match lookup_var gamma "__ret" with
        | Some t -> typecheck gamma e t
        | None -> false)

(* test cases *)  
let ct0 = update (update empty_context
    "Shape" (Class {cname = "Shape"; super = "Object"; fields = [(IntTy, "id")];
          methods = [{ret = IntTy; mname = "area"; params = []; body = Return (Num 0)}]}))
    "Square" (Class {cname = "Square"; super = "Shape"; fields = [(IntTy, "side")];
               methods = [{ret = IntTy; mname = "area"; params = [];
                    body = Seq (Assign ("x", GetField (Var "this", "side")),
                       Return (Add (Var "x", Var "x")))}]})


let gamma0 : context = update_var (update_var (update_var ct0 "s" (ClassTy "Square")) "x" IntTy) "y" IntTy

let gamma1 : context = update_var (update_var (update_var ct0 "s" (ClassTy "Shape")) "x" IntTy) "y" IntTy

let gamma2 : context = update_var (update_var (update_var (update_var ct0 "s2" (ClassTy "Square")) "s1" (ClassTy "Shape")) "x" IntTy) "y" IntTy


let exp2 : exp = GetField (Var "s", "id")
  
let cmd3 : cmd =
  Seq (New ("s", "Square", [Num 0; Num 2]),
       (* s = new Square(0, 2); *)
       Assign ("y", Add (GetField (Var "s", "side"), Num 1)))
       (* y = s.side + 1; *)
  
(* for the grad student problem *)
let cmd4 : cmd =
  Seq (New ("s", "Shape", [Num 2]),
       (* s = new Shape(2); *)
       Invoke ("x", Var "s", "area", []))
       (* x = s.area(); *)
  
let cmd5 : cmd =
  Seq (New ("s", "Square", [Num 0; Num 2]),
       (* s = new Square(0, 2); *)
  Seq (Assign ("y", Add (GetField (Var "s", "side"), Num 1)),
       (* y = s.side + 1; *)
       Invoke ("x", Var "s", "area", [])))
       (* x = s.area(); *)

(* run the tests *)
let supers_test1 = subtype ct0 (ClassTy "Square") (ClassTy "Object") (* should return true *)

let supers_test2 = subtype ct0 (ClassTy "Object") (ClassTy "Square") (* should return false *)

let field_test1 = (type_of gamma0 exp2 = Some IntTy) (* should return true *)
  
let new_test1 = typecheck_cmd gamma0 cmd3 (* should return true *)
  
let invoke_test1 = typecheck_cmd gamma1 cmd4 (* should return true *)
  
let invoke_test2 = typecheck_cmd gamma0 cmd5 (* should return true *)
