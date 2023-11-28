open List

type ident = string

(* Syntax *)
type exp = Var of ident | Num of int | Add of exp * exp | Sub of exp * exp
         | Bool of bool | And of exp * exp | Or of exp * exp
         | Eq of exp * exp

type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
           | If of exp * cmd * cmd | While of exp * cmd
           | Call of ident * ident * exp list | Return of exp

type value = IntVal of int | BoolVal of bool | StringVal of string

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

(* now can handle strings *)
let update_string (r : env) (x : ident) (v : value) : env =
  fun y -> if y = x then Some (Val v) else r y

let update_strings (r : env) (pairs : (ident * value) list) : env =
  List.fold_left (fun acc (x, v) -> update_string acc x v) r pairs
  


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
  | Return e -> 
    (match eval_exp e r with
    | Some v ->
      (match k with
        | (env0, x) :: krest -> Some (Skip, krest, update env0 x (Val v))
        | _ -> None)
    | None -> None)
  | Call (x, f, el) -> 
    (match eval_exps el r, lookup r f with
      | Some vl, Some (Fun (params, c)) ->
        (match add_args (empty_env) params vl with
          | env' -> Some (c, (r, x) :: k, env'))
      | _ -> None)

let rec run_config (con : config) : config =
  match step_cmd con with
  | Some con' -> run_config con'
  | None -> con

let run_prog (c : cmd) r =
  run_config (c, [], r)

(* Define a helper function to convert values to strings *)
let string_of_value = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | StringVal s -> s


let string_of_env env =
  let x_val, y_val = lookup env "x", lookup env "y" in
  let key_val, tuning_val, song_name_val, artist_val =
    lookup env "Key", lookup env "Tuning", lookup env "Song Name", lookup env "Artist"
  in

  let xy_str =
    match (x_val, y_val) with
    | (Some (Val (IntVal x)), Some (Val (IntVal y))) ->
      "x: " ^ string_of_int x ^ ", y: " ^ string_of_int y
    | _ -> ""
  in

  let key_tuning_str key tuning =
    match (key, tuning) with
    | (Some (Val (StringVal k)), Some (Val (StringVal t))) ->
      "\n Key: " ^ k ^ ", " ^ "Tuning: " ^ t ^ "\n "
    | _ -> ""
  in

  let song_artist_str song_name artist =
    match (song_name, artist) with
    | (Some (Val (StringVal sn)), Some (Val (StringVal ar))) ->
      "Song Name: " ^ sn ^ ", Artist: " ^ ar ^ "\n "
    | _ -> ""
  in

  let additional_strings =
    key_tuning_str key_val tuning_val
    ^ song_artist_str song_name_val artist_val
    (* Add more key-value pairs here if needed *)
  in

  xy_str ^ additional_strings

let env_song_metadata =
  update_strings empty_env [
    ("Song Name", StringVal "Hey Jude");
    ("Artist", StringVal "The Beatles");
    ("Key", StringVal "D");
    ("Tuning", StringVal "Standard")
  ]
let _ = print_endline ("song_metadata: " ^ string_of_env env_song_metadata)


(* BEGIN SONG DATA *)
type chord_bar = StringVal of string list
type music_bar = StringVal of string list

type song_data = chord_bar * music_bar

let print_song_data (data : song_data) : unit =
  let print_chord_bar (chord_bar : chord_bar) =
    match chord_bar with
    | StringVal chords -> String.concat " " chords
  in

  let print_music_bar (music_bar : music_bar) =
    match music_bar with
    | StringVal lyrics -> String.concat " " lyrics
  in

  let (chords, lyrics) = data in
  Printf.printf "Chords: %s\nLyrics: %s\n" (print_chord_bar chords) (print_music_bar lyrics)
;;

(* Populate the tuple *)
let example_data : song_data =
  (StringVal ["D"; "A"; "A7"; "D"], StringVal ["Hey Jude"; "don't make"; "it bad, take"; "a sad song, and make it better"])

(* Print the tuple to the console *)
let () = print_song_data example_data


(* EOF *)
