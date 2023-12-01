open List

type ident = string

type chord_bar = StringVal of string list | StringListVal of string list
type music_bar = StringVal of string list | StringListVal of string list

type music_bar_tuple = chord_bar * music_bar

(* Syntax *)
(* add constructor for string literals*)
type exp = Var of ident | Num of int | Add of exp * exp | Sub of exp * exp
         | Bool of bool | And of exp * exp | Or of exp * exp
         | Eq of exp * exp
         | StringLit of string
         | StringListVal of string list
         | AppendString of ident * exp

type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
           | If of exp * cmd * cmd | While of exp * cmd
           | Call of ident * ident * exp list | Return of exp
           (* Add constructor for appending string to bar*)
           | AppendString of ident * exp

(* add constructor for string lists*)
type value = IntVal of int | BoolVal of bool | StringVal of string | StringListVal of string list

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
  (* extend eval_exp func for string literals*)
  | StringLit s -> Some (StringVal s)
  | StringListVal lst -> Some (StringListVal lst)  (* New case for StringListVal *)
  | AppendString (bar, str_exp) -> (match lookup r bar, eval_exp str_exp r with
                                    | Some (Val (StringListVal lst)), Some (StringVal str) ->
                                      Some (StringListVal (lst @ [str]))
                                    | _, _ -> None)
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
  (*Extend eval_exps for string literals in a list*)
  | StringLit s :: rest -> (match eval_exp (StringLit s) r, eval_exps rest r with
                            | Some v, Some vs -> Some (v :: vs)
                            | _, _ -> None)
  | e :: rest -> (match eval_exp e r, eval_exps rest r with
                  | Some v, Some vs -> Some (v :: vs)
                  | _, _ -> None)

type stack = (env * ident) list

type config = cmd * stack * env

let rec step_cmd (con : config) : config option =
  let (c, k, r) = con in
  match c with
  | AppendString (bar, str_exp) ->
    (match lookup r bar, eval_exp str_exp r with
    | Some (Val (StringListVal lst)), Some (StringVal str) ->
        Some (Skip, k, update r bar (Val (StringListVal (lst @ [str]))))
    | Some (Val (StringListVal lst)), Some (StringListVal str_lst) ->
        Some (Skip, k, update r bar (Val (StringListVal (lst @ str_lst))))
    | Some (Val (StringVal s)), Some (StringVal str) ->
        Some (Skip, k, update r bar (Val (StringVal (s ^ " " ^ str))))
    | _, _ -> None)
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
  (*extend string_of_value to handle string lists*)
  | StringListVal lst -> "[" ^ String.concat "; " lst ^ "]"


let string_of_env env =
  (* let x_val, y_val = lookup env "x", lookup env "y" in *)
  let key_val, tuning_val, song_name_val, artist_val =
    lookup env "Key", lookup env "Tuning", lookup env "Song Name", lookup env "Artist"
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

  (* xy_str ^ additional_strings *)
  additional_strings

let env_song_metadata =
  update_strings empty_env [
    ("Song Name", StringVal "Hey Jude");
    ("Artist", StringVal "The Beatles");
    ("Key", StringVal "D");
    ("Tuning", StringVal "Standard");
    ("Capo", StringVal "III")
  ]
let _ = print_endline ("song_metadata: " ^ string_of_env env_song_metadata)


let initial_env =
  update_strings empty_env [
    ("chord_bar", StringListVal []);
    ("music_bar", StringListVal [])
  ]
    
let rec seq_of_cmds cmds =
  match cmds with
  | [] -> Skip
  | [cmd] -> cmd
  | cmd :: rest -> Seq (cmd, seq_of_cmds rest)
    

let print_music_bar_tuple (data : music_bar_tuple) : unit =
  let print_chord_bar (chord_bar : chord_bar) =
    match chord_bar with
    | StringVal chords -> String.concat " " chords
    (*extend print_chord_bar to handle StringListVal*)
    | StringListVal lst -> String.concat " " lst
  in

  let print_music_bar (music_bar : music_bar) =
    match music_bar with
    | StringVal lyrics -> String.concat " " lyrics
    (*extend print_music_bar to handle StringListVal*)
    | StringListVal lst -> String.concat " " lst
  in

  let (chords, lyrics) = data in
  Printf.printf "Chords: %s\nLyrics: %s\n" (print_chord_bar chords) (print_music_bar lyrics)
;;
    
let main_program : cmd =
  seq_of_cmds
    [
      AppendString ("chord_bar", StringListVal ["D"; "A"; "A7"; "Asus4"; "A\n"]);
      AppendString ("music_bar", StringListVal ["Hey Jude"; "don't make"; "it bad, take"; "a sad song, and make it better\n"]);

      AppendString ("chord_bar", StringListVal ["G"; "D"; "A"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["Remember to let her into your"; "heart, then you can"; "start to"; "make it"; "better\n"]);

      AppendString ("chord_bar", StringListVal ["D"; "A"; "A7"; "Asus4"; "A"; "D\n"]);
      AppendString ("music_bar", StringListVal ["Hey Jude, don't be"; "afraid, you were"; "made"; "to go"; "out and"; "get her\n"]);

      AppendString ("chord_bar", StringListVal ["G"; "D"; "A"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["The minute you let her under your"; "skin, then you"; "begin to"; "make it"; "better\n"]);

      AppendString ("chord_bar", StringListVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["And anytime you feel the"; "pain, hey"; "Jude,"; "refrain, don't"; "carry the"; "world upon your"; "shoulder\n"]);

      AppendString ("chord_bar", StringListVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["For well you know that it's a"; "fool who"; "plays it"; "cool by"; "making his"; "world a little"; "colder\n"]);

      AppendString ("chord_bar", StringListVal ["D"; "D7"; "A7\n"]);
      AppendString ("music_bar", StringListVal ["Da da da"; "da da"; "da da da\n"]);

      AppendString ("chord_bar", StringListVal ["D"; "A"; "A7"; "Asus4"; "A"; "D\n"]);
      AppendString ("music_bar", StringListVal ["Hey Jude, don't let me"; "down, you have"; "found"; "her, now"; "go and"; "get her\n"]);

      AppendString ("chord_bar", StringListVal ["G"; "D"; "A"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["Remember to let her into your"; "heart, then you can"; "start to"; "make it"; "better\n"]);

      AppendString ("chord_bar", StringListVal ["G"; "D"; "A"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["Remember to let her into your"; "heart, then you can"; "start to"; "make it"; "better\n"]);

      AppendString ("chord_bar", StringListVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["So let it out and let it"; "in, hey Jude,"; "begin, you're"; "waiting for"; "someone to"; "perform with\n"]);

      AppendString ("chord_bar", StringListVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D\n"]);
      AppendString ("music_bar", StringListVal ["And don't you know that it's just"; "you, hey"; "Jude, you'll"; "do, the"; "movement you"; "need is on your"; "shoulders\n"]);

      AppendString ("chord_bar", StringListVal ["D"; "C"; "G"; "D"]);
      AppendString ("music_bar", StringListVal ["Na na na"; "na na na na"; "na na na na,"; "hey Jude"]);
    ]
  
    
let () =
  let final_config = run_prog main_program initial_env in
  match final_config with
  | _, _, env ->
    (match lookup env "music_bar", lookup env "chord_bar" with
    | Some (Val (StringListVal music_bar)), Some (Val (StringListVal chord_bar)) ->
      Printf.printf "Chords:\n %s\n\nLyris:\n %s\n" (String.concat " " chord_bar) (String.concat " " music_bar)
    | _ -> print_endline "Error: Couldn't retrieve elements from the environment");;





(* Populate the tuple *)
(* let verse_one_bar_one : music_bar_tuple =
  (StringVal ["D"; "A"; "A7"; "Asus4"; "A"; "D"], StringVal ["Hey Jude"; "don't make"; "it bad, take"; "a sad song, and make it better"])

(* Print the tuple to the console *)
let () = print_music_bar_tuple verse_one_bar_one 

let verse_one_bar_two : music_bar_tuple =
  (StringVal ["G"; "D"; "A"; "A7"; "D"], StringVal ["Remember to let her into your"; "heart, then you can"; "start to"; "make it"; "better"])
let () = print_music_bar_tuple verse_one_bar_two

let verse_one_bar_three : music_bar_tuple =
  (StringVal ["D"; "A"; "A7"; "Asus4"; "A"; "D"], StringVal ["Hey Jude, don't be"; "afraid, you were"; "made"; "to go"; "out and"; "get her"])
let () = print_music_bar_tuple verse_one_bar_three

let verse_one_bar_four : music_bar_tuple =
  (StringVal ["G"; "D"; "A"; "A7"; "D"], StringVal ["The minute you let her under your"; "skin, then you"; "begin to"; "make it"; "better"])
let () = print_music_bar_tuple verse_one_bar_four 


let chorus_one_bar_one : music_bar_tuple =
  (StringVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D"], StringVal ["And anytime you feel the"; "pain, hey"; "Jude,"; "refrain, don't"; "carry the"; "world upon your"; "shoulder"])
let () = print_music_bar_tuple chorus_one_bar_one

let chorus_one_bar_two : music_bar_tuple =
  (StringVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D"], StringVal ["For well you know that it's a"; "fool who"; "plays it"; "cool by"; "making his"; "world a little"; "colder"])
let () = print_music_bar_tuple chorus_one_bar_two


let interlude_bar : music_bar_tuple =
  (StringVal ["D"; "D7"; "A7"], StringVal ["Da da da"; "da da"; "da da da"])
let () = print_music_bar_tuple interlude_bar 


let verse_two_bar_one : music_bar_tuple =
  (StringVal ["D"; "A"; "A7"; "Asus4"; "A"; "D"], StringVal ["Hey Jude, don't let me"; "down, you have"; "found"; "her, now"; "go and"; "get her"])

let () = print_music_bar_tuple verse_two_bar_one 

let verse_two_bar_two : music_bar_tuple =
  (StringVal ["G"; "D"; "A"; "A7"; "D"], StringVal ["Remember to let her into your"; "heart, then you can"; "start to"; "make it"; "better"])
let () = print_music_bar_tuple verse_two_bar_two 


let chorus_two_bar_one : music_bar_tuple =
  (StringVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D"], StringVal ["So let it out and let it"; "in, hey Jude,"; "begin, you're"; "waiting for"; "someone to"; "perform with"])
let () = print_music_bar_tuple chorus_two_bar_one

let chorus_two_bar_two : music_bar_tuple =
  (StringVal ["D7"; "G"; "Bm"; "Em"; "G"; "A7"; "D"], StringVal ["And don't you know that it's just"; "you, hey"; "Jude, you'll"; "do, the"; "movement you"; "need is on your"; "shoulders"])
let () = print_music_bar_tuple chorus_two_bar_two


let () = print_music_bar_tuple interlude_bar 


let () = print_music_bar_tuple verse_one_bar_one 
let () = print_music_bar_tuple verse_one_bar_two *)


let outro_bar : music_bar_tuple =
  (StringVal ["D"; "C"; "G"; "D"], StringVal ["Na na na"; "na na na na"; "na na na na,"; "hey Jude"])

let print_music_bar_tuple_multiple_times (data : music_bar_tuple) (n : int) : unit =
  for _ = 1 to n do
    print_music_bar_tuple data
  done

let () = print_music_bar_tuple_multiple_times outro_bar 9

(* EOF *)
