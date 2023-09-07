(*
REF:
  - ChatGPT-3.5 Free Research Preview used to assist debugging: https://chat.openai.com/?model=text-davinci-002-render-sha
  - OCaml STBLIB docs references also: https://v2.ocaml.org/api/Stdlib.html
  - Written using VSCode and the OCaml Platform extension for VSCode
*)

(* question 1 *)
let rec sum_range (min : int) (max : int) : int = 
  if min > max then
    0
  else
    min + sum_range (min+1) max ;;

let result_one = sum_range 2 9;; (* should return 44 *)
Printf.printf "sum range 2 9 = %d\n" result_one

let result_two = sum_range 3 3;; (* should return 3 *)
Printf.printf "sum range 3 3 = %d\n" result_two

(* question 2 *)
let sub2 (x : int * int) : int =
  (* subtracts the first from the second*)
  let second = snd x in
  let first = fst x in
  second - first

let result_three = sub2 (1, 3);; (* should return 2 *)
Printf.printf "sub2 1 3 = %d\n" result_three ;;

let result_four = sub2 (6, -1);; (* should return -7 *)
Printf.printf "sub2 6 -1 = %d\n" result_four;;

(* question 3 *)
(* type tree = <fill in here> ;; *)
type tree = 
  | Leaf of int
  | Node of tree * tree

(* question 4 *)
(* let my_tree : tree = <fill in here> ;; *)
let my_tree = Leaf 5
let my_tree_two = Node (Leaf 3, Leaf 7)
let my_tree_three = Node (Leaf 1, Node (Leaf 2, Leaf 3))

(* question 5 *)
let rec tree_sum (t : tree) : int =
  match t with
  | Leaf value -> value
  | Node (left, right) -> tree_sum left + tree_sum right

(* Testing the sum_tree_values function *)
let () =
  let my_tree = Node (Leaf 10, Node (Leaf 20, Leaf 30)) in
  let sum = tree_sum my_tree in
  Printf.printf "Sum of tree values: %d\n" sum
