(* test_hello.ml *)
open OUnit2
open Hello

let test_hello_world _ =
  assert_equal "Hello, World!" (hello_world ())

let suite =
  "suite" >::: [
    "test_hello_world" >:: test_hello_world
  ]

let () =
  run_test_tt_main suite
