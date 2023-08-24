(* hello.ml *)
module Hello = struct
  let hello_world () =
    "Hello, World!"
    
  let () =
    print_endline (hello_world ())
end

let hello_world () =
  "Hello, World!"
  
let () =
  print_endline (hello_world ())