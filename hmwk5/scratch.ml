let print_context (ct : context) : unit =
  let print_entry x entry =
    match entry with
    | Ty ty -> Printf.printf "Type %s: %s\n" x (match ty with IntTy -> "IntTy" | ClassTy c -> "ClassTy " ^ c)
    | Class cdecl -> Printf.printf "Class %s: %s\n" x cdecl.cname
  in
  Printf.printf "Context:\n";
  let entries = List.map (fun x -> (x, lookup ct x)) ["Square"; "Object"; "YourOtherClasses"] in
  List.iter (fun (x, entry) ->
    match entry with
    | Some e -> print_entry x e
    | None -> Printf.printf "Not found: %s\n" x
  ) entries

let () =
  print_context ct0