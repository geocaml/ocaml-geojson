

let f ~name ?age () = print_endline name; Option.iter print_int age

let () =
  let name = "Alice" in
  let age = Some 10 in
  f ~name ?age ()