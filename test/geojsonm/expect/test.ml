let capitalise_nom obj =
  let rec capitalise_nom acc = function
    | [] -> List.rev acc
    | ("nom", `String nom) :: xs ->
        capitalise_nom (("nom", `String (String.uppercase_ascii nom)) :: acc) xs
    | x :: xs -> capitalise_nom (x :: acc) xs
  in
  match obj with `O assoc -> `O (capitalise_nom [] assoc) | x -> x

let get_string_exn = function `String s -> s | _ -> failwith "err"

let get_name = function
  | `O assoc -> List.assoc "nom" assoc |> get_string_exn
  | _ -> failwith "err"

let with_src f func =
  let ic = open_in f in
  let res =
    try func (`Channel ic)
    with e ->
      close_in ic;
      raise e
  in
  close_in ic;
  res

let () =
  let dst = Buffer.create 1000 in
  let print_or_fail = function
    | Ok () -> Format.printf "%s\n\n" @@ Buffer.contents dst
    | Error e ->
        Geojsonm.Err.pp Format.err_formatter e;
        failwith "Internal err"
  in
  print_or_fail
    ( with_src "./input/simple.geojson" @@ fun src ->
      Geojsonm.map_props ~f:capitalise_nom src (`Buffer dst) );
  match
    with_src "./input/simple.geojson" @@ fun src ->
    Geojsonm.fold_props src ~f:(fun acc p -> get_name p :: acc) ~init:[]
  with
  | Ok lst ->
      Format.printf "Places: %a" Format.(pp_print_list pp_print_string) lst
  | Error e ->
      Geojsonm.Err.pp Format.err_formatter e;
      failwith "Internal err"
