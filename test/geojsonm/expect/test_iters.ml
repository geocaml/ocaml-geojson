let remove_coords p =
  let open Geojsonm in
  match p with G.Geometry.Polygon _ -> G.Geometry.(()) | p -> ()

let capitalise_nom obj =
  let rec capitalise_nom acc = function
    | [] -> List.rev acc
    | ("nom", `String nom) :: xs ->
        capitalise_nom (("nom", `String (String.uppercase_ascii nom)) :: acc) xs
    | x :: xs -> capitalise_nom (x :: acc) xs
  in
  match obj with `O assoc -> `O (capitalise_nom [] assoc) | x -> x

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
      Geojsonm.iter_geometry remove_coords src );
  `Buffer dst;
  Buffer.clear dst;
  print_or_fail
    ( with_src "./input/simple.geojson" @@ fun src ->
      Geojsonm.iter_props capitalise_nom src (`Buffer dst) );
  Buffer.clear dst
