let capitalise_nom obj =
  let rec capitalise_nom acc = function
    | [] -> List.rev acc
    | ("nom", `String nom) :: xs ->
        capitalise_nom (("nom", `String (String.uppercase_ascii nom)) :: acc) xs
    | x :: xs -> capitalise_nom (x :: acc) xs
  in
  match obj with `O assoc -> `O (capitalise_nom [] assoc) | x -> x

let remove_all_coords t =
  let open Geojsonm in
  match t with
  | G.Geometry.Polygon _ -> G.Geometry.(Polygon (Polygon.v [||]))
  | t -> t

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
  let print_or_fail = function
    | Ok () -> Format.printf "%s\n\n"
    | Error e ->
        Geojsonm.Err.pp Format.err_formatter e;
        failwith "Internal err"
  in
  print_or_fail
    ( with_src "./input/simple.geojson" @@ fun src ->
      Geojsonm.iter_props capitalise_nom src );
  print_or_fail
    ( with_src "./input/simple.geojson" @@ fun src ->
      Geojsonm.iter_geometry remove_all_coords src )
