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
  | G.Geometry.Polygon _, fm -> G.Geometry.(Polygon (Polygon.v [||]), fm)
  | t -> t

let get_string_exn = function `String s -> s | _ -> failwith "err"

let get_name = function
  | `O assoc -> List.assoc "nom" assoc |> get_string_exn
  | _ -> failwith "err"

let src_of_flow flow =
  let buff = Cstruct.create 2048 in
  fun () ->
    try
      let got = Eio.Flow.(read flow buff) in
      let t = Some (Cstruct.to_bytes buff, 0, got) in
      t
    with End_of_file -> None

let with_src cwd f func =
  Eio.Dir.with_open_in cwd f @@ fun ic -> func @@ src_of_flow ic

let buffer_to_dst buf = function
  | Some (bs, off, len) ->
      Eio.Flow.(
        copy
          (cstruct_source [ Cstruct.of_bytes ~off ~len bs ])
          (Eio.Flow.buffer_sink buf))
  | None -> ()

let () =
  Eio_main.run @@ fun env ->
  let dst = Buffer.create 1000 in
  let print_or_fail = function
    | Ok () -> Format.printf "%s\n\n" @@ Buffer.contents dst
    | Error e ->
        Geojsonm.Err.pp Format.err_formatter e;
        failwith "Internal err"
  in
  let cwd = Eio.Stdenv.cwd env in
  print_or_fail
    ( with_src cwd "./input/simple.geojson" @@ fun src ->
      Geojsonm.map_props capitalise_nom src (buffer_to_dst dst) );
  Buffer.clear dst;
  print_or_fail
    ( with_src cwd "./input/simple.geojson" @@ fun src ->
      Geojsonm.map_geometry remove_all_coords src (buffer_to_dst dst) );
  Buffer.clear dst;
  match
    with_src cwd "./input/simple.geojson" @@ fun src ->
    Geojsonm.fold_props (fun acc p -> get_name p :: acc) [] src
  with
  | Ok lst ->
      Format.printf "Places: %a" Format.(pp_print_list pp_print_string) lst
  | Error e ->
      Geojsonm.Err.pp Format.err_formatter e;
      failwith "Internal err"
