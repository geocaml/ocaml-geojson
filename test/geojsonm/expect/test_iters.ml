let print_geometry g =
  print_endline @@ Ezjsonm.value_to_string (Geojsonm.G.to_json g)

let print_property prop = print_endline @@ Ezjsonm.value_to_string prop

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
  let or_fail = function
    | Ok () -> ()
    | Error e ->
        Geojsonm.Err.pp Format.err_formatter e;
        failwith "Internal err"
  in
  or_fail
    ( with_src "./input/simple.geojson" @@ fun src ->
      Geojsonm.iter_geometry print_geometry src );
  or_fail
    ( with_src "./input/simple.geojson" @@ fun src ->
      Geojsonm.iter_props print_property src )
