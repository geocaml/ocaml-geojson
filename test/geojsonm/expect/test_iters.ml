open Eio

let buffer_to_dst buf bs =
  Flow.(copy (cstruct_source [ bs ]) (Flow.buffer_sink buf))

let value_to_buffer ?minify buf json =
  Geojsone.Ezjsone.value_to_dst ?minify (buffer_to_dst buf) json

let value_to_string ?minify json =
  let buf = Buffer.create 1024 in
  value_to_buffer ?minify buf json;
  Buffer.contents buf

let print_geometry g = print_endline @@ value_to_string (Geojsone.G.to_json g)
let print_property prop = print_endline @@ value_to_string prop

let with_src cwd f func =
  Eio.Path.(with_open_in (cwd / f)) @@ fun ic ->
  func @@ Geojsone_eio.src_of_flow ic

let () =
  Eio_main.run @@ fun env ->
  let or_fail = function
    | Ok () -> ()
    | Error e ->
        Geojsone.Err.pp Format.err_formatter e;
        failwith "Internal err"
  in
  let cwd = Eio.Stdenv.cwd env in
  or_fail
    ( with_src cwd "./input/simple.geojson" @@ fun src ->
      Geojsone.iter_geometry print_geometry src );
  or_fail
    ( with_src cwd "./input/simple.geojson" @@ fun src ->
      Geojsone.iter_props print_property src )
