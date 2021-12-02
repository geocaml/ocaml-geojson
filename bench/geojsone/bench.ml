(* module Vanilla = Geojsonm.Make (Jsonm) *)
module Gm = Geojsonm.Make (Geojsonm.Jsone)

let read_file ~sw dir path =
  let flow = Eio.Dir.open_in ~sw dir path in
  let buff = Cstruct.create 2048 in
  fun () ->
    try 
      let got = Eio.Flow.read_into flow buff in
      let t = Some (Cstruct.to_bytes buff, 0, got) in
      t
    with End_of_file -> Eio.Flow.close flow; None

let write_file ~sw ~create ?append dir path = 
  let flow = Eio.Dir.open_out ~sw ~create ?append dir path in
  fun t -> match t with
  | None -> ()
  | Some (buff, off, len) ->
    Eio.Flow.(copy (cstruct_source [ Cstruct.of_bytes ~off ~len buff ]) flow)

let remove_all_coords t =
  let open Gm in
  match t with
  | G.Geometry.Polygon _ ->
    G.Geometry.(Polygon (Polygon.v [||]))
  | t -> t

let main ~sw env =
  let cwd = Eio.Stdenv.cwd env in
  let src = read_file ~sw cwd "simple.geojson" in
  let dst = write_file ~sw ~create:(`Exclusive 0o666) cwd "out.geojson" in
  Printf.printf "promoted/iter\n%!";
  let _minor0, prom0, _major0 = Gc.counters () in
  Gc.full_major ();
  (match Gm.map_geometry remove_all_coords src dst with
    | Ok () -> ()
    | Error e -> Gm.Err.pp Format.std_formatter e; failwith "err");
  let _minor1, prom1, _major1 = Gc.counters () in
  let prom = prom1 -. prom0 in
  Printf.printf "%7.4f\n%!" (prom /. float 1)

let () =
  Eio_main.run @@ fun env ->
  Eio.Std.Switch.run @@ fun sw ->
  main ~sw env