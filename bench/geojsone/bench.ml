module Gv = Geojsonm.Make (Jsonm)
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

(* Maybe it wasn't a good idea putting them inside the functor... *)
let remove_all_coords' t =
  let open Gv in
  match t with
  | G.Geometry.Polygon _ ->
    G.Geometry.(Polygon (Polygon.v [||]))
  | t -> t


let eff_main ~sw env =
  let cwd = Eio.Stdenv.cwd env in
  let src = read_file ~sw cwd "simple0.geojson" in
  let dst = write_file ~sw ~create:(`Exclusive 0o666) cwd "out0.geojson" in
  Printf.printf "\n<><> Effects <><>\npromoted/iter\n%!";
  let _minor0, prom0, _major0 = Gc.counters () in
  Gc.full_major ();
  (match Gm.map_geometry remove_all_coords src dst with
    | Ok () -> ()
    | Error e -> Gm.Err.pp Format.std_formatter e; failwith "err");
  let _minor1, prom1, _major1 = Gc.counters () in
  let prom = prom1 -. prom0 in
  Printf.printf "%7.4f\n%!" (prom /. float 1)

let normal_main () =
  Printf.printf "\n<><> Normal <><>\npromoted/iter\n%!";
  let ic = open_in "simple1.geojson" in
  let oc = open_out "out1.geojson" in
  try 
    let _minor0, prom0, _major0 = Gc.counters () in
    Gc.full_major ();
    (match Gv.map_geometry remove_all_coords' (`Channel ic) (`Channel oc) with
      | Ok () -> ()
      | Error e -> Gv.Err.pp Format.std_formatter e; failwith "err");
    let _minor1, prom1, _major1 = Gc.counters () in
    let prom = prom1 -. prom0 in
    Printf.printf "%7.4f\n%!" (prom /. float 1);
    close_in ic; close_out oc
  with e ->
    close_in ic; close_out oc; raise e 

let run_eff () = 
  Eio_main.run @@ fun env ->
    Eio.Std.Switch.run @@ fun sw ->
    eff_main ~sw env

let () =
  (* TODO: Run tests multiple times and average *)
  normal_main ();
  (* run_eff (); *)
  Sys.remove "out1.geojson";
  (* Sys.remove "out0.geojson"; *)
  normal_main ();
  (* run_eff (); *)
  Sys.remove "out1.geojson";
  (* Sys.remove "out0.geojson"; *)
  normal_main ();
  run_eff ();
  