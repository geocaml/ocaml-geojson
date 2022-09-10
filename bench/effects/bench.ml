open Eio

module Je = struct
  type src = Jsone.src
  type dst = Jsone.dst

  module G = Geojsone.G

  let map_geometry f s d =
    match Geojsone.map_geometry f s d with
    | Ok () -> Ok ()
    | Error _ -> Error "jsone err"
end

module Jm = struct
  type src = Jsonm.src
  type dst = Jsonm.dst

  module G = Geojsonm.G

  let map_geometry f s d =
    match Geojsonm.map_geometry f s d with
    | Ok () -> Ok ()
    | Error _ -> Error "jsone err"
end

let src_of_flow flow =
  let buff = Cstruct.create 65536 in
  fun () ->
    let got = Eio.Flow.(read flow buff) in
    let t = Cstruct.sub buff 0 got in
    t

let dst_of_flow flow bs = Eio.Flow.(copy (cstruct_source [ bs ]) flow)

let run_test_effects ~fs ~file ~out =
  let fn t =
    let open Geojsone in
    match t with
    | G.Geometry.Polygon arr, fm ->
        let add i p =
          G.Geometry.Position.(
            v
              ~lng:(lng p +. i)
              ~lat:(lat p +. i)
              ?altitude:(Option.map (( +. ) i) (altitude p))
              ())
        in
        let arr =
          Array.map (Array.map (add 0.01)) (G.Geometry.Polygon.to_positions arr)
        in
        G.Geometry.(Polygon (Polygon.of_positions arr), fm)
    | t -> t
  in
  Path.(with_open_in (fs / file)) @@ fun in_flow ->
  Path.(with_open_out ~create:(`If_missing 0o666) (fs / out)) @@ fun out_flow ->
  Je.map_geometry fn (src_of_flow in_flow) (dst_of_flow out_flow)

let run_test ~file ~out =
  let fn t =
    let open Geojsonm in
    match t with
    | G.Geometry.Polygon arr, fm ->
        let add i p =
          G.Geometry.Position.(
            v
              ~lng:(lng p +. i)
              ~lat:(lat p +. i)
              ?altitude:(Option.map (( +. ) i) (altitude p))
              ())
        in
        let arr =
          Array.map (Array.map (add 0.01)) (G.Geometry.Polygon.to_positions arr)
        in
        G.Geometry.(Polygon (Polygon.of_positions arr), fm)
    | t -> t
  in
  In_channel.with_open_bin file @@ fun ic ->
  Out_channel.with_open_bin out @@ fun oc ->
  Jm.map_geometry fn (`Channel ic) (`Channel oc)

let in_file = "./bench/geojson/inputs/arrondissements-occitanie.geojson"
let out_file = "./test.geojson"

let run_bench ~fs ~clock ~n_iters =
  Gc.full_major ();
  let _minor0, prom0, _major0 = Gc.counters () in
  let t0 = Eio.Time.now clock in
  for _i = 0 to n_iters do
    Result.get_ok
      (match fs with
      | Some fs -> run_test_effects ~fs ~file:in_file ~out:out_file
      | None -> run_test ~file:in_file ~out:out_file)
  done;
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. t0 in
  let time_per_iter = time_total /. float n_iters in
  let _minor1, prom1, _major1 = Gc.counters () in
  let prom = prom1 -. prom0 in
  Sys.remove out_file;
  Printf.printf "%8d, %7.2f, %13.4f\n%!" n_iters (1e9 *. time_per_iter)
    (prom /. float n_iters)

let main ~fs ~clock =
  Printf.printf " n_iters,      ns/iter, promoted/iter\n%!";
  [ 10; 20; 50; 100 ]
  |> List.iter (fun n_iters -> run_bench ~fs ~clock ~n_iters)

let () =
  Printf.printf "<><><><> Jsonm <><><><>\n%!";
  Eio_main.run @@ fun env ->
  main ~fs:None ~clock:(Eio.Stdenv.clock env);
  Printf.printf "<><><><> Jsone <><><><>\n%!";
  Eio_main.run @@ fun env ->
  main ~fs:(Some (Eio.Stdenv.fs env)) ~clock:(Eio.Stdenv.clock env)
