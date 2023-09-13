(* Comparing Jsonm and Jsone for simple
   reading and writing JSON files. *)
module Jsone = Geojsone.Jsone

let ensure_ok = function `Ok -> () | `Partial -> assert false

let read_write decode encode =
  let rec loop () =
    match decode () with
    | `Lexeme _ as v ->
        encode v |> ensure_ok;
        loop ()
    | `End -> ()
    | `Error _ -> failwith "Error in read-write!"
    | `Await -> assert false
  in
  loop ()

let run_test_effects ~file ~out =
  Eio.Path.with_open_in file @@ fun in_flow ->
  Eio.Path.with_open_out ~create:(`If_missing 0o666) out @@ fun out_flow ->
  let decoder = Jsone.decoder (src_of_flow in_flow) in
  let encoder = Jsone.encoder (dst_of_flow out_flow) in
  read_write (fun () -> Jsone.decode decoder) (Jsone.encode encoder)

let run_test ~file ~out =
  In_channel.with_open_bin file @@ fun ic ->
  Out_channel.with_open_bin out @@ fun oc ->
  let decoder = Jsonm.decoder (`Channel ic) in
  let encoder = Jsonm.encoder (`Channel oc) in
  read_write (fun () -> Jsonm.decode decoder) (Jsonm.encode encoder)

let in_file fs = Eio.Path.(fs / "./bench/large-file.json")
let out_file fs = Eio.Path.(fs / "./test.geojson")

let run_bench ~fs ~effects ~clock ~n_iters =
  Gc.full_major ();
  let file = in_file fs in
  let out = out_file fs in
  let _minor0, prom0, _major0 = Gc.counters () in
  let t0 = Eio.Time.now clock in
  for _i = 0 to n_iters do
    match effects with
    | true -> run_test_effects ~file ~out
    | false -> run_test ~file:(snd file) ~out:(snd out)
  done;
  let t1 = Eio.Time.now clock in
  let time_total = t1 -. t0 in
  let time_per_iter = time_total /. float n_iters in
  let _minor1, prom1, _major1 = Gc.counters () in
  let prom = prom1 -. prom0 in
  Eio.Path.unlink out;
  Printf.printf "%8d, %7.2f, %13.4f\n%!" n_iters (1e9 *. time_per_iter)
    (prom /. float n_iters)

let main ~fs ~clock ~effects =
  Printf.printf " n_iters,      ns/iter, promoted/iter\n%!";
  [ 1; 5; 10 ]
  |> List.iter (fun n_iters -> run_bench ~fs ~effects ~clock ~n_iters)

let () =
  Printf.printf "<><><><> Jsonm <><><><>\n%!";
  Eio_main.run @@ fun env ->
  let fs = env#fs in
  main ~fs ~effects:false ~clock:(Eio.Stdenv.clock env);
  Printf.printf "<><><><> Jsone <><><><>\n%!";
  main ~fs ~effects:true ~clock:(Eio.Stdenv.clock env)
