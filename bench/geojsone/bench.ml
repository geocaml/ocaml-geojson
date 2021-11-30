let n_fibres = [ 1 ]

module Vanilla = Geojsonm.Make (Jsonm)

let main () =
  Printf.printf "n_fibers, ns/iter, promoted/iter\n%!";
  n_fibres |> List.iter (fun n_fibres ->
      let _n_iters = 1000000 / n_fibres in
      let _minor0, prom0, _major0 = Gc.counters () in
      Gc.full_major ();
      let _minor1, prom1, _major1 = Gc.counters () in
      let prom = prom1 -. prom0 in
      Printf.printf "%5d, %.2f, %7.4f\n%!" n_fibres (1e9 *. 1.) (prom /. float 1)
    )

let () =
  main ()