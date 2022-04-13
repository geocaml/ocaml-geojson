open Bechamel
open Toolkit

module Ezjsonm_parser = struct
  type t = Ezjsonm.value

  let catch_err f v =
    try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

  let find = Ezjsonm.find_opt
  let to_string t = catch_err Ezjsonm.get_string t
  let string = Ezjsonm.string
  let to_float t = catch_err Ezjsonm.get_float t
  let float = Ezjsonm.float
  let to_list f t = catch_err (Ezjsonm.get_list f) t
  let list f t = Ezjsonm.list f t
  let to_array f t = Result.map Array.of_list @@ to_list f t
  let array f t = list f (Array.to_list t)
  let obj = Ezjsonm.dict
  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

module Geo = Geojson.Make (Ezjsonm_parser)

let find_code expected_code nom file =
  let s =
    Result.get_ok @@ Bos.OS.File.read (Fpath.v file) |> Ezjsonm.from_string
  in
  let find f =
    match Geo.Feature.properties f with
    | Some props ->
        let nom' = Ezjsonm.find props [ "nom" ] |> Ezjsonm.get_string in
        nom = nom'
    | _ -> false
  in
  Staged.stage @@ fun () ->
  ignore
  @@
  let code =
    match Geo.of_json s with
    | Error (`Msg m) -> failwith m
    | Ok v -> (
        match Geo.geojson v with
        | FeatureCollection fcs -> (
            let features = Geo.Feature.Collection.features fcs in
            match List.find_opt (fun f -> find f) features with
            | Some f ->
                Option.map (fun v ->
                    Ezjsonm.find v [ "code" ] |> Ezjsonm.get_string)
                @@ Geo.Feature.properties f
            | None -> None)
        | _ -> assert false)
  in
  if expected_code = code then ()
  else failwith ("Got " ^ Option.value ~default:"None" code)

let of_json_to_json file =
  let s =
    Result.get_ok @@ Bos.OS.File.read (Fpath.v file) |> Ezjsonm.from_string
  in
  Staged.stage @@ fun () ->
  ignore
  @@
  match Geo.of_json s with
  | Ok geo -> Geo.to_json geo
  | Error (`Msg m) -> failwith m

let test =
  Test.make_grouped ~name:"standard-geojsom"
    [
      Test.make ~name:"of_to_json"
        (of_json_to_json "inputs/arrondissements-occitanie.geojson");
      Test.make ~name:"find"
        (find_code (Some "31003") "Toulouse"
           "inputs/arrondissements-occitanie.geojson");
    ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark () in
  img (window, results) |> eol |> output_image
