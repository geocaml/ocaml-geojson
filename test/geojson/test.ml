let read_file f =
  let ic = open_in f in
  let rec loop acc =
    try loop (input_line ic :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let lines =
    try loop []
    with _ ->
      close_in ic;
      failwith "Something went wrong reading file"
  in
  String.concat "\n" lines

module Ezjsonm_parser = struct
  type t = Ezjsonm.value

  let catch_err f v =
    try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

  let find = Ezjsonm.find_opt
  let to_string t = catch_err Ezjsonm.get_string t
  let string = Ezjsonm.string
  let to_float t = catch_err Ezjsonm.get_float t
  let float = Ezjsonm.float

  let to_list f t =
    print_endline @@ Ezjsonm.value_to_string t;
    catch_err (Ezjsonm.get_list f) t

  let list f t = Ezjsonm.list f t
  let to_array f t = Result.map Array.of_list @@ to_list f t
  let array f t = list f (Array.to_list t)
  let obj = Ezjsonm.dict
  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

module Geojson = Geojson.Make (Ezjsonm_parser)

let ezjsonm =
  Alcotest.of_pp (fun ppf t -> Fmt.pf ppf "%s" (Ezjsonm.value_to_string t))

let (msg : [ `Msg of string ] Alcotest.testable) =
  Alcotest.of_pp (fun ppf (`Msg m) -> Fmt.pf ppf "%s" m)

let _get_all_props s =
  let json = Ezjsonm.value_from_string s in
  let geo = Geojson.of_json json in
  match geo with
  | Ok { geojson = Feature f; _ } ->
      Option.to_list @@ Geojson.Feature.properties f
  | Ok { geojson = FeatureCollection fc; _ } ->
      let fs = Geojson.Feature.Collection.features fc in
      List.filter_map Geojson.Feature.properties fs
  | _ -> []

let test_multi_line () =
  let s = read_file "files/valid/multilinestring.json" in
  let json = Ezjsonm.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok ({ geojson = Geometry (MultiLineString m); _ } as g) -> (g, m)
    | _ -> assert false
  in
  let json' = Geojson.to_json geo in
  let t =
    Geojson.Geometry.(
      Array.map (fun v ->
          Array.map (fun l -> [| Position.long l; Position.lat l |])
          @@ LineString.coordinates v)
      @@ MultiLineString.lines coords)
  in

  Alcotest.(check (array @@ array @@ array (float 0.)))
    "same multi_line_string"
    [|
      [| [| 170.0; 45.0 |]; [| 180.0; 45.0 |] |];
      [| [| -180.0; 45.0 |]; [| -170.0; 45.0 |] |];
    |]
    t;
  Alcotest.(check ezjsonm) "same json" json json'

let test_multi_point () =
  let s = read_file "files/valid/multipoint.json" in
  let json = Ezjsonm.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok ({ geojson = Geometry (MultiPoint p); _ } as g) -> (g, p)
    | _ -> assert false
  in
  let json' = Geojson.to_json geo in
  let t =
    Geojson.Geometry.(
      Array.map (fun l -> [| Position.long l; Position.lat l |])
      @@ MultiPoint.coordinates coords)
  in

  Alcotest.(check (array @@ array (float 0.)))
    "same point"
    [| [| 100.0; 0.0 |]; [| 101.0; 1.0 |] |]
    t;
  Alcotest.(check ezjsonm) "same json" json json'

let test_bbox () =
  let s = read_file "files/valid/geo_with_bbox.json" in
  let json = Ezjsonm.value_from_string s in
  let geojson_obj = Geojson.of_json json in
  let bbox =
    match geojson_obj with
    | Ok { geojson = _; bbox = x } -> Option.get x
    | _ -> assert false
  in
  let json' = Geojson.to_json @@ Result.get_ok geojson_obj in

  Alcotest.(check (array (float 0.)))
    "same bbox"
    [| 100.0; 0.0; 101.0; 1.0 |]
    bbox;
  Alcotest.(check ezjsonm) "same json" json json'

let geojson =
  Alcotest.testable
    (fun ppf p -> Fmt.pf ppf "%s" (Ezjsonm.value_to_string (Geojson.to_json p)))
    (fun a b -> Geojson.to_json a = Geojson.to_json b)

let test_random () =
  let open Geojson.Random in
  let r =
    FC
      [
        { properties = None; geometry = Point };
        { properties = None; geometry = LineString 2 };
        { properties = None; geometry = Polygon 2 };
        {
          properties = Some (`O [ ("name", `String "abcd") ]);
          geometry = MultiPolygon (3, 3);
        };
      ]
  in
  let expect = random ~f:(fun () -> Random.float 100.) r in
  let actual = Geojson.to_json expect |> Geojson.of_json in
  Alcotest.(check (result geojson msg)) "same geojson" (Ok expect) actual

let () =
  Alcotest.run "geojson"
    [
      ( "geometry",
        [
          Alcotest.test_case "multi-line" `Quick test_multi_line;
          Alcotest.test_case "multi-point" `Quick test_multi_point;
        ] );
      ("random", [ Alcotest.test_case "simple-random" `Quick test_random ]);
      ("bbox", [ Alcotest.test_case "bbox" `Quick test_bbox ]);
    ]
