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
  let open Geojson in
  match geo with
  | Ok v -> (
      match geojson v with
      | Feature f -> Option.to_list @@ Geojson.Feature.properties f
      | FeatureCollection f ->
          let fs = Geojson.Feature.Collection.features f in
          List.filter_map Geojson.Feature.properties fs
      | _ -> [])
  | _ -> []

let test_multi_line () =
  let s = read_file "files/valid/multilinestring.json" in
  let json = Ezjsonm.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (MultiLineString m) -> (g, m)
        | _ -> assert false)
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
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (MultiPoint p) -> (g, p)
        | _ -> assert false)
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

let test_feature () =
  let s = read_file "files/valid/feature.json" in
  let json = Ezjsonm.value_from_string s in
  let feature = Geojson.of_json json in
  let prop_from_file =
    Ezjsonm.value_from_string @@ read_file "files/valid/prop1.json"
  in
  let property = match _get_all_props s with [ x ] -> x | _ -> assert false in
  let f, coord =
    match feature with
    | Ok v -> (
        match Geojson.geojson v with
        | Feature t -> (
            match Geojson.Feature.geometry t with
            | Some (MultiPoint p) -> (v, p)
            | _ -> assert false)
        | _ -> assert false)
    | _ -> assert false
  in
  let json' = Geojson.to_json f in
  let t =
    Geojson.Geometry.(
      Array.map (fun l -> [| Position.long l; Position.lat l |])
      @@ MultiPoint.coordinates coord)
  in

  Alcotest.(check (array @@ array (float 0.)))
    "same point"
    [| [| 125.1; 40.0 |]; [| 155.9; 22.5 |] |]
    t;
  Alcotest.(check ezjsonm) "same json" prop_from_file property;
  Alcotest.(check ezjsonm) "same json" json json'

let test_feature_collection () =
  let s = read_file "files/valid/featurecollection.json" in
  let json = Ezjsonm.value_from_string s in
  let feature_collection =
    match Geojson.of_json json with Ok v -> v | _ -> assert false
  in
  let prop_from_file1 =
    Ezjsonm.value_from_string @@ read_file "files/valid/prop1.json"
  in
  let prop_from_file2 =
    Ezjsonm.value_from_string @@ read_file "files/valid/prop2.json"
  in
  let prop1, prop2 =
    match _get_all_props s with [ x; y ] -> (x, y) | _ -> assert false
  in
  let c1, c2 =
    match Geojson.geojson feature_collection with
    | FeatureCollection fc -> (
        match Geojson.Feature.Collection.features fc with
        | [ x; y ] -> (
            match (Geojson.Feature.geometry x, Geojson.Feature.geometry y) with
            | Some (MultiPoint a), Some (MultiLineString b) -> (a, b)
            | _, _ -> assert false)
        | _ -> assert false)
    | _ -> assert false
  in
  let json' = Geojson.to_json feature_collection in
  let mp =
    Geojson.Geometry.(
      Array.map (fun l -> [| Position.long l; Position.lat l |])
      @@ MultiPoint.coordinates c1)
  in
  let ml =
    Geojson.Geometry.(
      Array.map (fun v ->
          Array.map (fun l -> [| Position.long l; Position.lat l |])
          @@ LineString.coordinates v)
      @@ MultiLineString.lines c2)
  in

  Alcotest.(check (array @@ array (float 0.)))
    "same point"
    [| [| 125.1; 40.0 |]; [| 155.9; 22.5 |] |]
    mp;
  Alcotest.(check (array @@ array @@ array (float 0.)))
    "same multi_line_string"
    [|
      [| [| 170.0; 45.0 |]; [| 180.0; 45.0 |] |];
      [| [| -180.0; 45.0 |]; [| -170.0; 45.0 |] |];
    |]
    ml;
  Alcotest.(check ezjsonm) "same json" prop_from_file1 prop1;
  Alcotest.(check ezjsonm) "same json" prop_from_file2 prop2;
  Alcotest.(check ezjsonm) "same json" json json'

let test_bbox () =
  let s = read_file "files/valid/geo_with_bbox.json" in
  let json = Ezjsonm.value_from_string s in
  let geojson_obj = Geojson.of_json json in
  let bbox =
    match geojson_obj with
    | Ok v -> ( match Geojson.bbox v with Some x -> x | _ -> assert false)
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
      ("feature", [ Alcotest.test_case "feature" `Quick test_feature ]);
      ( "feature-collection",
        [
          Alcotest.test_case "feature-collection" `Quick test_feature_collection;
        ] );
      ("random", [ Alcotest.test_case "simple-random" `Quick test_random ]);
      ("bbox", [ Alcotest.test_case "bbox" `Quick test_bbox ]);
    ]
