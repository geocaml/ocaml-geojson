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

module Ezjsone_parser = struct
  type t = Ezjsone.value

  let catch_err f v =
    try Ok (f v) with Ezjsone.Parse_error (_, s) -> Error (`Msg s)

  let find = Ezjsone.find_opt
  let to_string t = catch_err Ezjsone.get_string t
  let string = Ezjsone.string
  let to_float t = catch_err Ezjsone.get_float t
  let float = Ezjsone.float
  let to_int t = catch_err Ezjsone.get_int t
  let int = Ezjsone.int
  let to_list f t = catch_err (Ezjsone.get_list f) t
  let list f t = Ezjsone.list f t
  let to_array f t = Result.map Array.of_list @@ to_list f t
  let array f t = list f (Array.to_list t)
  let to_obj t = catch_err Ezjsone.get_dict t
  let obj = Ezjsone.dict
  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

module Geojson = Geojson.Make (Ezjsone_parser)

let ezjsone =
  Alcotest.of_pp (fun ppf t -> Fmt.pf ppf "%s" (Ezjsone.value_to_string t))

let (msg : [ `Msg of string ] Alcotest.testable) =
  Alcotest.of_pp (fun ppf (`Msg m) -> Fmt.pf ppf "%s" m)

let _get_all_props s =
  let json = Ezjsone.value_from_string s in
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
  let json = Ezjsone.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (MultiLineString m, []) -> (g, m)
        | _ -> assert false)
    | _ -> assert false
  in

  let json' = Geojson.to_json geo in
  let t =
    Geojson.Geometry.(
      Array.map (fun v ->
          Array.map (fun l -> [| Position.lng l; Position.lat l |])
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
  Alcotest.(check ezjsone) "same json" json json'

let test_multi_point () =
  let s = read_file "files/valid/multipoint.json" in
  let json = Ezjsone.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (MultiPoint p, []) -> (g, p)
        | _ -> assert false)
    | _ -> assert false
  in
  let json' = Geojson.to_json geo in
  let t =
    Geojson.Geometry.(
      Array.map (fun l -> [| Position.lng l; Position.lat l |])
      @@ MultiPoint.coordinates coords)
  in

  Alcotest.(check (array @@ array (float 0.)))
    "same point"
    [| [| 100.0; 0.0 |]; [| 101.0; 1.0 |] |]
    t;
  Alcotest.(check ezjsone) "same json" json json'

let test_point () =
  let s = read_file "files/valid/point.json" in
  let json = Ezjsone.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (Point p, []) -> (g, p)
        | _ -> assert false)
    | _ -> assert false
  in
  let json' = Geojson.to_json geo in
  let open Geojson.Geometry in
  let pos = Point.position coords in
  let p = [| Position.lng pos; Position.lat pos |] in

  Alcotest.(check (array (float 0.))) "same point" [| 125.6; 10.1 |] p;
  Alcotest.(check ezjsone) "same json" json json'

let test_linestring () =
  let s = read_file "files/valid/linestring.json" in
  let json = Ezjsone.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (LineString p, []) -> (g, p)
        | _ -> assert false)
    | _ -> assert false
  in
  let json' = Geojson.to_json geo in
  let l =
    Geojson.Geometry.(
      Array.map (fun l -> [| Position.lng l; Position.lat l |])
      @@ LineString.coordinates coords)
  in

  Alcotest.(check (array @@ array (float 0.)))
    "same linestring"
    [| [| 100.0; 0. |]; [| 101.0; 1.0 |] |]
    l;
  Alcotest.(check ezjsone) "same json" json json'

let test_polygon () =
  let s = read_file "files/valid/polygon.json" in
  let json = Ezjsone.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (Polygon p, []) -> (g, p)
        | _ -> assert false)
    | _ -> assert false
  in

  let json' = Geojson.to_json geo in
  let t =
    Geojson.Geometry.(
      Array.map (fun v ->
          Array.map (fun l -> [| Position.lng l; Position.lat l |])
          @@ LineString.coordinates v)
      @@ Polygon.rings coords)
  in

  Alcotest.(check (array @@ array @@ array (float 0.)))
    "same polygon"
    [|
      [|
        [| 100.0; 0.0 |];
        [| 101.0; 0.0 |];
        [| 101.0; 1.0 |];
        [| 100.0; 1.0 |];
        [| 100.0; 0.0 |];
      |];
    |]
    t;
  Alcotest.(check ezjsone) "same json" json json'

let test_multi_polygon () =
  let s = read_file "files/valid/multi_polygon.json" in
  let json = Ezjsone.value_from_string s in
  let geo = Geojson.of_json json in
  let geo, coords =
    match geo with
    | Ok g -> (
        match Geojson.geojson g with
        | Geometry (MultiPolygon mp, []) -> (g, mp)
        | _ -> assert false)
    | _ -> assert false
  in

  let json' = Geojson.to_json geo in
  let t =
    Geojson.Geometry.(
      Array.map (fun v ->
          Array.map (fun n ->
              Array.map (fun l -> [| Position.lng l; Position.lat l |])
              @@ LineString.coordinates n)
          @@ Polygon.rings v)
      @@ MultiPolygon.polygons coords)
  in

  Alcotest.(check (array @@ array @@ array @@ array (float 0.)))
    "same multi-polygon"
    [|
      [|
        [|
          [| 102.0; 2.0 |];
          [| 103.0; 2.0 |];
          [| 103.0; 3.0 |];
          [| 102.0; 3.0 |];
          [| 102.0; 2.0 |];
        |];
      |];
      [|
        [|
          [| 100.0; 0.0 |];
          [| 101.0; 0.0 |];
          [| 101.0; 1.0 |];
          [| 100.0; 1.0 |];
          [| 100.0; 0.0 |];
        |];
        [|
          [| 100.2; 0.2 |];
          [| 100.2; 0.8 |];
          [| 100.8; 0.8 |];
          [| 100.8; 0.2 |];
          [| 100.2; 0.2 |];
        |];
      |];
    |]
    t;
  Alcotest.(check ezjsone) "same json" json json'

let test_feature () =
  let s = read_file "files/valid/feature.json" in
  let json = Ezjsone.value_from_string s in
  let feature = Geojson.of_json json in
  let prop_from_file =
    Ezjsone.value_from_string @@ read_file "files/valid/prop1.json"
  in
  let property = match _get_all_props s with [ x ] -> x | _ -> assert false in
  let f, coord, foreign_members =
    match feature with
    | Ok v -> (
        match Geojson.geojson v with
        | Feature t -> (
            let foreign_members = Geojson.Feature.foreign_members t in
            match Geojson.Feature.geometry t with
            | Some (MultiPoint p, []) -> (v, p, foreign_members)
            | _ -> assert false)
        | _ -> assert false)
    | _ -> assert false
  in

  let json' = Geojson.to_json f in
  let t =
    Geojson.Geometry.(
      Array.map (fun l -> [| Position.lng l; Position.lat l |])
      @@ MultiPoint.coordinates coord)
  in
  Alcotest.(check (array @@ array (float 0.)))
    "same point"
    [| [| 125.1; 40.0 |]; [| 155.9; 22.5 |] |]
    t;
  Alcotest.(check (list (pair string ezjsone)))
    "same string"
    [ ("title", `String "Some Islands") ]
    foreign_members;
  Alcotest.(check ezjsone) "same json" prop_from_file property;
  Alcotest.(check ezjsone) "same json" json json'

let test_feature_collection () =
  let s = read_file "files/valid/featurecollection.json" in
  let json = Ezjsone.value_from_string s in
  let feature_collection =
    match Geojson.of_json json with Ok v -> v | _ -> assert false
  in
  let prop_from_file1 =
    Ezjsone.value_from_string @@ read_file "files/valid/prop1.json"
  in
  let prop_from_file2 =
    Ezjsone.value_from_string @@ read_file "files/valid/prop2.json"
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
            | Some (MultiPoint a, []), Some (MultiLineString b, []) -> (a, b)
            | _, _ -> assert false)
        | _ -> assert false)
    | _ -> assert false
  in
  let json' = Geojson.to_json feature_collection in
  let mp =
    Geojson.Geometry.(
      Array.map (fun l -> [| Position.lng l; Position.lat l |])
      @@ MultiPoint.coordinates c1)
  in
  let ml =
    Geojson.Geometry.(
      Array.map (fun v ->
          Array.map (fun l -> [| Position.lng l; Position.lat l |])
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
  Alcotest.(check ezjsone) "same json" prop_from_file1 prop1;
  Alcotest.(check ezjsone) "same json" prop_from_file2 prop2;
  Alcotest.(check ezjsone) "same json" json json'

let test_bbox () =
  let s = read_file "files/valid/geo_with_bbox.json" in
  let json = Ezjsone.value_from_string s in
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
  Alcotest.(check ezjsone) "same json" json json'

let test_3d_feature_collection () =
  let s = read_file "files/valid/3d_featurecollection.json" in
  let json = Ezjsone.value_from_string s in
  let feature_collection =
    match Geojson.of_json json with Ok v -> v | _ -> assert false
  in
  let prop_from_file1 =
    Ezjsone.value_from_string @@ read_file "files/valid/prop1.json"
  in
  let prop_from_file2 =
    Ezjsone.value_from_string @@ read_file "files/valid/prop2.json"
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
            | Some (MultiPoint a, []), Some (MultiLineString b, []) -> (a, b)
            | _, _ -> assert false)
        | _ -> assert false)
    | _ -> assert false
  in
  let json' = Geojson.to_json feature_collection in
  let mp =
    Geojson.Geometry.(
      Array.map (fun l ->
          [|
            Position.lng l; Position.lat l; Option.get @@ Position.altitude l;
          |])
      @@ MultiPoint.coordinates c1)
  in
  let ml =
    Geojson.Geometry.(
      Array.map (fun v ->
          Array.map (fun l ->
              [|
                Position.lng l;
                Position.lat l;
                Option.get @@ Position.altitude l;
              |])
          @@ LineString.coordinates v)
      @@ MultiLineString.lines c2)
  in

  Alcotest.(check (array @@ array (float 0.)))
    "same point"
    [| [| 130.1; 40.0; 33.3 |]; [| 143.7; 22.5; 15.0 |] |]
    mp;
  Alcotest.(check (array @@ array @@ array (float 0.)))
    "same multi_line_string"
    [|
      [| [| 170.0; 45.0; 60.2 |]; [| 180.0; 45.0; 35.0 |] |];
      [| [| -180.0; 45.0; 35.0 |]; [| -170.0; 45.0; 60.2 |] |];
    |]
    ml;
  Alcotest.(check ezjsone) "same json" prop_from_file1 prop1;
  Alcotest.(check ezjsone) "same json" prop_from_file2 prop2;
  Alcotest.(check ezjsone) "same json" json json'

let geojson =
  Alcotest.testable
    (fun ppf p -> Fmt.pf ppf "%s" (Ezjsone.value_to_string (Geojson.to_json p)))
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
          Alcotest.test_case "point" `Quick test_point;
          Alcotest.test_case "linestring" `Quick test_linestring;
          Alcotest.test_case "polygon" `Quick test_polygon;
          Alcotest.test_case "multi_polygon" `Quick test_multi_polygon;
        ] );
      ("feature", [ Alcotest.test_case "feature" `Quick test_feature ]);
      ( "feature-collection",
        [
          Alcotest.test_case "feature-collection" `Quick test_feature_collection;
        ] );
      ("random", [ Alcotest.test_case "simple-random" `Quick test_random ]);
      ("bbox", [ Alcotest.test_case "bbox" `Quick test_bbox ]);
      ("3D", [ Alcotest.test_case "3D" `Quick test_3d_feature_collection ]);
    ]
