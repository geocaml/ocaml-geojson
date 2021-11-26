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
  let to_list f t = catch_err (Ezjsonm.get_list f) t
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

let test_multi_line () =
  let open Geojson in
  let s = read_file "files/valid/multilinestring.json" in
  let json = Ezjsonm.value_from_string s in
  let coords = Geometry.MultiLineString.of_json json in
  let json' = Result.map Geometry.MultiLineString.to_json coords in
  let t =
    Result.map
      (fun v ->
        Geometry.(
          Array.map (fun v ->
              Array.map (fun l -> [| Position.long l; Position.lat l |])
              @@ LineString.coordinates v)
          @@ MultiLineString.lines v))
      coords
  in
  Alcotest.(check (result (array @@ array @@ array (float 0.)) msg))
    "same multi_line_string"
    (Ok
       [|
         [| [| 170.0; 45.0 |]; [| 180.0; 45.0 |] |];
         [| [| -180.0; 45.0 |]; [| -170.0; 45.0 |] |];
       |])
    t;
  Alcotest.(check (result ezjsonm msg)) "same json" (Ok json) json'

let () =
  Alcotest.run "geojson"
    [ ("geometry", [ Alcotest.test_case "multi-line" `Quick test_multi_line ]) ]
