module S = S

let ( let* ) = Result.bind

let decode_or_err f v =
  match f v with Ok x -> x | Error (`Msg m) -> failwith m

module Make (J : S.JSON) = struct
  type json = J.t

  module Geometry = struct
    type json = J.t

    module Position = struct
      (* We use a float array internally for performance *)
      type t = float array

      let long t = t.(0)
      let lat t = t.(1)
      let altitude t = try Some t.(2) with _ -> None

      let v ?altitude ~long ~lat () =
        match altitude with
        | Some f -> [| long; lat; f |]
        | None -> [| long; lat |]

      let equal l1 l2 =
        let n1 = Array.length l1 and n2 = Array.length l2 in
        if n1 <> n2 then false
        else
          let rec loop i =
            if i = n1 then true
            else if Float.equal (Array.unsafe_get l1 i) (Array.unsafe_get l2 i)
            then loop (succ i)
            else false
          in
          loop 0

      let of_json t =
        try J.to_array (decode_or_err J.to_float) t
        with Failure m -> Error (`Msg m)

      let to_json arr = J.array J.float arr
    end

    module Point = struct
      type t = Position.t

      let typ = "Point"
      let position = Fun.id
      let v position = position
      let parse_coords coords = J.to_array (decode_or_err J.to_float) coords

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ ->
            Error
              (`Msg
                "JSON should have a key-value for `type' whilst parsing Point")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> parse_coords coords
            | t -> Error (`Msg ("Expected type of `Point' but got " ^ t)))

      let to_json position =
        J.obj
          [ ("type", J.string typ); ("coordinates", Position.to_json position) ]
    end

    module MultiPoint = struct
      type t = Position.t array

      let typ = "MultiPoint"
      let coordinates = Fun.id
      let v positions = positions

      let parse_coords coords =
        J.to_array (decode_or_err Point.parse_coords) coords

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ ->
            Error
              (`Msg
                "JSON should have a key-value for `type' whilst parsing \
                 MultiPoint")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> (
                try parse_coords coords with Failure m -> Error (`Msg m))
            | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ("coordinates", J.array Position.to_json positions);
          ]
    end

    module LineString = struct
      type t = Position.t array

      let typ = "LineString"
      let coordinates = Fun.id

      let parse_coords coords =
        let* arr =
          try MultiPoint.parse_coords coords with Failure m -> Error (`Msg m)
        in
        if Array.length arr < 2 then
          Error (`Msg "LineStrings should have two or more points")
        else Ok arr

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ ->
            Error
              (`Msg
                "JSON should have a key-value for `type' whilst parsing \
                 LineString")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> parse_coords coords
            | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ("coordinates", J.array Position.to_json positions);
          ]
    end

    module MultiLineString = struct
      type t = LineString.t array

      let typ = "MultiLineString"
      let lines = Fun.id

      let parse_coords coords =
        J.to_array (decode_or_err LineString.parse_coords) coords

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ ->
            Error
              (`Msg
                "JSON should have a key-value for `type' whilst parsing \
                 MultiLineString")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> (
                try parse_coords coords with Failure m -> Error (`Msg m))
            | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ("coordinates", J.array (J.array (J.array J.float)) positions);
          ]
    end

    module Polygon = struct
      type t = LineString.t array

      let typ = "Polygon"
      let interior_ring t = t.(0)

      (* If used a lot, should changed to cstruct style off and len
         to avoid the allocations here. *)
      let exterior_rings t = Array.sub t 1 (Array.length t - 1)

      let parse_coords coords =
        try
          J.to_array
            (decode_or_err
               (J.to_array
                  (decode_or_err (J.to_array (decode_or_err J.to_float)))))
            coords
        with Failure m -> Error (`Msg m)

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ ->
            Error
              (`Msg
                "JSON should have a key-value for `type' whilst parsing Polygon")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> parse_coords coords
            | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ("coordinates", J.array LineString.to_json positions);
          ]
    end

    module MultiPolygon = struct
      type t = Polygon.t array

      let typ = "MultiPolygon"
      let polygons = Fun.id

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ ->
            Error
              (`Msg
                "JSON should have a key-value for `type' whilst parsing \
                 MultiPolygon")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> (
                try J.to_array (decode_or_err Polygon.parse_coords) coords
                with Failure m -> Error (`Msg m))
            | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ("coordinates", J.array Polygon.to_json positions);
          ]
    end

    module Collection = struct
      type elt =
        | Point of Point.t
        | MultiPoint of MultiPoint.t
        | LineString of LineString.t
        | MultiLineString of MultiLineString.t
        | Polygon of Polygon.t
        | MultiPolygon of MultiPolygon.t
        | Collection of elt

      type t = elt list

      let of_json _ = assert false
      let to_json _ = assert false
    end

    type t =
      | Point of Point.t
      | MultiPoint of MultiPoint.t
      | LineString of LineString.t
      | MultiLineString of MultiLineString.t
      | Polygon of Polygon.t
      | MultiPolygon of MultiPolygon.t
      | Collection of Collection.t

    let of_json json =
      match J.find json [ "type" ] with
      | Some typ -> (
          match J.to_string typ with
          | Ok "Point" -> Result.map (fun v -> Point v) @@ Point.of_json json
          | Ok "MultiPoint" ->
              Result.map (fun v -> MultiPoint v) @@ MultiPoint.of_json json
          | Ok "LineString" ->
              Result.map (fun v -> LineString v) @@ LineString.of_json json
          | Ok "MultiLineString" ->
              Result.map (fun v -> MultiLineString v)
              @@ MultiLineString.of_json json
          | Ok "Polygon" ->
              Result.map (fun v -> Polygon v) @@ Polygon.of_json json
          | Ok "MultiPolygon" ->
              Result.map (fun v -> MultiPolygon v) @@ MultiPolygon.of_json json
          | Ok "GeometryCollection" ->
              Result.map (fun v -> Collection v) @@ Collection.of_json json
          | Ok typ -> Error (`Msg ("Unknown type of geometry " ^ typ))
          | Error _ as e -> e)
      | None ->
          Error
            (`Msg
              "A Geojson text should contain one object with a member `type`.")

    let to_json = function
      | Point point -> Point.to_json point
      | MultiPoint mp -> MultiPoint.to_json mp
      | LineString ls -> LineString.to_json ls
      | MultiLineString mls -> MultiLineString.to_json mls
      | Polygon p -> Polygon.to_json p
      | MultiPolygon mp -> MultiPolygon.to_json mp
      | Collection c -> Collection.to_json c
  end

  module Feature = struct
    type t = Geometry.t option * json option

    let geometry = fst
    let properties = snd

    let of_json json =
      match J.find json [ "type" ] with
      | Some typ -> (
          match J.to_string typ with
          | Ok "Feature" -> (
              match
                (J.find json [ "geometry" ], J.find json [ "properties" ])
              with
              | Some geometry, props ->
                  Result.map
                    (fun v -> (Option.some v, props))
                    (Geometry.of_json geometry)
              | None, props -> Ok (None, props))
          | Ok s ->
              Error
                (`Msg
                  ("A Geojson feature requires the type `Feature`. Found type, \
                    but it was "
                  ^ s))
          | Error _ as e -> e)
      | None ->
          Error
            (`Msg
              "A Geojson feature requires the type `Feature`. No type was \
               found.")

    let to_json (t, props) =
      J.obj
        [
          ("type", J.string "Feature");
          ("geometry", Option.(value ~default:J.null @@ map Geometry.to_json t));
          ("properties", Option.(value ~default:J.null props));
        ]

    module Collection = struct
      type feature = t
      type nonrec t = feature array

      let features = Fun.id

      let of_json json =
        match J.find json [ "type" ] with
        | Some typ -> (
            match J.to_string typ with
            | Ok "FeatureCollection" -> (
                match J.find json [ "features" ] with
                | Some features ->
                    J.to_array
                      (fun geometry -> decode_or_err of_json geometry)
                      features
                | None ->
                    Error
                      (`Msg
                        "A feature collection should have a member called \
                         `features`."))
            | Ok s ->
                Error
                  (`Msg
                    ("A Geojson feature collection requires the type \
                      `FeatureCollection`. Found type, but it was "
                    ^ s))
            | Error _ as e -> e)
        | None ->
            Error
              (`Msg
                "A Geojson feature collection requires the type \
                 `FeatureCollection`. No type was found.")

      let to_json t =
        J.obj
          [
            ("type", J.string "FeatureCollection");
            ("features", J.array to_json t);
          ]
    end
  end

  type t =
    | Feature of Feature.t
    | FeatureCollection of Feature.Collection.t
    | Geometry of Geometry.t

  let of_json json =
    match J.find json [ "type" ] with
    | Some typ -> (
        match J.to_string typ with
        | Ok "Feature" ->
            Result.map (fun v -> Feature v) @@ Feature.of_json json
        | Ok "FeatureCollection" ->
            Result.map (fun v -> FeatureCollection v)
            @@ Feature.Collection.of_json json
        | Ok _maybe_geometry ->
            Result.map (fun v -> Geometry v) @@ Geometry.of_json json
        | Error _ as e -> e)
    | None ->
        Error
          (`Msg
            "A Geojson text should contain one object with a member `type`.")

  let to_json = function
    | Feature f -> Feature.to_json f
    | FeatureCollection fc -> Feature.Collection.to_json fc
    | Geometry g -> Geometry.to_json g
end
