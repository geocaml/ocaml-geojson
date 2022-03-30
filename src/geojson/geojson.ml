(* Copyright (c) 2021 Patrick Ferris <patrick@sirref.org>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
*)

module type S = Geojson_intf.S

let ( let* ) = Result.bind

let decode_or_err f v =
  match f v with Ok x -> x | Error (`Msg m) -> failwith m

module Make (J : Geojson_intf.Json) = struct
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

    (* Returns the float array of coordinates if all goes well for any GeoJson type *)
    let parse_by_type json p_c typ =
      match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
      | None, _ ->
          Error
            (`Msg
              ("JSON should"
              ^ "have a key-value for `type' whilst parsing "
              ^ typ))
      | _, None -> Error (`Msg "JSON should have a key-value for `coordinates'")
      | Some typ, Some coords -> (
          let* typ = J.to_string typ in
          match typ with
          | t when t = typ -> p_c coords
          | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

    module Point = struct
      type t = Position.t

      let typ = "Point"
      let position = Fun.id
      let v position = position
      let parse_coords coords = J.to_array (decode_or_err J.to_float) coords
      let of_json json = parse_by_type json parse_coords typ

      let to_json position =
        J.obj
          [ ("type", J.string typ); ("coordinates", Position.to_json position) ]
    end

    module MultiPoint = struct
      type t = Point.t array

      let typ = "MultiPoint"
      let coordinates = Fun.id
      let v positions = positions

      let parse_coords coords =
        try J.to_array (decode_or_err Point.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let of_json json = parse_by_type json parse_coords typ

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
      let v = Fun.id

      let parse_coords coords =
        let* arr =
          try MultiPoint.parse_coords coords with Failure m -> Error (`Msg m)
        in
        if Array.length arr < 2 then
          Error (`Msg "LineStrings should have two or more points")
        else Ok arr

      let of_json json = parse_by_type json parse_coords typ

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
      let v = Fun.id

      let parse_coords coords =
        try J.to_array (decode_or_err LineString.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let of_json json = parse_by_type json parse_coords typ

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
      let v = Fun.id

      let parse_coords coords =
        try
          J.to_array
            (decode_or_err
               (J.to_array
                  (decode_or_err (J.to_array (decode_or_err J.to_float)))))
            coords
        with Failure m -> Error (`Msg m)

      let of_json json = parse_by_type json parse_coords typ

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ("coordinates", J.array (J.array (J.array J.float)) positions);
          ]
    end

    module MultiPolygon = struct
      type t = Polygon.t array

      let typ = "MultiPolygon"
      let polygons = Fun.id
      let v = Fun.id

      let parse_coords coords =
        try J.to_array (decode_or_err Polygon.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let of_json json = parse_by_type json parse_coords typ

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ( "coordinates",
              J.array (J.array (J.array (J.array J.float))) positions );
          ]
    end

    type t =
      | Point of Point.t
      | MultiPoint of MultiPoint.t
      | LineString of LineString.t
      | MultiLineString of MultiLineString.t
      | Polygon of Polygon.t
      | MultiPolygon of MultiPolygon.t
      | Collection of t list

    let rec of_json json =
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
          | Ok "GeometryCollection" -> (
              match J.find json [ "geometries" ] with
              | Some list ->
                  let geo = J.to_list (decode_or_err of_json) list in
                  Result.map (fun v -> Collection v) geo
              | None ->
                  Error
                    (`Msg
                      "A geometry collection should have a member called \
                       geometries"))
          | Ok typ -> Error (`Msg ("Unknown type of geometry " ^ typ))
          | Error _ as e -> e)
      | None ->
          Error
            (`Msg
              "A Geojson text should contain one object with a member `type`.")

    let rec to_json = function
      | Point point -> Point.to_json point
      | MultiPoint mp -> MultiPoint.to_json mp
      | LineString ls -> LineString.to_json ls
      | MultiLineString mls -> MultiLineString.to_json mls
      | Polygon p -> Polygon.to_json p
      | MultiPolygon mp -> MultiPolygon.to_json mp
      | Collection c ->
          J.obj
            [
              ("type", J.string "GeometryCollection");
              ("geometries", J.list to_json c);
            ]
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
      type nonrec t = feature list

      let features = Fun.id

      let of_json json =
        match J.find json [ "type" ] with
        | Some typ -> (
            match J.to_string typ with
            | Ok "FeatureCollection" -> (
                match J.find json [ "features" ] with
                | Some features ->
                    J.to_list
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
            ("features", J.list to_json t);
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

  module Random = struct
    type geometry =
      | Point
      | MultiPoint of int
      | LineString of int
      | MultiLineString of int * int
      | Polygon of int
      | MultiPolygon of int * int
      | Collection of geometry list

    type feature = { properties : json option; geometry : geometry }
    type r = FC of feature list | F of feature | G of geometry

    let random ~f t =
      let rec aux_random = function
        | FC fs ->
            let features = List.map random_f fs in
            FeatureCollection features
        | F f -> Feature (random_f f)
        | G g -> Geometry (random_g g)
      and random_f { properties; geometry } =
        let geo = random_g geometry in
        (Some geo, properties)
      and random_g = function
        | Point -> Geometry.Point (random_point ())
        | MultiPoint i ->
            Geometry.MultiPoint (Array.init i (fun _ -> random_point ()))
        | LineString i ->
            Geometry.LineString (Array.init i (fun _ -> random_point ()))
        | MultiLineString (i, j) ->
            Geometry.MultiLineString
              (Array.init i @@ fun _ -> Array.init j (fun _ -> random_point ()))
        | Polygon i -> Geometry.Polygon (random_polygon i)
        | MultiPolygon (i, j) ->
            let arr = Array.init i (fun _ -> random_polygon j) in
            Geometry.MultiPolygon arr
        | Collection lst ->
            let lst = List.map random_g lst in
            Geometry.Collection lst
      and random_point () =
        Geometry.(Point.v (Position.v ~lat:(f ()) ~long:(f ()) ()))
      and random_polygon i =
        (* This geometry is not going to be very country like... *)
        let points = Array.init i (fun _ -> random_point ()) in
        points.(i - 1) <- points.(0);
        [| points |]
      in
      aux_random t
  end
end
