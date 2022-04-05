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

    let bbox json = 
      match J.to_array (decode_or_err J.to_float) json with
      | Ok v -> Some v
      | Error _ -> None
    let bbox_to_json_or_null bbox = Option.(if is_some bbox then J.array J.float (get bbox) else J.null)

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
      match (J.find json [ "type" ], J.find json [ "coordinates" ], J.find json [ "bbox" ]) with
      | None, _,_ ->
          Error
            (`Msg
              ("JSON should"
              ^ "have a key-value for `type' whilst parsing "
              ^ typ))
      | _, None,_ -> Error (`Msg "JSON should have a key-value for `coordinates'")
      | Some typ, Some coords, bx-> (
          let* typ = J.to_string typ in
          match typ with
          | t when t = typ -> (
            match p_c coords with 
            | Ok v -> Ok (v, Option.bind bx bbox)
            | Error e-> Error e
          )
          | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))
      
      

    module Point = struct
      type t = {
        coordinates: Position.t; 
        bbox: float array option
      }

      let typ = "Point"
      let position = Fun.id
      let v position = position
      let parse_coords coords = J.to_array (decode_or_err J.to_float) coords
      let of_json json = match parse_by_type json parse_coords typ with
      | Ok v -> Ok {coordinates = fst v; bbox = snd v}
      | Error e -> Error e
      let to_json tp =
        J.obj
          [ 
            ("type", J.string typ); 
            ("bbox", bbox_to_json_or_null tp.bbox);
            ("coordinates", Position.to_json tp.coordinates)
         ]
    end

    module MultiPoint = struct
      type t = {
        coordinates: Position.t array;
        bbox: float array option
      }

      let typ = "MultiPoint"
      let coordinates t = t.coordinates
      let v positions = { coordinates =  positions; bbox = None}

      let parse_coords coords =
        try J.to_array (decode_or_err Point.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let of_json json = match parse_by_type json parse_coords typ with
      | Ok v -> Ok {coordinates = fst v; bbox = snd v}
      | Error e -> Error e

      let to_json tp =
        J.obj
          [
            ("type", J.string typ);
            ("bbox", bbox_to_json_or_null tp.bbox);
            ("coordinates", J.array Position.to_json  tp.coordinates);
          ]
    end

    module LineString = struct
      type t = {
        coordinates: Position.t array;
        bbox: float array option
      }

      let typ = "LineString"
      let coordinates t = t.coordinates
      let v p = { coordinates= p; bbox=None}

      let parse_coords coords =
        let* arr =
          try MultiPoint.parse_coords coords with Failure m -> Error (`Msg m)
        in
        if Array.length arr < 2 then
          Error (`Msg "LineStrings should have two or more points")
        else Ok arr

      let of_json json = match parse_by_type json parse_coords typ with
      | Ok v -> Ok {coordinates = fst v; bbox = snd v}
      | Error e -> Error e

      let to_json tp =
        J.obj
          [
            ("type", J.string typ);
            ("bbox", bbox_to_json_or_null tp.bbox);
            ("coordinates", J.array Position.to_json tp.coordinates);
          ]
    end

    module MultiLineString = struct
      type t = {
        coordinates: Position.t array array;
        bbox: float array option
      }

      let typ = "MultiLineString"
      let lines t = t.coordinates
      let v p = { coordinates=p ; bbox=None}

      let parse_coords coords =
        try J.to_array (decode_or_err LineString.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let of_json json = match parse_by_type json parse_coords typ with
      | Ok v -> Ok {coordinates = fst v; bbox = snd v}
      | Error e -> Error e

      let to_json tp =
        J.obj
          [
            ("type", J.string typ);
            ("bbox", bbox_to_json_or_null tp.bbox);
            ("coordinates", J.array (J.array (J.array J.float)) tp.coordinates);
          ]
    end

    module Polygon = struct
      type t = {
        coordinates: Position.t array array;
        bbox: float array option
      }

      let typ = "Polygon"
      let interior_ring t = t.coordinates.(0)

      (* If used a lot, should changed to cstruct style off and len
         to avoid the allocations here. *)
      let exterior_rings t = Array.sub t.coordinates 1 (Array.length t.coordinates - 1)
      let v p = {coordinates = p; bbox=None}

      let parse_coords coords =
        try
          J.to_array
            (decode_or_err
               (J.to_array
                  (decode_or_err (J.to_array (decode_or_err J.to_float)))))
            coords
        with Failure m -> Error (`Msg m)

      let of_json json = match parse_by_type json parse_coords typ with
      | Ok v -> Ok {coordinates = fst v; bbox = snd v}
      | Error e -> Error e

      let to_json tp =
        J.obj
          [
            ("type", J.string typ);
            ("bbox", bbox_to_json_or_null tp.bbox);
            ("coordinates", J.array (J.array (J.array J.float)) tp.coordinates);
          ]
    end

    module MultiPolygon = struct
      type t = {
        coordinates: Position.t array array array; 
        bbox: float array option
      }
      let typ = "MultiPolygon"
      let polygons t = t.coordinates
      let v p = {coordinates = p; bbox= None}

      let parse_coords coords =
        try J.to_array (decode_or_err Polygon.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let of_json json = match parse_by_type json parse_coords typ with
      | Ok v -> Ok {coordinates = fst v; bbox = snd v}
      | Error e -> Error e

      let to_json tp =
        J.obj
          [
            ("type", J.string typ);
            ("bbox", bbox_to_json_or_null tp.bbox);
            ( "coordinates",
              J.array (J.array (J.array (J.array J.float))) tp.coordinates );
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

    type t = {
      bbox : float array option;
      geometry : Geometry.t option;
      properties : json option;
    } 

    let bbox json = 
      match J.to_array (decode_or_err J.to_float) json with
      | Ok v -> Some v
      | Error _ -> None
    let bbox_to_json_or_null bbox = Option.(if is_some bbox then J.array J.float (get bbox) else J.null)

    let geometry t = t.geometry

    let properties t = t.properties

    let of_json json =
      match J.find json [ "type" ] with
      | Some typ -> (
          match J.to_string typ with
          | Ok "Feature" -> (
              match
                (J.find json [ "bbox" ], J.find json [ "geometry" ], J.find json [ "properties" ])
              with
              | Some box, Some geometry, props ->(
                  Result.map
                    (fun v -> {bbox=bbox box;geometry=Option.some v; properties=props})
                    (Geometry.of_json geometry))
              | None, Some geometry, props ->
                  Result.map
                    (fun v -> {bbox=None;geometry=Option.some v; properties=props})
                    (Geometry.of_json geometry)
              | Some box, None, props -> Ok {bbox=bbox box;geometry=None;properties=props}
              | None, None, props -> Ok {bbox=None;geometry=None;properties=props})
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

    let to_json t =
      J.obj
        [
          ("type", J.string "Feature");
          ("bbox", bbox_to_json_or_null t.bbox);
          ("geometry", Option.(value ~default:J.null @@ map Geometry.to_json t.geometry));
          ("properties", Option.(value ~default:J.null t.properties));
        ]

    module Collection = struct
      type feature = t
      type nonrec t = {
        features: feature list;
        bbox: float array option
      }
      let features t = t.features

      let of_json json =
        match J.find json [ "type" ] with
        | Some typ -> (
            match J.to_string typ with
            | Ok "FeatureCollection" -> (
                match (J.find json [ "features" ], J.find json [ "bbox" ]) with
                | Some features, bbx ->(
                  let bbox = Option.map (decode_or_err @@ J.to_array (decode_or_err J.to_float)) bbx in
                    match (J.to_list
                      (fun geometry -> decode_or_err of_json geometry)
                      features) with 
                      | Ok v -> Ok {features= v; bbox= bbox}
                      | Error e -> Error e
                )
                | None, _ ->
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
            ("bbox", bbox_to_json_or_null t.bbox);
            ("features", J.list to_json t.features);
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

    type feature = {bbox: float array option; properties : json option; geometry : geometry }
    type r = FC of feature list | F of feature | G of geometry 

    let random ~f t =
      let rec aux_random = function
        | FC fs ->
            let features = List.map random_f fs in
            FeatureCollection {features= features; bbox=None}
        | F f -> Feature (random_f f)
        | G g -> Geometry (random_g g)
      and random_f p =
        let geo = random_g p.geometry in
        { bbox = p.bbox ; geometry = Some geo; properties=p.properties }
      and random_g = function
        | Point -> Geometry.Point {coordinates= (random_point ()); bbox = None}
        | MultiPoint i ->
            Geometry.MultiPoint {coordinates = (Array.init i (fun _ -> random_point ())); bbox = None}
        | LineString i ->
            Geometry.LineString {coordinates=(Array.init i (fun _ -> random_point ())); bbox=None}
        | MultiLineString (i, j) ->
            Geometry.MultiLineString
              {coordinates = (Array.init i @@ fun _ -> Array.init j (fun _ -> random_point ())); bbox=None}
        | Polygon i -> Geometry.Polygon {coordinates = (random_polygon i); bbox=None}
        | MultiPolygon (i, j) ->
            let arr = Array.init i (fun _ -> random_polygon j) in
            Geometry.MultiPolygon {coordinates = arr; bbox=None}
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
