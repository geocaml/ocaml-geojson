(* Copyright (c) 2021-2022 Patrick Ferris <patrick@sirref.org>

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
module Intf = Geojson_intf

module type S = Geojson_intf.S
module type Json = Geojson_intf.Json

let decode_or_err f v =
  match f v with Ok x -> x | Error (`Msg m) -> failwith m

module Make (J : Intf.Json) = struct
  type json = J.t

  let bbox_to_json_or_empty bbox =
    Option.(
      if is_some bbox then [ ("bbox", J.array J.float (get bbox)) ] else [])

  module Geometry = struct
    type json = J.t

    let keys_in_use = [ "type"; "coordinates"; "bbox" ]

    let foreign_members json =
      match J.to_obj json with
      | Ok assoc ->
          List.filter (fun (k, _v) -> not (List.mem k keys_in_use)) assoc
      | Error _ -> []

    module Position = struct
      (* We use a float array internally for performance *)
      type t = float array

      let lng t = t.(0)
      let lat t = t.(1)
      let altitude t = try Some t.(2) with _ -> None

      let v ?altitude ~lng ~lat () =
        match altitude with
        | Some f -> [| lng; lat; f |]
        | None -> [| lng; lat |]

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

      let to_json arr = J.array J.float arr
    end

    (* Returns the float array of coordinates if all goes well for any Geometry type *)
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
          Result.bind (J.to_string typ) @@ fun typ ->
          match typ with
          | t when t = typ -> p_c coords
          | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

    module Point = struct
      type t = Position.t

      let typ = "Point"
      let position = Fun.id
      let v position = position
      let parse_coords coords = J.to_array (decode_or_err J.to_float) coords
      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) position =
        J.obj
          ([
             ("type", J.string typ); ("coordinates", Position.to_json position);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module MultiPoint = struct
      type t = Point.t array

      let typ = "MultiPoint"
      let coordinates = Fun.id
      let v positions = positions

      let parse_coords coords =
        try J.to_array (decode_or_err Point.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) positions =
        J.obj
          ([
             ("type", J.string typ);
             ("coordinates", J.array Position.to_json positions);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module LineString = struct
      type t = Position.t array

      let typ = "LineString"
      let coordinates = Fun.id
      let v = Fun.id

      let parse_coords coords =
        Result.bind
          (try MultiPoint.parse_coords coords with Failure m -> Error (`Msg m))
        @@ fun arr ->
        if Array.length arr < 2 then
          Error (`Msg "LineStrings should have two or more points")
        else Ok arr

      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) positions =
        J.obj
          ([
             ("type", J.string typ);
             ("coordinates", J.array Position.to_json positions);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module MultiLineString = struct
      type t = LineString.t array

      let typ = "MultiLineString"
      let lines = Fun.id
      let v = Fun.id
      let to_positions = Fun.id
      let of_positions = Fun.id

      let parse_coords coords =
        try J.to_array (decode_or_err LineString.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) positions =
        J.obj
          ([
             ("type", J.string typ);
             ("coordinates", J.array (J.array (J.array J.float)) positions);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module Polygon = struct
      type t = LineString.t array

      let typ = "Polygon"
      let rings = Fun.id
      let exterior_ring t = t.(0)

      (* If used a lot, should changed to cstruct style off and len
         to avoid the allocations here. *)
      let interior_rings t = Array.sub t 1 (Array.length t - 1)
      let v = Fun.id
      let to_positions = Fun.id
      let of_positions = Fun.id

      let parse_coords coords =
        try
          J.to_array
            (decode_or_err
               (J.to_array
                  (decode_or_err (J.to_array (decode_or_err J.to_float)))))
            coords
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) positions =
        J.obj
          ([
             ("type", J.string typ);
             ("coordinates", J.array (J.array (J.array J.float)) positions);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    module MultiPolygon = struct
      type t = Polygon.t array

      let typ = "MultiPolygon"
      let polygons = Fun.id
      let v = Fun.id
      let to_positions = Fun.id
      let of_positions = Fun.id

      let parse_coords coords =
        try J.to_array (decode_or_err Polygon.parse_coords) coords
        with Failure m -> Error (`Msg m)

      let base_of_json json = parse_by_type json parse_coords typ

      let to_json ?bbox ?(foreign_members = []) positions =
        J.obj
          ([
             ("type", J.string typ);
             ( "coordinates",
               J.array (J.array (J.array (J.array J.float))) positions );
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)
    end

    type geometry =
      | Point of Point.t
      | MultiPoint of MultiPoint.t
      | LineString of LineString.t
      | MultiLineString of MultiLineString.t
      | Polygon of Polygon.t
      | MultiPolygon of MultiPolygon.t
      | Collection of t list

    and t = geometry * (string * json) list

    let rec base_of_json json =
      let fm = foreign_members json in
      match J.find json [ "type" ] with
      | Some typ -> (
          match J.to_string typ with
          | Ok "Point" ->
              Result.map (fun v -> (Point v, fm)) @@ Point.base_of_json json
          | Ok "MultiPoint" ->
              Result.map (fun v -> (MultiPoint v, fm))
              @@ MultiPoint.base_of_json json
          | Ok "LineString" ->
              Result.map (fun v -> (LineString v, fm))
              @@ LineString.base_of_json json
          | Ok "MultiLineString" ->
              Result.map (fun v -> (MultiLineString v, fm))
              @@ MultiLineString.base_of_json json
          | Ok "Polygon" ->
              Result.map (fun v -> (Polygon v, fm)) @@ Polygon.base_of_json json
          | Ok "MultiPolygon" ->
              Result.map (fun v -> (MultiPolygon v, fm))
              @@ MultiPolygon.base_of_json json
          | Ok "GeometryCollection" -> (
              match J.find json [ "geometries" ] with
              | Some list ->
                  let geo = J.to_list (decode_or_err base_of_json) list in
                  Result.map (fun v -> (Collection v, fm)) geo
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

    let rec to_json ?bbox = function
      | Point point, foreign_members ->
          Point.to_json ?bbox ~foreign_members point
      | MultiPoint mp, foreign_members ->
          MultiPoint.to_json ?bbox ~foreign_members mp
      | LineString ls, foreign_members ->
          LineString.to_json ?bbox ~foreign_members ls
      | MultiLineString mls, foreign_members ->
          MultiLineString.to_json ?bbox ~foreign_members mls
      | Polygon p, foreign_members -> Polygon.to_json ?bbox ~foreign_members p
      | MultiPolygon mp, foreign_members ->
          MultiPolygon.to_json ?bbox ~foreign_members mp
      | Collection c, foreign_members ->
          J.obj
            ([
               ("type", J.string "GeometryCollection");
               ("geometries", J.list to_json c);
             ]
            @ bbox_to_json_or_empty bbox
            @ foreign_members)

    let foreign_members (_, fm) = fm
    let geometry (g, _) = g
    let v ?(foreign_members = []) g = (g, foreign_members)
  end

  module Feature = struct
    type t = {
      geometry : Geometry.t option;
      properties : json option;
      foreign_members : (string * json) list;
      id : [ `String of string | `Float of float ] option;
    }

    let v ?id ?properties ?(foreign_members = []) geo =
      { geometry = Some geo; properties; foreign_members; id }

    let geometry t = t.geometry
    let properties t = t.properties
    let keys_in_use = [ "type"; "geometry"; "properties"; "id"; "bbox" ]

    let foreign_members json =
      match J.to_obj json with
      | Ok assoc ->
          List.filter (fun (k, _v) -> not (List.mem k keys_in_use)) assoc
      | Error _ -> []

    let id_of_json = function
      | Some json -> (
          match J.to_string json with
          | Ok s -> Ok (Some (`String s))
          | _ -> (
              match J.to_float json with
              | Ok f -> Ok (Some (`Float f))
              | _ -> Error (`Msg "Identifier is not a string or number")))
      | None -> Ok None

    let id_to_json = function `String s -> J.string s | `Float f -> J.float f

    let base_of_json json =
      match J.find json [ "type" ] with
      | Some typ -> (
          match J.to_string typ with
          | Ok "Feature" -> (
              let fm = foreign_members json in
              match
                ( J.find json [ "geometry" ],
                  J.find json [ "properties" ],
                  J.find json [ "id" ] )
              with
              | Some geometry, properties, id ->
                  Result.bind (id_of_json id) (fun id ->
                      Result.map
                        (fun v ->
                          {
                            geometry = Option.some v;
                            properties;
                            foreign_members = fm;
                            id;
                          })
                        (Geometry.base_of_json geometry))
              | None, properties, id ->
                  Result.map
                    (fun id ->
                      { geometry = None; properties; foreign_members = fm; id })
                    (id_of_json id))
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

    let to_json ?bbox { geometry; properties; foreign_members; id } =
      J.obj
        ([ ("type", J.string "Feature") ]
        @ (match geometry with
          | Some p -> [ ("geometry", Geometry.to_json p) ]
          | None -> [])
        @ (match properties with Some p -> [ ("properties", p) ] | None -> [])
        @ (match id with Some s -> [ ("id", id_to_json s) ] | None -> [])
        @ bbox_to_json_or_empty bbox
        @ foreign_members)

    let foreign_members t = t.foreign_members
    let id t = t.id

    module Collection = struct
      type feature = t

      type nonrec t = {
        features : feature list;
        foreign_members : (string * json) list;
      }

      let features t = t.features
      let v ?(foreign_members = []) features = { features; foreign_members }

      let keys_in_use =
        [ "type"; "features"; "geometry"; "properties"; "id"; "bbox" ]

      let foreign_members json =
        match J.to_obj json with
        | Ok assoc ->
            List.filter (fun (k, _v) -> not (List.mem k keys_in_use)) assoc
        | Error _ -> []

      let base_of_json json =
        match J.find json [ "type" ] with
        | Some typ -> (
            match J.to_string typ with
            | Ok "FeatureCollection" -> (
                let fm = foreign_members json in
                match J.find json [ "features" ] with
                | Some features ->
                    let features =
                      J.to_list
                        (fun geometry -> decode_or_err base_of_json geometry)
                        features
                    in
                    Result.map
                      (fun v -> { features = v; foreign_members = fm })
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

      let to_json ?bbox { features; foreign_members } =
        J.obj
          ([
             ("type", J.string "FeatureCollection");
             ("features", J.list to_json features);
           ]
          @ bbox_to_json_or_empty bbox
          @ foreign_members)

      let foreign_members t = t.foreign_members
    end
  end

  type geojson =
    | Feature of Feature.t
    | FeatureCollection of Feature.Collection.t
    | Geometry of Geometry.t

  and t = { geojson : geojson; bbox : float array option }

  let geojson t = t.geojson
  let bbox t = t.bbox
  let v ?bbox geojson = { geojson; bbox }
  let geojson_to_t gjson bbox = { geojson = gjson; bbox }

  let json_to_bbox json =
    match J.to_array (decode_or_err J.to_float) json with
    | Ok v -> Some v
    | Error _ -> None

  let of_json json =
    match (J.find json [ "type" ], J.find json [ "bbox" ]) with
    | Some typ, bbx -> (
        match J.to_string typ with
        | Ok "Feature" -> (
            match Feature.base_of_json json with
            | Ok v ->
                Ok (geojson_to_t (Feature v) @@ Option.bind bbx json_to_bbox)
            | Error e -> Error e)
        | Ok "FeatureCollection" -> (
            match Feature.Collection.base_of_json json with
            | Ok v ->
                Ok
                  (geojson_to_t (FeatureCollection v)
                  @@ Option.bind bbx json_to_bbox)
            | Error e -> Error e)
        | Ok _maybe_geometry -> (
            match Geometry.base_of_json json with
            | Ok v ->
                Ok (geojson_to_t (Geometry v) @@ Option.bind bbx json_to_bbox)
            | Error e -> Error e)
        | Error _ as e -> e)
    | None, _ ->
        Error
          (`Msg
            "A Geojson text should contain one object with a member `type`.")

  let to_json = function
    | { geojson = Feature f; bbox } -> Feature.to_json ?bbox f
    | { geojson = FeatureCollection fc; bbox } ->
        Feature.Collection.to_json ?bbox fc
    | { geojson = Geometry g; bbox } -> Geometry.to_json ?bbox g

  module Accessor = struct
    module Optics = Optics
    include Optics.Infix

    let get = Optics.Lens.get

    let geojson =
      Optics.Lens.V
        ((fun t -> (t.geojson, t)), fun (geojson, t) -> { t with geojson })

    let bbox =
      Optics.Lens.V ((fun t -> (t.bbox, t)), fun (bbox, t) -> { t with bbox })

    let feature =
      let into = function
        | Feature f -> Ok f
        | v ->
            ignore (failwith "Big yikes");
            Error v
      in
      let out_of = function Ok f -> Feature f | Error v -> v in
      Optics.Prism.V (into, out_of)

    let feature_collection =
      let into = function FeatureCollection f -> Ok f | v -> Error v in
      let out_of = function Ok f -> FeatureCollection f | Error v -> v in
      Optics.Prism.V (into, out_of)

    let geometry =
      let into = function Geometry f -> Ok f | v -> Error v in
      let out_of = function Ok f -> Geometry f | Error v -> v in
      Optics.Prism.V (into, out_of)

    module Feature = struct
      let properties =
        Optics.Lens.V
          ( (fun t -> (t.Feature.properties, t)),
            fun (properties, t) -> { t with properties } )

      let foreign_members =
        Optics.Lens.V
          ( (fun t -> (t.Feature.foreign_members, t)),
            fun (foreign_members, t) -> { t with foreign_members } )

      let geometry =
        Optics.Lens.V
          ( (fun t -> (t.Feature.geometry, t)),
            fun (geometry, t) -> { t with geometry } )

      let geometry_exn =
        Optics.Lens.V
          ( (fun t -> (Option.get t.Feature.geometry, t)),
            fun (geometry, t) -> { t with geometry = Some geometry } )
    end

    module Geometry = struct
      let geometry : (Geometry.t, Geometry.geometry) Optics.Lens.t =
        Optics.Lens.fst

      let foreign_members : (Geometry.t, (string * json) list) Optics.Lens.t =
        Optics.Lens.snd

      let point : (Geometry.geometry, Geometry.Point.t) Optics.Prism.t =
        let into = function Geometry.Point f -> Ok f | v -> Error v in
        let out_of = function Ok f -> Geometry.Point f | Error v -> v in
        Optics.Prism.V (into, out_of)

      let multipoint : (Geometry.geometry, Geometry.MultiPoint.t) Optics.Prism.t
          =
        let into = function Geometry.MultiPoint f -> Ok f | v -> Error v in
        let out_of = function Ok f -> Geometry.MultiPoint f | Error v -> v in
        Optics.Prism.V (into, out_of)

      let linestring : (Geometry.geometry, Geometry.LineString.t) Optics.Prism.t
          =
        let into = function Geometry.LineString f -> Ok f | v -> Error v in
        let out_of = function Ok f -> Geometry.LineString f | Error v -> v in
        Optics.Prism.V (into, out_of)

      let multilinestring :
          (Geometry.geometry, Geometry.MultiLineString.t) Optics.Prism.t =
        let into = function
          | Geometry.MultiLineString f -> Ok f
          | v -> Error v
        in
        let out_of = function
          | Ok f -> Geometry.MultiLineString f
          | Error v -> v
        in
        Optics.Prism.V (into, out_of)

      let polygon : (Geometry.geometry, Geometry.Polygon.t) Optics.Prism.t =
        let into = function Geometry.Polygon f -> Ok f | v -> Error v in
        let out_of = function Ok f -> Geometry.Polygon f | Error v -> v in
        Optics.Prism.V (into, out_of)

      let multipolygon :
          (Geometry.geometry, Geometry.MultiPolygon.t) Optics.Prism.t =
        let into = function Geometry.MultiPolygon f -> Ok f | v -> Error v in
        let out_of = function
          | Ok f -> Geometry.MultiPolygon f
          | Error v -> v
        in
        Optics.Prism.V (into, out_of)
    end
  end

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
            {
              geojson = FeatureCollection { features; foreign_members = [] };
              bbox = None;
            }
        | F f -> { geojson = Feature (random_f f); bbox = None }
        | G g -> { geojson = Geometry (random_g g); bbox = None }
      and random_f { properties; geometry } =
        let geo = random_g geometry in
        { geometry = Some geo; properties; foreign_members = []; id = None }
      and random_g = function
        | Point -> (Geometry.Point (random_point ()), [])
        | MultiPoint i ->
            (Geometry.MultiPoint (Array.init i (fun _ -> random_point ())), [])
        | LineString i ->
            (Geometry.LineString (Array.init i (fun _ -> random_point ())), [])
        | MultiLineString (i, j) ->
            ( Geometry.MultiLineString
                ( Array.init i @@ fun _ ->
                  Array.init j (fun _ -> random_point ()) ),
              [] )
        | Polygon i -> (Geometry.Polygon (random_polygon i), [])
        | MultiPolygon (i, j) ->
            let arr = Array.init i (fun _ -> random_polygon j) in
            (Geometry.MultiPolygon arr, [])
        | Collection lst ->
            let lst = List.map random_g lst in
            (Geometry.Collection lst, [])
      and random_point () =
        Geometry.(Point.v (Position.v ~lat:(f ()) ~lng:(f ()) ()))
      and random_polygon i =
        (* This geometry is not going to be very country like... *)
        let points = Array.init i (fun _ -> random_point ()) in
        points.(i - 1) <- points.(0);
        [| points |]
      in
      aux_random t
  end
end
