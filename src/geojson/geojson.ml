module S = S

let ( let* ) = Result.bind

let decode_or_err f v =
  match f v with Ok x -> x | Error (`Msg m) -> failwith m

module Make (J : S.JSON) = struct
  type json = J.t
  type t = unit

  let of_json _json = Ok ()

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

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ -> Error (`Msg "JSON should have a key-value for `type'")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> J.to_array (decode_or_err J.to_float) coords
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

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ -> Error (`Msg "JSON should have a key-value for `type'")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> (
                try
                  J.to_array
                    (decode_or_err (J.to_array (decode_or_err J.to_float)))
                    coords
                with Failure m -> Error (`Msg m))
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

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ -> Error (`Msg "JSON should have a key-value for `type'")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ ->
                let* arr =
                  try
                    J.to_array
                      (decode_or_err (J.to_array (decode_or_err J.to_float)))
                      coords
                  with Failure m -> Error (`Msg m)
                in
                if Array.length arr < 2 then
                  Error (`Msg "LineStrings should have two or more points")
                else Ok arr
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

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ -> Error (`Msg "JSON should have a key-value for `type'")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> (
                try
                  J.to_array
                    (decode_or_err
                       (J.to_array
                          (decode_or_err
                             (J.to_array (decode_or_err J.to_float)))))
                    coords
                with Failure m -> Error (`Msg m))
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

      let of_json json =
        match (J.find json [ "type" ], J.find json [ "coordinates" ]) with
        | None, _ -> Error (`Msg "JSON should have a key-value for `type'")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> (
                try J.to_array (decode_or_err LineString.of_json) coords
                with Failure m -> Error (`Msg m))
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
        | None, _ -> Error (`Msg "JSON should have a key-value for `type'")
        | _, None ->
            Error (`Msg "JSON should have a key-value for `coordinates'")
        | Some typ, Some coords -> (
            let* typ = J.to_string typ in
            match typ with
            | t when t = typ -> (
                try J.to_array (decode_or_err Polygon.of_json) coords
                with Failure m -> Error (`Msg m))
            | t -> Error (`Msg ("Expected type of `" ^ typ ^ "' but got " ^ t)))

      let to_json positions =
        J.obj
          [
            ("type", J.string typ);
            ("coordinates", J.array Polygon.to_json positions);
          ]
    end

    module GeometryCollection = struct
      type elt =
        | Point of Point.t
        | MultiPoint of MultiPoint.t
        | LineString of LineString.t
        | MultiLineString of MultiLineString.t
        | Polygon of Polygon.t
        | MultiPolygon of MultiPolygon.t
        | Collection of elt

      type t = elt list
    end
  end
end
