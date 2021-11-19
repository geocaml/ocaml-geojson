module S = S


module Position = struct
  (* We use a float array internally for performance *)
  type t = float array 

  let longitude t = t.(0)
  let latitude t = t.(1)

  let altitude t =
    try Some t.(2) with _ -> None

  let equal l1 l2 =
    let n1 = Array.length l1
    and n2 = Array.length l2 in
    if n1 <> n2 then false
    else let rec loop i =
      if i = n1 then true
      else if Float.equal (Array.unsafe_get l1 i) (Array.unsafe_get l2 i) then loop (succ i)
      else false in
    loop 0
end

module Point = struct 
  type t = Position.t

  let position = Fun.id
end

module MultiPoint = struct
  type t = Position.t array
  let coordinates = Fun.id
end

module LineString = struct
  type t = Position.t array
  let coordinates = Fun.id
end

module MultiLineString = struct
  type t = LineString.t array
  let lines = Fun.id
end

module Polygon = struct 
  type t = LineString.t array

  let interior_ring t = t.(0)

  (* If used a lot, should changed to cstruct style off and len 
     to avoid the allocations here. *)
  let exterior_rings t = Array.sub t 1 (Array.length t - 1)
end

module MultiPolygon = struct
  type t = Polygon.t array
  let polygons = Fun.id
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

module Make (P : S.PARSER) = struct
  type t
end

module Ezjsonm_parser = struct
  type t = Ezjsonm.value

  let catch_err f v = 
    try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

  let of_string = catch_err Ezjsonm.value_from_string

  let of_channel = catch_err Ezjsonm.value_from_channel

  let find = Ezjsonm.find_opt

  let to_string t = 
    catch_err Ezjsonm.get_string t
  
  let to_float t = 
    catch_err Ezjsonm.get_float t

    let to_list f t = 
      catch_err (Ezjsonm.get_list f) t
end