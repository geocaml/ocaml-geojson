module type PARSER = sig
  type t

  val of_string : string -> (t, [`Msg of string]) result
  val of_channel : in_channel -> (t, [`Msg of string]) result

  val find : t -> string list -> t option

  val to_string : t -> (string, [`Msg of string]) result
  val to_float : t -> (float, [`Msg of string]) result
  val to_list : (t -> 'a) -> t -> ('a list, [`Msg of string]) result
end

module type GEOMETRY = sig
  module Position : sig
    type t
    (** A position - a longitude and latitude with an optional altitude *)
  
    val longitude : t -> float
    val latitude : t -> float
    val altitude : t -> float option
    val equal : t -> t -> bool
  end
  
  module Point : sig 
    type t
  
    val position : t -> Position.t
  end
  
  module MultiPoint : sig
      type t
  
      val coordinates : t -> Position.t array
  end
  
  module LineString : sig
      type t
      val coordinates : t -> Position.t array
  end
  
  module MultiLineString : sig
      type t
  
      val lines : t -> LineString.t array
  end
  
  module Polygon : sig
      type t
      val interior_ring : t -> LineString.t 
      val exterior_rings : t -> LineString.t array
  end
  
  module MultiPolygon : sig
      type t 
      val polygons : t -> Polygon.t array
  end
  
  module GeometryCollection : sig
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

module type GEOJSON = sig 
  type t
  (** A geojson object *)
end 