module type JSON = sig
  type t
  (** The type your parser uses to represent a parsed JSON object. *)

  val find : t -> string list -> t option

  val to_string : t -> (string, [ `Msg of string ]) result
  (** Convert the JSON to a string. *)

  val string : string -> t
  (** Create a JSON string. *)

  val to_float : t -> (float, [ `Msg of string ]) result
  (** Convert the JSON to a float. *)

  val float : float -> t
  (** Converts a float to JSON *)

  val to_list : (t -> 'a) -> t -> ('a list, [ `Msg of string ]) result
  (** [to_list f] converts the JSON array to a list and applies [f] to each
      element to convert them too. *)

  val list : ('a -> t) -> 'a list -> t
  (** Make a JSON array from a list *)

  val to_array : (t -> 'a) -> t -> ('a array, [ `Msg of string ]) result
  (** Like {!to_list} except to an array. *)

  val array : ('a -> t) -> 'a array -> t
  (** Like {!list} except for OCaml arrays *)

  val obj : (string * t) list -> t
  (** A JSON object from an association list *)

  val null : t
  (** Null value *)

  val is_null : t -> bool
  (** Test for null *)
end

module type JSON_CONV = sig
  type t
  type json

  val of_json : json -> (t, [ `Msg of string ]) result
  val to_json : t -> json
end

module type GEOMETRY = sig
  type json

  module Position : sig
    type t
    (** A position - a longitude and latitude with an optional altitude *)

    val long : t -> float
    (** The longitude value of the position *)

    val lat : t -> float
    (** The latitude value of the position *)

    val altitude : t -> float option
    (** Optional altitude/elevation value of the position *)

    val equal : t -> t -> bool
    (** Whether two positions are equal by comparing each value *)

    val v : ?altitude:float -> long:float -> lat:float -> unit -> t
    (** A position constructor *)

    include JSON_CONV with type t := t and type json := json
  end

  module Point : sig
    type t

    val position : t -> Position.t
    val v : Position.t -> t

    include JSON_CONV with type t := t and type json := json
  end

  module MultiPoint : sig
    type t

    val coordinates : t -> Position.t array
    val v : Position.t array -> t

    include JSON_CONV with type t := t and type json := json
  end

  module LineString : sig
    type t

    val coordinates : t -> Position.t array

    include JSON_CONV with type t := t and type json := json
  end

  module MultiLineString : sig
    type t

    val lines : t -> LineString.t array

    include JSON_CONV with type t := t and type json := json
  end

  module Polygon : sig
    type t

    val interior_ring : t -> LineString.t
    val exterior_rings : t -> LineString.t array

    include JSON_CONV with type t := t and type json := json
  end

  module MultiPolygon : sig
    type t

    val polygons : t -> Polygon.t array

    include JSON_CONV with type t := t and type json := json
  end

  module Collection : sig
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

  type t =
    | Point of Point.t
    | MultiPoint of MultiPoint.t
    | LineString of LineString.t
    | MultiLineString of MultiLineString.t
    | Polygon of Polygon.t
    | MultiPolygon of MultiPolygon.t
    | Collection of Collection.t
end

module type GEOJSON = sig
  type json
  (** The internal representation of JSON *)

  module Geometry : GEOMETRY with type json = json

  module Feature : sig
    type t

    val geometry : t -> Geometry.t option
    val properties : t -> json option

    include JSON_CONV with type t := t and type json := json

    module Collection : sig
      type feature = t
      type t

      val features : t -> feature array

      include JSON_CONV with type t := t and type json := json
    end
  end

  type t =
    | Feature of Feature.t
    | FeatureCollection of Feature.Collection.t
    | Geometry of Geometry.t  (** A geojson object *)

  include JSON_CONV with type t := t and type json := json
end
