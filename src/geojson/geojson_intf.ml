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

(** {2 Json}

The GeoJson library does not force you to use a particular JSON parsing
library. You must provide one. See the tests and benchmarks for an [Ezjsonm]
parser and one for JS using [Brr]'s [Jv] library. *)
module type Json = sig
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

(* {2 Json Conversion} *)

module type Json_conv = sig
  type t
  type json

  val base_of_json : json -> (t, [ `Msg of string ]) result
  val to_json : ?bbox:float array option -> t -> json
end

(** {2 GeoJson Geometry Objects}

    The basic primitives for building geometrical shapes in GeoJson. *)

module type Geometry = sig
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

    val of_json : json -> (t, [ `Msg of string ]) result
    val to_json : t -> json
  end

  module Point : sig
    type t
    (** A point is a single {!Position.t} *)

    val position : t -> Position.t
    (** Convert a point to a position *)

    val v : Position.t -> t
    (** Create a poitn from a position. *)

    include Json_conv with type t := t and type json := json
  end

  module MultiPoint : sig
    type t
    (** A multipoint is an array of positions. *)

    val coordinates : t -> Position.t array
    (** Get the positions that make up this multipoint object. *)

    val v : Position.t array -> t
    (** Create a multipoint object from an array of positions. *)

    include Json_conv with type t := t and type json := json
  end

  module LineString : sig
    type t
    (** A line string is two or more points *)

    val coordinates : t -> Position.t array
    (** Convert the line into a positionn array *)

    val v : Position.t array -> t
    (** Create a line string from positions, will raise [Invalid_argument] if
        the array doesn't have at least two positions. *)

    include Json_conv with type t := t and type json := json
  end

  module MultiLineString : sig
    type t
    (** A collection of line strings *)

    val lines : t -> LineString.t array
    (** Access the lines *)

    val v : LineString.t array -> t
    (** Create a multiline string *)

    include Json_conv with type t := t and type json := json
  end

  module Polygon : sig
    type t
    (** A close loop with optional rings *)

    val interior_ring : t -> LineString.t
    val exterior_rings : t -> LineString.t array

    val v : LineString.t array -> t
    (** Create a polygon object from an array of close line strings (note no
        checking is down here to ensure the loops are indeed closed.) *)

    include Json_conv with type t := t and type json := json
  end

  module MultiPolygon : sig
    type t
    (** A multi-polygon object *)

    val polygons : t -> Polygon.t array
    (** Access the polygons *)

    val v : Polygon.t array -> t
    (** Create a multi-polygon object from an array of {!Polygon.t}s *)

    include Json_conv with type t := t and type json := json
  end

  type t =
    | Point of Point.t
    | MultiPoint of MultiPoint.t
    | LineString of LineString.t
    | MultiLineString of MultiLineString.t
    | Polygon of Polygon.t
    | MultiPolygon of MultiPolygon.t
    | Collection of t list

  include Json_conv with type t := t and type json := json
end

module type S = sig
  type json
  (** The internal representation of JSON *)

  module Geometry : Geometry with type json = json

  module Feature : sig
    type t
    (** A feature object is a geojson object with optional geometry and
        properties members. *)

    val geometry : t -> Geometry.t option
    val properties : t -> json option

    include Json_conv with type t := t and type json := json

    module Collection : sig
      type feature = t
      type t

      val features : t -> feature list

      include Json_conv with type t := t and type json := json
    end
  end

  type geojson =
    | Feature of Feature.t
    | FeatureCollection of Feature.Collection.t
    | Geometry of Geometry.t  (** A geojson object *)

  type t = { geojson : geojson; bbox : float array option }

  val of_json : json -> (t, [ `Msg of string ]) result
  val to_json : t -> json

  module Random : sig
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

    (** {3 Generate random geojson}

        The random module provides a way of quickly constructing random, correct
        GeoJson. You provide the skeleton of the document using type {!t} and
        tweaking some of the parameters. For example:

        [{
          let random_structure = 
            FC (List.init 100 (fun _ -> { properties = None; geometry = Point }))
        }]*)

    val random : f:(unit -> float) -> r -> t
    (** [random ~f r] produces random GeoJson based on the structure provided by
        [r] and using the random float generator [f]. Note the random geometry
        maker will follow the rules of GeoJson (for making Polygons for
        example). *)
  end
end

module type Geojson = sig
  module type S = S
  (** Types for Geojson texts and objects *)

  (** A functor that takes a Json parsing implementation and returns a GeoJson
      parser and constructor. *)
  module Make (J : Json) : S with type json = J.t
end
