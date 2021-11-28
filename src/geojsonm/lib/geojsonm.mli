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

(** A library for manipulating large GeoJson documents without reading the whole
    document into memory using the {!Jsonm} streaming, JSON parser. *)

module Err : sig
  type location = (int * int) * (int * int)
  type t = [ `Error of location * Jsonm.error | `EOI | `Unexpected of string ]

  val pp : Format.formatter -> t -> unit
end

type geometry =
  | Point
  | MultiPoint
  | LineString
  | MultiLineString
  | Polygon
  | MultiPolygon
      (** Types for geometry objects to be used in mapping over coordinates
          (hence the GeometryCollection is absent). *)

type document =
  | FeatureCollection
  | Feature
  | Geometry  (** A GeoJson text is a single object of one of these types. *)

(** {2 Maps}

    Maps are functions that allow you to manipulate common structure in GeoJson
    objects. These will be written directly back to the destination that you
    provide.

    For example, you might want to scale all coordinates by a factor of two.

    {[
      let scale factor decoder = map_coords decoder (Array.map (( *. ) factor))
    ]} *)

val map_coords :
  Jsonm.src -> Jsonm.dst -> f:(geometry -> float array -> float array) -> unit
(** [map_coords src dst ~f] will apply [f] to all GeoJson objects with something
    coordinate-like. This is essentially any
    {{:https://datatracker.ietf.org/doc/html/rfc7946#section-3.1} geometry
    object}. The type of geometry object currently being mapped over is also
    provided in case you wish to be more specific.

    The map will recurse into geometry collections. *)

val map_props :
  Jsonm.src ->
  Jsonm.dst ->
  f:(Ezjsonm.value -> Ezjsonm.value) ->
  (unit, Err.t) result
(** [map_props src dst ~f] will apply [f] to each feature's properties field.
    The properties field is decoded into an {!Ezjsonm.value} for convenience. *)

(** {2 Folds}

    Folds are like maps except you can collect items into an accumulator which
    is returned to you.

    For example, you might want to collect all of the [names] in the
    [properties] of features.

    {[
      let get_string_exn = function `String s -> s | _ -> failwith "err"

      let get_name = function
        | `O assoc -> List.assoc "name" assoc |> get_string_exn
        | _ -> failwith "err"

      let places src =
        Geojsonm.fold_props src ~f:(fun acc p -> get_name p :: acc) []
    ]} *)

val fold_props :
  Jsonm.src -> f:('a -> Ezjsonm.value -> 'a) -> init:'a -> ('a, Err.t) result
(** [fold_props src f init] *)
