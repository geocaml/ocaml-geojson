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

module G : Geojson.S with type json = Ezjsonm.value

(** {2 Maps}

    Maps are functions that allow you to manipulate common structure in GeoJson
    objects. These will be written directly back to the destination that you
    provide. *)

val map_geometry :
  (G.Geometry.t -> G.Geometry.t) ->
  Jsonm.src ->
  Jsonm.dst ->
  (unit, Err.t) result
(** [map_geometry f src dst] will apply [f] to all GeoJson objects. This is
    essentially any
    {{:https://datatracker.ietf.org/doc/html/rfc7946#section-3.1} geometry
    object}.

    The map will recurse into geometry collections. Note for the moment if you
    have a single geometry object as your document, this will not work. *)

val map_props :
  (Ezjsonm.value -> Ezjsonm.value) ->
  Jsonm.src ->
  Jsonm.dst ->
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
        Geojsonm.fold_props (fun acc p -> get_name p :: acc) [] src
    ]} *)

val fold_geometry :
  ('a -> G.Geometry.t -> 'a) -> 'a -> Jsonm.src -> ('a, Err.t) result
(** [fold_geometry f acc src] is much like {!map_geometry} but allows you to
    accumulate some result that is then returned to you. *)

val fold_props :
  ('a -> Ezjsonm.value -> 'a) -> 'a -> Jsonm.src -> ('a, Err.t) result
(** [fold_props f init src] *)

(** {2 Iterators}

    Iterators follows the type signature as:

    val iter : ('a -> unit) -> 'a list -> unit

    For eample, List.iter print_int [1; 2; 3; 4] ;;

    will return

    1234- : unit =() as a unit.

    Iterators are similar to map functions except they take a function [f] that
    takes a single element from the data-structure as an argument and returns
    [unit]. In that sense, they tend to be functions with side-effects, such as
    [print_endline] *)

val iter_geometry : (G.Geometry.t -> unit) -> Jsonm.src -> (unit, Err.t) result
(** [iter_geometry f src] will apply [f] to all GeoJson objects. *)

val iter_props : (Ezjsonm.value -> unit) -> Jsonm.src -> (unit, Err.t) result
(** [iter_props src ~f] will apply [f] to each feature's properties field. *)
