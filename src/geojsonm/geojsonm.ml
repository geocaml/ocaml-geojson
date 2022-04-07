(* Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
   Copyright (c) 2021 Patrick Ferris <patrick@sirref.org>

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

   The defunctionalised value construction is borrowed from Ezjsonm.
*)

(* A GeoJson document consists of a single JSON document that is either a feature collection
   (an array of features), a single feature (an array of geometry objects) or a single geometry
   objects (which could contain multiple geometry objects thanks to the collection type).

   Most commmonly, the large size of a GeoJson document is because it is a feature collection
   containing many features, although it's probably not infeasible that there are huge documents
   containing a single feature with lots of geometry objects. *)

module Err = struct
  type location = (int * int) * (int * int)
  type t = [ `Error of location * Jsonm.error | `EOI | `Unexpected of string ]

  let pp ppf = function
    | `Error (((l1, l2), (l3, l4)), e) ->
        Format.fprintf ppf "Error %a (%i:%i - %i:%i)" Jsonm.pp_error e l1 l2 l3
          l4
    | `EOI -> Format.fprintf ppf "Unexpected end of input"
    | `Unexpected s -> Format.fprintf ppf "Unexpected %s" s
end

exception Abort of Err.t

module G = struct
  module Ezjsonm_parser = struct
    type t = Ezjsonm.value

    let catch_err f v =
      try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

    let find = Ezjsonm.find_opt
    let to_string t = catch_err Ezjsonm.get_string t
    let string = Ezjsonm.string
    let to_float t = catch_err Ezjsonm.get_float t
    let float = Ezjsonm.float
    let to_list f t = catch_err (Ezjsonm.get_list f) t
    let list f t = Ezjsonm.list f t
    let to_array f t = Result.map Array.of_list @@ to_list f t
    let array f t = list f (Array.to_list t)
    let obj = Ezjsonm.dict
    let null = `Null
    let is_null = function `Null -> true | _ -> false
  end

  include Geojson.Make (Ezjsonm_parser)
end

let decode_single_object decoder : Ezjsonm.value =
  let module Stack = struct
    type t =
      | In_array of Ezjsonm.value list * t
      | In_object of string * (string * Ezjsonm.value) list * t
      | Empty
  end in
  let loc () = Jsonm.decoded_range decoder in
  let dec () =
    match Jsonm.decode decoder with
    | `Lexeme l -> l
    | `Error e -> raise (Abort (`Error (loc (), e)))
    | `End -> raise (Abort `EOI)
    | `Await -> assert false
  in
  let rec enter l stack =
    match l with
    | `Os -> obj [] stack
    | _ -> raise (Abort (`Unexpected "decoding single object failed"))
  and value l stack =
    match l with
    | `Os -> obj [] stack
    | `As -> arr [] stack
    | (`Null | `Bool _ | `String _ | `Float _) as l -> continue l stack
    | _ -> raise (Abort (`Unexpected "value"))
  and arr so_far stack =
    match dec () with
    | `Ae -> continue (`A (List.rev so_far)) stack
    | l ->
        let stack = Stack.In_array (so_far, stack) in
        value l stack
  and obj so_far stack =
    match dec () with
    | `Oe -> continue (`O (List.rev so_far)) stack
    | `Name n ->
        let stack = Stack.In_object (n, so_far, stack) in
        value (dec ()) stack
    | _ -> raise (Abort (`Unexpected "object fields"))
  and continue v stack =
    match stack with
    | Stack.In_array (vs, stack) ->
        let so_far = v :: vs in
        arr so_far stack
    | Stack.In_object (n, ms, stack) ->
        let so_far = (n, v) :: ms in
        obj so_far stack
    | Stack.Empty -> v
  in
  enter (dec ()) Empty

let encode_value e json =
  let module Stack = struct
    type t =
      | In_array of Ezjsonm.value list * t
      | In_object of (string * Ezjsonm.value) list * t
      | Empty
  end in
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
  let rec t v e stack =
    match v with
    | `A vs ->
        enc e `As;
        arr vs e stack
    | `O ms ->
        enc e `Os;
        obj ms e stack
  and value v e stack =
    match v with
    | (`Null | `Bool _ | `Float _ | `String _) as v ->
        enc e v;
        continue e stack
    | #Ezjsonm.t as x -> t (x :> Ezjsonm.t) e stack
  and arr vs e stack =
    match vs with
    | v :: vs' ->
        let stack = Stack.In_array (vs', stack) in
        value v e stack
    | [] ->
        enc e `Ae;
        continue e stack
  and obj ms e stack =
    match ms with
    | (n, v) :: ms ->
        enc e (`Name n);
        let stack = Stack.In_object (ms, stack) in
        value v e stack
    | [] ->
        enc e `Oe;
        continue e stack
  and continue e stack =
    match stack with
    | Stack.In_array (vs, stack) -> arr vs e stack
    | Stack.In_object (ms, stack) -> obj ms e stack
    | Stack.Empty -> ()
  in
  value json e Stack.Empty

let map_geometry f src dst =
  let decoder = Jsonm.decoder src in
  let encoder = Jsonm.encoder dst in
  let loc () = Jsonm.decoded_range decoder in
  let enc v =
    match Jsonm.encode encoder v with
    | `Ok -> ()
    | `Partial -> raise (Abort (`Unexpected "partial encoding"))
  in
  let rec go () =
    match Jsonm.decode decoder with
    (* TODO(patricoferris): A geometry collection could explode on us here... *)
    | `Lexeme (`Name "geometry" as t) -> (
        match G.Geometry.base_of_json @@ decode_single_object decoder with
        | Error (`Msg m) -> raise (Abort (`Unexpected m))
        | Ok g ->
            let g' = f g in
            enc (`Lexeme t);
            encode_value encoder (G.Geometry.to_json g');
            go ())
    | `Lexeme _ as t ->
        enc t;
        go ()
    | `Error e -> raise (Abort (`Error (loc (), e)))
    | `End -> ignore @@ Jsonm.encode encoder `End
    | `Await -> assert false
  in
  try Ok (go ()) with Abort e -> Error e

let map_props f src dst =
  let decoder = Jsonm.decoder src in
  let encoder = Jsonm.encoder dst in
  let loc () = Jsonm.decoded_range decoder in
  let enc v =
    match Jsonm.encode encoder v with
    | `Ok -> ()
    | `Partial -> raise (Abort (`Unexpected "partial encoding"))
  in
  let rec go () =
    match Jsonm.decode decoder with
    | `Lexeme (`Name "properties" as t) ->
        let o = f @@ decode_single_object decoder in
        enc (`Lexeme t);
        encode_value encoder o;
        go ()
    | `Lexeme _ as t ->
        enc t;
        go ()
    | `Error e -> raise (Abort (`Error (loc (), e)))
    | `End -> ignore @@ Jsonm.encode encoder `End
    | `Await -> assert false
  in
  try Ok (go ()) with Abort e -> Error e

let fold_geometry f init src =
  let decoder = Jsonm.decoder src in
  let loc () = Jsonm.decoded_range decoder in
  let rec go acc =
    match Jsonm.decode decoder with
    | `Lexeme (`Name "geometry") -> (
        match G.Geometry.base_of_json @@ decode_single_object decoder with
        | Error (`Msg m) -> raise (Abort (`Unexpected m))
        | Ok g ->
            let acc = f acc g in
            go acc)
    | `Lexeme _ -> go acc
    | `Error e -> raise (Abort (`Error (loc (), e)))
    | `End -> acc
    | `Await -> assert false
  in
  try Ok (go init) with Abort e -> Error e

let fold_props f init src =
  let decoder = Jsonm.decoder src in
  let loc () = Jsonm.decoded_range decoder in
  let rec go acc =
    match Jsonm.decode decoder with
    | `Lexeme (`Name "properties") ->
        let acc' = f acc @@ decode_single_object decoder in
        go acc'
    | `Lexeme _ -> go acc
    | `Error e -> raise (Abort (`Error (loc (), e)))
    | `End -> acc
    | `Await -> assert false
  in
  try Ok (go init) with Abort e -> Error e

let iter_geometry f src =
  let decoder = Jsonm.decoder src in
  let loc () = Jsonm.decoded_range decoder in
  let rec go () =
    match Jsonm.decode decoder with
    | `Lexeme (`Name "geometry") -> (
        match G.Geometry.of_json @@ decode_single_object decoder with
        | Error (`Msg m) -> raise (Abort (`Unexpected m))
        | Ok g ->
            f g;
            go ())
    | `Lexeme _ -> go ()
    | `Error e -> raise (Abort (`Error (loc (), e)))
    | `End -> ()
    | `Await -> assert false
  in
  try Ok (go ()) with Abort e -> Error e

let iter_props f src =
  let decoder = Jsonm.decoder src in
  let loc () = Jsonm.decoded_range decoder in
  let rec go () =
    match Jsonm.decode decoder with
    | `Lexeme (`Name "properties") ->
        f @@ decode_single_object decoder;
        go ()
    | `Lexeme _ -> go ()
    | `Error e -> raise (Abort (`Error (loc (), e)))
    | `End -> ()
    | `Await -> assert false
  in
  try Ok (go ()) with Abort e -> Error e
