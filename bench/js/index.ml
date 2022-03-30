module Ezjsonm_parser = struct
  type t = Ezjsonm.value

  let catch_err f v =
    try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

  let find_opt t path =
    try Some (Ezjsonm.find t path) with Not_found -> None

  let find = find_opt
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

module Brr_parser = struct
  type t = Jv.t

  let catch_err f v =
    try Ok (f v) with Ezjsonm.Parse_error (_, s) -> Error (`Msg s)

  let find t s =
    let rec loop jv = function
      | [] -> jv
      | x :: xs -> loop (Option.bind jv (fun jv -> Jv.find jv x)) xs
    in
    loop (Some t) s

  let to_string t = catch_err Jv.to_string t
  let string = Jv.of_string
  let to_float t = catch_err Jv.to_float t
  let float = Jv.of_float
  let to_list f t = catch_err (Jv.to_list f) t
  let list f t = Jv.of_list f t
  let to_array f t = catch_err (Jv.to_array f) t
  let array f t = Jv.of_array f t
  let obj arr = Jv.obj (Array.of_list arr)
  let null = Jv.null
  let is_null = Jv.is_null
end

module Ezgeo = Geojson.Make (Ezjsonm_parser)
module Brrgeo = Geojson.Make (Brr_parser)

let x = Jv.get Jv.global "__DATA__" |> Jv.to_jstr

open Brr

let () =
  Console.log [ x ];
  let s = Jstr.to_string x in
  let start_ez = Performance.now_ms G.performance in
  let json = Ezjsonm.value_from_string s in
  let _geo = Ezgeo.of_json json in
  let finish_ez = Performance.now_ms G.performance in
  let json = Result.get_ok @@ Json.decode x in
  let start_brr = Performance.now_ms G.performance in
  let _geo = Brrgeo.of_json json in
  let finish_brr = Performance.now_ms G.performance in
  Console.log [ Jstr.v "Ezjsonm: "; Jv.of_float @@ (finish_ez -. start_ez) ];
  Console.log [ Jstr.v "Brr: "; Jv.of_float @@ (finish_brr -. start_brr) ]
