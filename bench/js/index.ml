module Ezjsone_parser = struct
  type t = Ezjsone.value

  let catch_err f v =
    try Ok (f v) with Ezjsone.Parse_error (_, s) -> Error (`Msg s)

  let find = Ezjsone.find_opt
  let to_string t = catch_err Ezjsone.get_string t
  let string = Ezjsone.string
  let to_float t = catch_err Ezjsone.get_float t
  let float = Ezjsone.float
  let to_int t = catch_err Ezjsone.get_int t
  let int = Ezjsone.int
  let to_list f t = catch_err (Ezjsone.get_list f) t
  let list f t = Ezjsone.list f t
  let to_array f t = Result.map Array.of_list @@ to_list f t
  let array f t = list f (Array.to_list t)
  let to_obj t = catch_err Ezjsone.get_dict t
  let obj = Ezjsone.dict
  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

module Brr_parser = struct
  type t = Jv.t

  let catch_err f v = try Ok (f v) with _ -> Error (`Msg "Brr Error")

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
  let to_int t = catch_err Jv.to_int t
  let int = Jv.of_int
  let to_list f t = catch_err (Jv.to_list f) t
  let list f t = Jv.of_list f t
  let to_array f t = catch_err (Jv.to_array f) t
  let array f t = Jv.of_array f t
  let obj = Jv.get Jv.global "Object"

  let keys jv =
    let keys_method = Jv.get obj "keys" in
    Jv.apply keys_method [| jv |] |> Jv.to_jstr_list |> List.map Jstr.to_string

  let to_obj t =
    let keys = keys t in
    Ok (List.map (fun key -> (key, Jv.get t key)) keys)

  let obj arr = Jv.obj (Array.of_list arr)
  let null = Jv.null
  let is_null = Jv.is_null
end

module Ezgeo = Geojson.Make (Ezjsone_parser)
module Brrgeo = Geojson.Make (Brr_parser)

let x = Jv.get Jv.global "__DATA__" |> Jv.to_jstr

open Brr

let () =
  Console.log [ x ];
  let s = Jstr.to_string x in
  let start_ez = Performance.now_ms G.performance in
  let json = Ezjsone.value_from_string s in
  let _geo = Ezgeo.of_json json in
  let finish_ez = Performance.now_ms G.performance in
  let json = Result.get_ok @@ Json.decode x in
  let start_brr = Performance.now_ms G.performance in
  let _geo = Brrgeo.of_json json in
  let finish_brr = Performance.now_ms G.performance in
  Console.log [ Jstr.v "Ezjsone: "; Jv.of_float @@ (finish_ez -. start_ez) ];
  Console.log [ Jstr.v "Brr: "; Jv.of_float @@ (finish_brr -. start_brr) ]
