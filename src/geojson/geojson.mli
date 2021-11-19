module S = S

include S.GEOMETRY

module Make (P : S.PARSER) : S.GEOJSON

module Ezjsonm_parser : S.PARSER with type t = Ezjsonm.value