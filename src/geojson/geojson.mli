module S = S
module Make (J : S.JSON) : S.GEOJSON with type json = J.t
