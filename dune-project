(lang dune 2.9)
(version 0.0.1)
(name geojson)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-geojson))

(license MIT)

(authors "Patrick Ferris")

(maintainers "pf341@patricoferris.com")

(package
 (name geojson)
 (synopsis "OCaml functions for Geojson")
 (description "Geojson is a 'schema' for JSON to describe geospatial information. This library provides a functor for building a module for manipulating and parsing Geojson into OCaml."))

(package
 (name geojsonm)
 (synopsis "Streaming geojson library using jsonm")
 (description "A library for manipulating Geojson as a stream, useful because Geojson can be gigabytes in size.")
 (depends 
  (geojson (= :version))))

(using mdx 0.1)