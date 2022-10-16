(lang dune 2.9)

(name geojson)

(generate_opam_files true)

(source
 (github geocaml/ocaml-geojson))

(license MIT)

(authors "Patrick Ferris")

(maintainers "patrick@sirref.org")

(package
 (name geojson)
 (synopsis "Pure OCaml library for GeoJSON")
 (tags ("geojson" "geospatial" "geocaml"))
 (description "GeoJSON is a 'schema' for JSON to describe geospatial information. This library provides a JSON-parser-agnostic library for manipulating and parsing GeoJSON into OCaml.")
 (depends
   (bos :with-test)
   (mdx :with-test)
   (alcotest :with-test)
   (ezjsonm :with-test)))

(package
 (name geojsone)
 (synopsis "Streaming GeoJSON library")
  (tags ("geojson" "geospatial" "geocaml"))
 (description "A library for manipulating GeoJSON using a streaming parser. This is useful because GeoJSON can be gigabytes in size.")
 (depends
   (geojson (= :version))
   (mdx :with-test)
   (ezjsonm :with-test) ; Needed for benchmarks
   (eio_main (and (>= 0.6) :with-test))
   hex
   sexplib0))

(using mdx 0.1)