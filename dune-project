(lang dune 2.9)

(name geojson)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-geojson))

(license MIT)

(authors "Patrick Ferris")

(maintainers "pf341@patricoferris.com")

(package
 (name geojson)
 (synopsis "OCaml library for Geojson")
 (description "Geojson is a 'schema' for JSON to describe geospatial information. 
  This library provides a functor for building a module for manipulating and parsing Geojson into OCaml.")
 (depends
   (brr :with-test)
   (bechamel-notty (and (>= 0.1.0) :with-test))
   (alcotest :with-test)
   (ezjsonm  :with-test)))

(package
 (name geojsonm)
 (synopsis "Streaming geojson library using jsonm")
 (description "A library for manipulating Geojson as a stream, useful because Geojson can be gigabytes in size.")
 (depends 
   (geojson (= :version))
   jsonm 
   ezjsonm))

(using mdx 0.1)