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
 (synopsis "OCaml library for Geojson")
 (description "Geojson is a 'schema' for JSON to describe geospatial information. 
  This library provides a functor for building a module for manipulating and parsing Geojson into OCaml.")
 (depends
   (bos :with-test)
   (alcotest :with-test)
   (geojsone  (and :version :with-test))))

(package
 (name geojsone)
 (synopsis "Streaming geojson library")
 (description "A library for manipulating Geojson as a stream, useful because Geojson can be gigabytes in size.")
 (depends
   (geojson (= :version))
   (eio (>= 0.5))
   hex
   sexplib0))

(using mdx 0.1)