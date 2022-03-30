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

(package
 (name extension)
 (synopsis "Library to extend functionalit of Ezjsonm")
 (description "This Extension library adds the find_opt function to earlier versions of Ezjsonm")
 (depends
   (ezjsonm  :with-test)))

(using mdx 0.1)