ocaml-geojson
-------------

A collection of libraries for parsing, building and manipulating [geojson][] (a.k.a RFC7946). There are two libraries:

 - *geojson*: a library of types and functors for building useful modules for manipulating and constructing geojson. The OCaml implementation uses *ezjsonm* whilst the JS version uses built-in browser tools.
 - *geojsonm*: geojson can be huge (GBs of data) so building an in-memory representation of the geojson is infeasible. Geojsonm uses the excellent [jsonm]() to provide functions to manipulate geojson using a streaming parser.

[geojson]: https://datatracker.ieft.org/doc/html/rfc7946