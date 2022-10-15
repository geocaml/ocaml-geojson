# ocaml-geojson

A collection of libraries for reading and writing [GeoJSON](https://www.rfc-editor.org/rfc/rfc7946). This repository contains two libraries, a non-blocking streaming GeoJSON library called `geojsone` (this requires OCaml 5 with effects) and a normal GeoJSON library.

- [Geojson](#geojson)
  - [Reading GeoJSON Values](#reading-geojson-values)
  - [Foreign Members](#foreign-members)
  - [Accessing Deeply Nested Objects with Optics](#accessing-deeply-nested-objects-with-optics)
  - [Building GeoJSON values](#building-geojson-values)
- [Geojsone](#geojsone)
  - [Constructing Decoders and Encoders](#constructing-decoders-and-encoders)
    - [Eio sources and sinks](#eio-sources-and-sinks)
  - [Mapping](#mapping)
  - [Folding](#folding)

## Geojson

The `geojson` library allows you to parse GeoJSON objects. The implementation requires you to provide a JSON parser of your choosing. Take a look in [the prelude file](./docs/prelude.txt) for an implementation using [ezjsonm](https://github.com/mirage/ezjsonm). The prelude also contains encoded GeoJSON objects as OCaml strings for use in the examples.

The first thing to do is create a GeoJSON parser from your JSON parser.

```ocaml
module G = Geojson.Make (Ezjsonm_parser)
```

### Reading GeoJSON Values

Reading values relies on your JSON parser's methods for creating a JSON value. With `ezjsonm` we can read strings.

```ocaml
# let feature = G.of_json (Ezjsonm.value_from_string feature_example);;
val feature : (G.t, [ `Msg of string ]) result = Ok <abstr>
```

This returns a result, so either `Ok g` where `g` is a GeoJSON object or an `Error`.

```ocaml
# let feature = Result.get_ok feature;;
val feature : G.t = <abstr>
```

A GeoJSON object can either be a feature, a geometry or a feature collection. To know which one you have, you will need to pattern match on the value for the `geojson` value. With this example we know we should have a feature so we will just `assert false`.

```ocaml
# let f = match G.geojson feature with
  | G.Feature f -> f
  | _ -> assert false;;
val f : G.Feature.t = <abstr>
```

Now we can access feature specific values from our OCaml value.

```ocaml
# let props = G.Feature.properties f;;
val props : G.json option = Some (`O [("name", `String "Dinagat Islands")])
```

### Foreign Members

Foreign members are those JSON key-value pairs that are not a part of the specification. Sometimes your GeoJSON data might include extra information and this is a way to gain access to it after you have parsed the value.

```ocaml
# G.Feature.foreign_members f;;
- : (string * G.json) list = [("title", `String "Some Islands")]
```

### Accessing Deeply Nested Objects with Optics

There is an experimental module in the library called `Geojson.Accessor`. This uses [optics]() to allow you to more easily access values that are deeply nested. An important note is that they will always tend to be less efficient than manually pattern-matching.

However, using our feature as an example, if we wanted to access the multipoint without matching all the way down, we can construct an optic to help us.

```ocaml
# let g_to_mp = G.Accessor.(geojson >& feature &> Feature.geometry_exn &> Geometry.geometry $> Geometry.multipoint);;
val g_to_mp : (G.t, G.Geometry.MultiPoint.t) G.Accessor.Optics.Optional.t =
  Geojson__Optics.Lens.V (<fun>, <fun>)
```

This is a lens that lets use focus all the way down from the GeoJSON object containing a feature, containing a geometry that is a multipoint.

```ocaml
# G.Accessor.get g_to_mp feature |> Option.get |> G.Geometry.MultiPoint.coordinates;;
- : G.Geometry.Position.t array = [|[|125.1; 40.|]; [|155.9; 22.5|]|]
```

### Building GeoJSON values

You can also construct GeoJSON objects using OCaml values.

```ocaml
# let geometry = 
  G.Geometry.(v 
    ~foreign_members:["hello", `String "World"]
    (Point (Point.v (Position.v ~lat:1.123 ~lng:2.321 ()))));;
val geometry : G.Geometry.t = <abstr>
# let g = G.(v (Geometry geometry));; 
val g : G.t = <abstr>
# G.to_json g |> Ezjsonm.value_to_string;;
- : string =
"{\"type\":\"Point\",\"coordinates\":[2.321,1.123],\"hello\":\"World\"}"
```

## Geojsone

Geojsone is a non-blocking, streaming parser for GeoJSON objects. Currently, it 
uses a modified version of [jsonm](http://erratique.ch/software/jsonm), called jsone. 
It uses effects to provide non-blocking reading and writing functions rather than
passing continuations the whole way through the parser. It is still experimental.

### Constructing Decoders and Encoders

In order to build decoders and encoder, you must provide a source and destination.

```ocaml
# #show_type Geojsone.Jsone.src;;
type nonrec src = unit -> Cstruct.t
# #show_type Geojsone.Jsone.dst;;
type nonrec dst = Cstruct.t -> unit
```

These are functions for filling a buffer (a `Cstruct.t`) and reading a buffer. Note they appear as normal OCaml functions (no IO monad like `Lwt.t`). You will have to use a library that uses effects for non-blocking IO in order to make the decoders and encoders non-blocking. If you don't mind blocking (for example, in js_of_ocaml) you can provide blocking versions of these functions.

#### Eio sources and sinks

The API was designed around [Eio](https://github.com/ocaml-multicore/eio) although there is no explicit dependency. Here are two functions using Eio that provide Jsone sources and sinks.

The first takes a buffer and turns it into a destination.

```ocaml
# let buffer_to_dst buf bs =
  Eio.Flow.(copy (cstruct_source [ bs ]) (buffer_sink buf));;
val buffer_to_dst : Buffer.t -> Cstruct.t -> unit = <fun>
```

For reading, we can turn an arbitrary Eio `Flow.t` into a source. A `Flow.t` is a byte-stream so a file, a socket etc.

```ocaml
# let src_of_flow flow =
  let buff = Cstruct.create 2048 in
  fun () ->
    let got = Eio.Flow.(read flow buff) in
    let t = Cstruct.sub buff 0 got in
    t;;
val src_of_flow : #Eio.Flow.source -> unit -> Cstruct.t = <fun>
```

Note that your source function should raise `End_of_file` when there are no more bytes to be read. `Eio.Flow.read` does this.

With both of these we can now construct an encoder and decoder.

```ocaml
# let decoder s = Geojsone.Jsone.decoder (src_of_flow @@ Eio.Flow.string_source s);;
val decoder : string -> Geojsone.Jsone.decoder = <fun>
# let encoder buf = Geojsone.Jsone.encoder (buffer_to_dst buf);;
val encoder : Buffer.t -> Geojsone.Jsone.encoder = <fun>
```

### Mapping

There are various mapping functions for iterating over a GeoJSON object. For example, you may wish to visit all of the `properties` in your object.

```ocaml
# Geojsone.map_props;;
- : (Geojsone.G.json -> Geojsone.G.json) ->
    Geojsone.Jsone.src -> Geojsone.Jsone.dst -> (unit, Geojsone.Err.t) result
= <fun>
```

We can see if any properties are objects with a field called `name`, and capitalise its value.

```ocaml
let capitalise_name = function
  | `O [ "name", `String s ] -> `O [ "name", `String (String.uppercase_ascii s) ]
  | v -> v
let buf = Buffer.create 256
```

We can then use this function for our example.

```ocaml
# let feature_source () = src_of_flow @@ Eio.Flow.string_source feature_example;;
val feature_source : unit -> unit -> Cstruct.t = <fun>
# Geojsone.(map_props capitalise_name (feature_source ()) (buffer_to_dst buf));;
- : (unit, Geojsone.Err.t) result = Ok ()
# Buffer.contents buf;;
- : string =
"{\"type\":\"Feature\",\"geometry\":{\"type\":\"MultiPoint\",\"coordinates\":[[125.1,40],[155.9,22.5]]},\"properties\":{\"name\":\"DINAGAT ISLANDS\"},\"title\":\"Some Islands\"}"
```

### Folding

Folding is similar to mapping except you can accumulate a value as you iterate over the document.

```ocaml
# Geojsone.fold_geometry;;
- : ('a -> Geojsone.G.Geometry.t -> 'a) ->
    'a -> Geojsone.Jsone.src -> ('a, Geojsone.Err.t) result
= <fun>
```

So we could simply count the number of geometry objects for example.

```ocaml
let count_geometries acc _ = acc + 1
```

And we can apply it to our running example.

```ocaml
# Geojsone.fold_geometry count_geometries 0 (feature_source ()) ;;
- : (int, Geojsone.Err.t) result = Ok 1
```
