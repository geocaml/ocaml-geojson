module type Jsonm = sig
  type decoder
  type encoder
  type error
  type lexeme = Jsonm.lexeme

  val pp_error : Format.formatter -> error -> unit

  val decoded_range : decoder -> (int * int) * (int * int)

  type src = unit -> (bytes * int * int) option

  type dst = (bytes * int * int) option -> unit

  type encoding = [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

  val decoder : ?encoding:[< encoding ] -> src -> decoder
  val encoder : ?minify:bool -> dst -> encoder
  
  val decode : decoder -> [> `Await | `Lexeme of lexeme | `End | `Error of error ]
  val encode : encoder -> [ `Lexeme of lexeme | `End ] -> [`Ok | `Partial]
end