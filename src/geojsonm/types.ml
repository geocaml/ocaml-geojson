module type Jsonm = sig
  type decoder
  type encoder
  type error
  type lexeme = Jsonm.lexeme

  val pp_error : Format.formatter -> error -> unit

  val decoded_range : decoder -> (int * int) * (int * int)

  type src = [ `Manual | `Channel of in_channel | `String of string]
  type dst = [ `Manual | `Channel of out_channel | `Buffer of Buffer.t]

  type encoding = [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

  val decoder : ?encoding:[< encoding ] -> src -> decoder
  val encoder : ?minify:bool -> dst -> encoder
  
  val decode : decoder -> [> `Await | `Lexeme of lexeme | `End | `Error of error ]
  val encode : encoder -> [ `Lexeme of lexeme | `End ] -> [`Ok | `Partial]
end