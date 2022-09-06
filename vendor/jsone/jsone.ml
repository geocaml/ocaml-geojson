(*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Braced non-terminals in comments refer to RFC 4627 non-terminals. *)

let debug = false
let debug_print fn = if debug then fn ()
let io_buffer_size = 65536 (* IO_BUFFER_SIZE 4.0.0 *)
let pp = Format.fprintf

(* Unsafe string and bytes manipulations. If you don't believe the authors's
   invariants, replacing with safe versions makes everything safe in the
   module. He won't be upset. *)

let unsafe_byte s j = Char.code (String.unsafe_get s j)
let unsafe_blit s soff d doff = Cstruct.blit_from_string s soff d doff
let unsafe_set_byte s j byte = Cstruct.set_char s j (Char.unsafe_chr byte)

(* Characters and their classes *)

let ux_eoi = max_int (* End of input, outside unicode range. *)
let ux_soi = max_int - 1 (* Start of input, outside unicode range. *)
let u_nl = 0x0A (* \n *)
let u_sp = 0x20 (*   *)
let u_quot = 0x22 (* '' *)
let u_lbrack = 0x5B (* [ *)
let u_rbrack = 0x5D (* ] *)
let u_lbrace = 0x7B (* { *)
let u_rbrace = 0x7D (* } *)
let u_colon = 0x3A (* : *)
let u_comma = 0x2C (* , *)
let u_minus = 0x2D (* - *)
let u_slash = 0x2F (* / *)
let u_bslash = 0x5C (* \ *)
let u_rep = Uchar.to_int Uutfe.u_rep
let must_escape u = u <= 0x1F || u = 0x22 || u = 0x5C
let is_digit u = 0x30 <= u && u <= 0x39

let is_hex_digit u =
  (0x30 <= u && u <= 0x39)
  || (0x41 <= u && u <= 0x46)
  || (0x61 <= u && u <= 0x66)

let is_white = function
  (* N.B. Uutfe normalizes U+000D to U+000A. *)
  | 0x20 | 0x09 | 0x0A -> true
  | _ -> false

let is_val_sep = function
  (* N.B. Uutfe normalizes U+000D to U+000A. *)
  | 0x20 | 0x09 | 0x0A | 0x2C | 0x5D | 0x7D -> true
  | _ -> false

(* Data model *)

type lexeme =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe ]

let pp_lexeme ppf = function
  | `Null -> pp ppf "`Null"
  | `Bool b -> pp ppf "@[`Bool %b@]" b
  | `String s -> pp ppf "@[`String %S@]" s
  | `Name s -> pp ppf "@[`Name %S@]" s
  | `Float f -> pp ppf "@[`Float %s@]" (string_of_float f)
  | `As -> pp ppf "`As"
  | `Ae -> pp ppf "`Ae"
  | `Os -> pp ppf "`Os"
  | `Oe -> pp ppf "`Oe"

(* Decode *)

type error =
  [ `Illegal_BOM
  | `Illegal_escape of
    [ `Not_hex_uchar of Uchar.t
    | `Not_esc_uchar of Uchar.t
    | `Not_lo_surrogate of int
    | `Lone_lo_surrogate of int
    | `Lone_hi_surrogate of int ]
  | `Illegal_string_uchar of Uchar.t
  | `Illegal_bytes of string
  | `Illegal_literal of string
  | `Illegal_number of string
  | `Unclosed of [ `As | `Os | `String | `Comment ]
  | `Expected of
    [ `Comment
    | `Value
    | `Name
    | `Name_sep
    | `Json
    | `Eoi
    | `Aval of bool (* [true] if first array value  *)
    | `Omem of bool (* [true] if first object member *) ] ]

let err_bom = `Error `Illegal_BOM
let err_not_hex u = `Error (`Illegal_escape (`Not_hex_uchar (Uchar.of_int u)))
let err_not_esc u = `Error (`Illegal_escape (`Not_esc_uchar (Uchar.of_int u)))
let err_not_lo p = `Error (`Illegal_escape (`Not_lo_surrogate p))
let err_lone_lo p = `Error (`Illegal_escape (`Lone_lo_surrogate p))
let err_lone_hi p = `Error (`Illegal_escape (`Lone_hi_surrogate p))
let err_str_char u = `Error (`Illegal_string_uchar (Uchar.of_int u))
let err_unclosed_string = `Error (`Unclosed `String)
let err_unclosed_arr = `Error (`Unclosed `As)
let err_unclosed_obj = `Error (`Unclosed `Os)
let err_number s = `Error (`Illegal_number s)
let err_literal s = `Error (`Illegal_literal s)
let err_exp_value = `Error (`Expected `Value)
let err_exp_name = `Error (`Expected `Name)
let err_exp_nsep = `Error (`Expected `Name_sep)
let err_exp_arr_fst = `Error (`Expected (`Aval true))
let err_exp_arr_nxt = `Error (`Expected (`Aval false))
let err_exp_obj_fst = `Error (`Expected (`Omem true))
let err_exp_obj_nxt = `Error (`Expected (`Omem false))
let err_exp_json = `Error (`Expected `Json)
let err_exp_eoi = `Error (`Expected `Eoi)
let pp_cp ppf u = pp ppf "U+%04X" u

let pp_uchar ppf u =
  if Uchar.to_int u <= 0x1F (* most control chars *) then
    pp_cp ppf (Uchar.to_int u)
  else
    let b = Buffer.create 4 in
    Uutfe.Buffer.add_utf_8 b u;
    pp ppf "'%s' (%a)" (Buffer.contents b) pp_cp (Uchar.to_int u)

let pp_error ppf = function
  | `Illegal_BOM -> pp ppf "@[illegal@ initial@ BOM@ in@ character@ stream@]"
  | `Illegal_escape r -> (
      pp ppf "@[illegal@ escape,@ ";
      match r with
      | `Not_hex_uchar u -> pp ppf "%a@ not@ a@ hex@ digit@]" pp_uchar u
      | `Not_esc_uchar u ->
          pp ppf "%a@ not@ an@ escaped@ character@]" pp_uchar u
      | `Lone_lo_surrogate p -> pp ppf "%a@ lone@ low@ surrogate@]" pp_cp p
      | `Lone_hi_surrogate p -> pp ppf "%a@ lone@ high@ surrogate@]" pp_cp p
      | `Not_lo_surrogate p -> pp ppf "%a@ not@ a@ low@ surrogate@]" pp_cp p)
  | `Illegal_string_uchar u ->
      pp ppf "@[illegal@ character@ in@ JSON@ string@ (%a)@]" pp_uchar u
  | `Illegal_bytes bs ->
      let l = String.length bs in
      pp ppf "@[illegal@ bytes@ in@ character@ stream@ (";
      if l > 0 then pp ppf "%02X" (Char.code bs.[0]);
      for i = 1 to l - 1 do
        pp ppf " %02X" (Char.code bs.[i])
      done;
      pp ppf ")@]"
  | `Illegal_number n -> pp ppf "@[illegal@ number@ (%s)@]" n
  | `Illegal_literal l -> pp ppf "@[illegal@ literal@ (%s)@]" l
  | `Unclosed r -> (
      pp ppf "@[unclosed@ ";
      match r with
      | `As -> pp ppf "array@]"
      | `Os -> pp ppf "object@]"
      | `String -> pp ppf "string@]"
      | `Comment -> pp ppf "comment@]")
  | `Expected r -> (
      pp ppf "@[expected@ ";
      match r with
      | `Comment -> pp ppf "JavaScript@ comment@]"
      | `Value -> pp ppf "JSON@ value@]"
      | `Name -> pp ppf "member@ name@]"
      | `Name_sep -> pp ppf "name@ separator@ (':')@]"
      | `Aval true -> pp ppf "value@ or@ array@ end@ (value@ or@ ']')@]"
      | `Aval false ->
          pp ppf "value@ separator@ or@ array@ end@ (','@ or@ ']')@]"
      | `Omem true -> pp ppf "member@ name@ or@ object@ end@ ('\"'@ or@ '}')@]"
      | `Omem false ->
          pp ppf "value@ separator@ or@ object@ end@ (','@ or@ '}')@]"
      | `Json -> pp ppf "JSON@ text (JSON value)@]"
      | `Eoi -> pp ppf "end@ of@ input@]")

type pos = int * int
type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]
type src = unit -> Cstruct.t
type decode = [ `Await | `End | `Lexeme of lexeme | `Error of error ]
type uncut = [ `Comment of [ `M | `S ] * string | `White of string ]

let pp_decode ppf = function
  | `Lexeme l -> pp ppf "@[`Lexeme @[(%a)@]@]" pp_lexeme l
  | `Await -> pp ppf "`Await"
  | `End -> pp ppf "`End"
  | `Error e -> pp ppf "@[`Error @[(%a)@]@]" pp_error e
  | `White s -> pp ppf "@[`White @[%S@]@]" s
  | `Comment (style, s) ->
      let pr_style ppf = function `M -> pp ppf "`M" | `S -> pp ppf "`S" in
      pp ppf "@[`Comment @[(%a, %S)@]@]" pr_style style s

type decoder = {
  u : Uutfe.decoder; (* Unicode character decoder. *)
  buf : Buffer.t; (* string accumulation buffer. *)
  mutable started : bool;
  mutable uncut : bool; (* [true] to bufferize comments and white space. *)
  mutable s_line : int; (* last saved start line. *)
  mutable s_col : int; (* last saved start column. *)
  mutable e_line : int; (* last saved end line. *)
  mutable e_col : int; (* last saved end column. *)
  mutable c : int; (* character lookahead. *)
  mutable stack :
    (* stack of open arrays and objects. *)
    [ `As of pos | `Os of pos ] list;
  mutable next_name : bool; (* [true] if next decode should be [`Name]. *)
  mutable last_start : bool; (* [true] if last lexeme was `As or `Os. *)
}

let baddc d c = Uutfe.Buffer.add_utf_8 d.buf (Uchar.unsafe_of_int c)
let badd d = Uutfe.Buffer.add_utf_8 d.buf (Uchar.unsafe_of_int d.c)

let buf d =
  let t = Buffer.contents d.buf in
  Buffer.clear d.buf;
  t

let dpos d = (Uutfe.decoder_line d.u, Uutfe.decoder_col d.u)

let spos d =
  d.s_line <- Uutfe.decoder_line d.u;
  d.s_col <- Uutfe.decoder_col d.u

let epos d =
  d.e_line <- Uutfe.decoder_line d.u;
  d.e_col <- Uutfe.decoder_col d.u

let stack_range d =
  match d.stack with
  | [] -> assert false
  | `As (l, c) :: _ | `Os (l, c) :: _ ->
      d.s_line <- l;
      d.s_col <- c;
      epos d

let dpop d =
  match
    spos d;
    epos d;
    d.stack
  with
  | _ :: (`Os _ :: _ as ss) ->
      d.next_name <- true;
      d.stack <- ss
  | _ :: (`As _ :: _ as ss) ->
      d.next_name <- false;
      d.stack <- ss
  | [ _ ] ->
      d.next_name <- false;
      d.stack <- []
  | [] -> assert false

let ret_eoi _d = `End

(* let ret (v : [< decode | uncut]) k d = d.k <- k; v *)
let readc d =
  match Uutfe.decode d.u with
  | `Uchar u ->
      debug_print (fun () -> Printf.printf "CHAR: %c\n" (Uchar.to_char u));
      d.c <- Uchar.to_int u
  | `End -> d.c <- ux_eoi
  | `Await -> assert false
  | `Malformed _bs ->
      d.c <- u_rep;
      epos d

(* ; ret (err_bytes bs) k d *)

(* let rec r_scomment d =               (* single line comment. // was eaten. *)
   if (d.c <> u_nl && d.c <> ux_eoi) then (badd d; readc d; r_scomment d) else
   (epos d; (`Comment (`S, buf d))) *)

(* let rec r_mcomment closing d =         (* multiline comment. /* was eaten. *)
   if (d.c = ux_eoi) then (epos d; err_unclosed_comment ) else
   if closing then begin
     if (d.c = u_slash) then (epos d; (`Comment (`M, buf d))) else
     if (d.c = u_times) then (badd d; readc d; r_mcomment true d) else
     (baddc d u_times; badd d; readc d; r_mcomment false d)
   end else begin
     if (d.c = u_times) then (readc d; r_mcomment true d) else
     (badd d; readc d; r_mcomment false d)
   end *)

(* let r_comment d =                                 (* comment, / was eaten. *)
     if d.c = u_slash then (readc d; r_scomment d) else
     if d.c = u_times then (readc d; r_mcomment false d) else
     (epos d; err_exp_comment)

   let rec r_ws_uncut d =
     if (is_white d.c) then (epos d; badd d; readc d; r_ws_uncut d) else
     (* ret (`White (buf d)) k d *)
     (* `White (buf d) *)
     Buffer.clear d.buf;
     ()

   let rec r_white_uncut d =                                (* {ws} / comment *)
     (* Not sure about this! *)
     if (is_white d.c) then (spos d; r_ws_uncut d; ) else
     if (d.c = u_slash) then (spos d; readc d; r_comment d) else
     (* r_white_uncut d *) ()
*)

let rec r_ws d =
  while is_white d.c do
    readc d;
    r_ws d
  done
(* {ws} *)

(* TODO: Add uncut *)
let r_white d = r_ws d

let rec r_u_escape hi u count d =
  (* unicode escapes. *)
  let error err d =
    baddc d u_rep;
    err
  in
  if count > 0 then (
    if not (is_hex_digit d.c) then (
      epos d;
      ignore @@ error (err_not_hex d.c) d;
      readc d)
    else
      let u =
        (u * 16)
        +
        if d.c <= 0x39 (* 9 *) then d.c - 0x30
        else if d.c <= 0x46 (* F *) then d.c - 0x37
        else d.c - 0x57
      in
      epos d;
      readc d;
      ignore @@ (r_u_escape hi u (count - 1)) d)
  else
    match hi with
    | Some hi ->
        (* combine high and low surrogate into scalar value. *)
        if u < 0xDC00 || u > 0xDFFF then ignore @@ error (err_not_lo u) d
        else
          let u = (((hi land 0x3FF) lsl 10) lor (u land 0x3FF)) + 0x10000 in
          baddc d u (* error (err_lone_lo u) d *)
    | None ->
        if u < 0xD800 || u > 0xDFFF then ignore @@ error (err_lone_lo u) d
        else if u > 0xDBFF then ignore @@ error (err_lone_lo u) d
        else if d.c <> u_bslash then ignore @@ error (err_lone_hi u) d
        else (
          readc d;
          (fun d ->
            if d.c <> 0x75 (* u *) then ignore @@ error (err_lone_hi u) d
            else (
              readc d;
              (r_u_escape (Some u) 0 4) d))
            d)

and r_escape d =
  match d.c with
  | 0x22 (* '' *) ->
      baddc d u_quot;
      readc d
  | 0x5C (* \ *) ->
      baddc d u_bslash;
      readc d
  | 0x2F (* / *) ->
      baddc d u_slash;
      readc d
  | 0x62 (* b *) ->
      baddc d 0x08;
      readc d
  | 0x66 (* f *) ->
      baddc d 0x0C;
      readc d
  | 0x6E (* n *) ->
      baddc d u_nl;
      readc d
  | 0x72 (* r *) ->
      baddc d 0x0D;
      readc d
  | 0x74 (* t *) ->
      baddc d 0x09;
      readc d
  | 0x75 (* u *) ->
      readc d;
      r_u_escape None 0 4 d
  | c ->
      epos d;
      baddc d u_rep;
      ignore (err_not_esc c);
      readc d

let rec r_string d =
  (* {string}, '' eaten. *)
  if d.c = ux_eoi then (
    epos d;
    ignore err_unclosed_string;
    ignore @@ ret_eoi d)
  else if not (must_escape d.c) then (
    badd d;
    readc d;
    r_string d)
  else if d.c = u_quot then (
    epos d;
    readc d)
  else if d.c = u_bslash then (
    readc d;
    r_escape d;
    r_string d)
  else (
    epos d;
    baddc d u_rep;
    let e = err_str_char d.c in
    readc d;
    r_string d;
    ignore e)

let rec r_float d =
  (* {number} *)
  if (not (is_val_sep d.c)) && d.c <> ux_eoi then (
    epos d;
    badd d;
    readc d;
    r_float d)
  else
    let s = buf d in
    try `Lexeme (`Float (float_of_string s)) with Failure _ -> err_number s

let rec r_literal d =
  (* {true} / {false} / {null} *)
  if (not (is_val_sep d.c)) && d.c <> ux_eoi then (
    epos d;
    badd d;
    readc d;
    r_literal d)
  else
    match buf d with
    | "true" -> `Lexeme (`Bool true)
    | "false" -> `Lexeme (`Bool false)
    | "null" -> `Lexeme `Null
    | s -> err_literal s

let r_value err d =
  match d.c with
  (* {value} *)
  | 0x5B (* [ *) ->
      (* {begin-array} *)
      spos d;
      epos d;
      d.last_start <- true;
      d.stack <- `As (dpos d) :: d.stack;
      (* ret (`Lexeme `As) (readc k) d *)
      let v = `Lexeme `As in
      readc d;
      v
  | 0x7B (* { *) ->
      (* {begin-object} *)
      spos d;
      epos d;
      d.last_start <- true;
      d.next_name <- true;
      d.stack <- `Os (dpos d) :: d.stack;
      (* ret (`Lexeme `Os) (readc k) d *)
      let v = `Lexeme `Os in
      readc d;
      v
  | 0x22 (* '' *) ->
      let lstring d = `Lexeme (`String (buf d)) in
      spos d;
      readc d;
      r_string d;
      lstring d
  | 0x66 (* f *) | 0x6E (* n *) | 0x74 (* t *) ->
      spos d;
      r_literal d
  | u when is_digit u || u = u_minus ->
      spos d;
      r_float d
  | _u -> err d

let rec discard_to c1 c2 err d =
  Printf.printf "%c %c\n" (Char.chr c1) (Char.chr c2);
  if d.c = c1 || d.c = c2 || d.c = ux_eoi then err
  else (
    epos d;
    readc d;
    (discard_to c1 c2 err) d)

let r_arr_val d =
  (* [{value-separator}] {value} / {end-array} *)
  let nxval err d =
    spos d;
    discard_to u_comma u_rbrack err d
  in
  let last_start = d.last_start in
  d.last_start <- false;
  if d.c = ux_eoi then (
    stack_range d;
    err_unclosed_arr)
  else if d.c = u_rbrack then (
    dpop d;
    readc d;
    `Lexeme `Ae)
  else if last_start then r_value (nxval err_exp_arr_fst) d
  else if d.c = u_comma then (
    readc d;
    r_white d;
    r_value (nxval err_exp_value) d)
  else nxval err_exp_arr_nxt d

let nxmem err d =
  (* Printf.printf "NXM: %c" (Char.chr d.c); *)
  spos d;
  d.next_name <- true;
  discard_to u_comma u_rbrace err d

let r_obj_value d =
  (* {name-separator} {value} *)
  d.next_name <- true;
  if d.c = u_colon then (
    readc d;
    r_white d;
    r_value (nxmem err_exp_value) d)
  else nxmem err_exp_nsep d

let r_obj_name d =
  (* [{value-separator}] string / end-object *)
  let r_name err d =
    let ln d =
      let s = buf d in
      `Lexeme (`Name s)
    in
    if d.c <> u_quot then nxmem err d
    else (
      spos d;
      readc d;
      r_string d;
      ln d)
  in
  let last_start = d.last_start in
  d.last_start <- false;
  d.next_name <- false;
  if d.c = ux_eoi then (
    stack_range d;
    err_unclosed_obj)
  else if d.c = u_rbrace then (
    dpop d;
    readc d;
    `Lexeme `Oe)
  else if last_start then r_name err_exp_obj_fst d
  else if d.c = u_comma then (
    readc d;
    r_white d;
    r_name err_exp_name d)
  else nxmem err_exp_obj_nxt d

let r_end d =
  (* end of input *)
  if d.c = ux_eoi then ret_eoi d
  else
    let drain d =
      spos d;
      discard_to ux_eoi ux_eoi err_exp_eoi d
    in
    drain d

let r_lexeme d =
  match d.stack with
  | `As _ :: _ ->
      r_white d;
      r_arr_val d
  | `Os (_i, _j) :: _ ->
      if d.next_name then (
        r_white d;
        r_obj_name d)
      else (
        r_white d;
        r_obj_value d)
  | [] ->
      r_white d;
      r_end d

let discard_to_white d =
  while not (is_white d.c || d.c = ux_eoi) do
    epos d;
    readc d
  done

let rec r_json d =
  (* {value} *)
  let err d =
    spos d;
    discard_to_white d;
    r_white d;
    r_json d
  in
  if d.c <> ux_eoi then r_value err d else err_exp_json

let r_start d =
  (* start of input *)
  let bom d =
    if Uutfe.decoder_removed_bom d.u then err_bom
    else (
      r_white d;
      r_json d)
  in
  readc d;
  bom d

let nln = `ASCII (Uchar.unsafe_of_int 0x000A)

let decoder ?encoding src =
  let u = Uutfe.decoder ?encoding ~nln src in
  {
    u;
    buf = Buffer.create 1024;
    uncut = false;
    started = false;
    s_line = 1;
    s_col = 0;
    e_line = 1;
    e_col = 0;
    c = ux_soi;
    next_name = false;
    last_start = false;
    stack = [];
  }

let decode_uncut d =
  d.uncut <- true;
  r_start d

let decode d =
  match
    d.uncut <- false;
    if d.started then r_lexeme d
    else (
      d.started <- true;
      r_start d)
  with
  | #decode as v -> (v :> [> decode ])
  | `Comment _ | `White _ -> assert false

let decoder_src d = Uutfe.decoder_src d.u
let decoded_range d = ((d.s_line, d.s_col), (d.e_line, d.e_col))

let decoder_encoding d =
  match Uutfe.decoder_encoding d.u with
  | #encoding as enc -> enc
  | `US_ASCII | `ISO_8859_1 -> assert false

(* Encode *)

let invalid_arg fmt =
  let b = Buffer.create 20 in
  (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf =
    Format.pp_print_flush ppf ();
    invalid_arg (Buffer.contents b)
  in
  Format.kfprintf k ppf fmt

let invalid_bounds j l = invalid_arg "invalid bounds (index %d, length %d)" j l
let expect e v = invalid_arg "%a encoded but expected %s" pp_decode v e
let expect_end l = expect "`End" (`Lexeme l)
let expect_mem_value l = expect "any `Lexeme but `Name, `Oe or `Ae" (`Lexeme l)
let expect_arr_value_ae l = expect "any `Lexeme but `Name or `Oe" (`Lexeme l)
let expect_name_or_oe l = expect "`Lexeme (`Name _ | `Oe)" (`Lexeme l)

let expect_json v =
  expect "`Lexeme (`Null | `Bool _ | `Float _ | `String _ | `As | `Os)" v

let expect_lend lstart v =
  expect (if lstart = `As then "`Lexeme `Ae" else "`Lexeme `Oe") v

type dst = Cstruct.t -> unit
type encode = [ `Await | `End | `Lexeme of lexeme ]

type encoder = {
  dst : dst; (* output destination. *)
  minify : bool; (* [true] for compact output. *)
  mutable encode_ : bool;
  mutable started : bool;
  mutable o : Cstruct.t; (* current output chunk. *)
  mutable o_pos : int; (* next output position to write. *)
  mutable o_max : int; (* maximal output position to write. *)
  buf : Buffer.t; (* buffer to format floats. *)
  mutable stack : [ `As | `Os ] list; (* stack of open arrays and objects. *)
  mutable nest : int; (* nesting level (String.length stack). *)
  mutable next_name : bool; (* [true] if next encode should `Name. *)
  mutable last_start : bool; (* [true] if last encode was [`As | `Os]. *)
}

let o_rem e = e.o_max - e.o_pos + 1 (* remaining bytes to write in [e.o]. *)

let dst e s j l =
  (* set [e.o] with [s]. *)
  if j < 0 || l < 0 || j + l > Cstruct.length s then invalid_bounds j l;
  e.o <- s;
  e.o_pos <- j;
  e.o_max <- j + l - 1

(* let partial k e = function `Await -> k e | v -> expect_await v *)
let flush e ~stop =
  if stop then (
    if e.o_pos <> 0 then e.dst (Cstruct.sub e.o 0 e.o_pos);
    e.dst Cstruct.empty)
  else e.dst (Cstruct.sub e.o 0 e.o_pos);
  e.o_pos <- 0

let rec writeb b e =
  (* write byte [b] and [k]ontinue. *)
  if e.o_pos > e.o_max then (
    flush e ~stop:false;
    writeb b e)
  else (
    unsafe_set_byte e.o e.o_pos b;
    e.o_pos <- e.o_pos + 1)

let rec writes s j l e =
  (* write [l] bytes from [s] starting at [j]. *)
  let rem = o_rem e in
  if rem >= l then (
    unsafe_blit s j e.o e.o_pos l;
    e.o_pos <- e.o_pos + l)
  else (
    unsafe_blit s j e.o e.o_pos rem;
    e.o_pos <- e.o_pos + rem;
    flush e ~stop:false;
    writes s (j + rem) (l - rem) e)

let rec writebuf j l e =
  (* write [l] bytes from [e.buf] starting at [j]. *)
  let rem = o_rem e in
  if rem >= l then (
    Cstruct.blit_from_bytes (Buffer.to_bytes e.buf) j e.o e.o_pos l;
    e.o_pos <- e.o_pos + l)
  else (
    Cstruct.blit_from_bytes (Buffer.to_bytes e.buf) j e.o e.o_pos rem;
    e.o_pos <- e.o_pos + rem;
    flush e ~stop:false;
    writebuf (j + rem) (l - rem) e)

let w_indent e =
  let rec loop indent e =
    let spaces e indent =
      let max = e.o_pos + indent - 1 in
      for j = e.o_pos to max do
        Cstruct.set_uint8 e.o j u_sp
      done;
      e.o_pos <- max + 1
    in
    let rem = o_rem e in
    if rem < indent then (
      spaces e rem;
      flush e ~stop:false;
      loop (indent - rem) e)
    else spaces e indent
  in
  loop (e.nest * 2) e

let w_json_string s e =
  (* escapes as mandated by the standard. *)
  let rec loop s j pos max e =
    if pos > max then if j > max then () else writes s j (pos - j) e
    else
      let next = pos + 1 in
      let escape esc =
        (* assert (String.length esc = 2 ). *)
        writes s j (pos - j) e;
        writes esc 0 2 e;
        loop s next next max e
      in
      match unsafe_byte s pos with
      | 0x22 -> escape "\\\""
      | 0x5C -> escape "\\\\"
      | 0x0A -> escape "\\n"
      | c when c <= 0x1F ->
          let hex d = if d < 10 then 0x30 + d else 0x41 + (d - 10) in
          writes s j (pos - j) e;
          writes "\\u00" 0 4 e;
          writeb (hex (c lsr 4)) e;
          writeb (hex (c land 0xF)) e;
          loop s next next max e
      | _c -> loop s j next max e
  in
  writeb u_quot e;
  loop s 0 0 (String.length s - 1) e;
  writeb u_quot e

let w_name n e =
  e.last_start <- false;
  e.next_name <- false;
  w_json_string n e;
  writeb u_colon e

let w_value ~in_obj l e =
  match l with
  | `String s ->
      e.last_start <- false;
      e.next_name <- in_obj;
      w_json_string s e
  | `Bool b ->
      e.last_start <- false;
      e.next_name <- in_obj;
      if b then writes "true" 0 4 e else writes "false" 0 5 e
  | `Float f ->
      e.last_start <- false;
      e.next_name <- in_obj;
      Buffer.clear e.buf;
      Printf.bprintf e.buf "%.16g" f;
      writebuf 0 (Buffer.length e.buf) e
  | `Os ->
      e.last_start <- true;
      e.next_name <- true;
      e.nest <- e.nest + 1;
      e.stack <- `Os :: e.stack;
      writeb u_lbrace e
  | `As ->
      e.last_start <- true;
      e.next_name <- false;
      e.nest <- e.nest + 1;
      e.stack <- `As :: e.stack;
      writeb u_lbrack e
  | `Null ->
      e.last_start <- false;
      e.next_name <- in_obj;
      writes "null" 0 4 e
  | (`Oe | `Ae | `Name _) as l ->
      if in_obj then expect_mem_value l else expect_arr_value_ae l

let w_lexeme e l =
  let epop e =
    e.last_start <- false;
    e.nest <- e.nest - 1;
    e.stack <- List.tl e.stack;
    match e.stack with
    | `Os :: _ -> e.next_name <- true
    | _ -> e.next_name <- false
  in
  match List.hd e.stack with
  | `Os -> (
      if (* inside object. *)
         not e.next_name then w_value ~in_obj:true l e
      else
        match l with
        | `Name n ->
            let name n e =
              if e.minify then w_name n e
              else (
                writeb u_nl e;
                w_indent e;
                w_name n e;
                writeb u_sp e)
            in
            if e.last_start then name n e
            else (
              writeb u_comma e;
              name n e)
        | `Oe ->
            if e.minify || e.last_start then (
              epop e;
              writeb u_rbrace e)
            else (
              epop e;
              writeb u_nl e;
              w_indent e;
              writeb u_rbrace e)
        | _v -> expect_name_or_oe l)
  | `As -> (
      (* inside array. *)
      match l with
      | `Ae ->
          if e.minify || e.last_start then (
            epop e;
            writeb u_rbrack e)
          else (
            epop e;
            writeb u_nl e;
            w_indent e;
            writeb u_rbrack e)
      | l ->
          let value l e =
            if e.minify then w_value ~in_obj:false l e
            else (
              writeb u_nl e;
              w_indent e;
              w_value ~in_obj:false l e)
          in
          if e.last_start then value l e
          else (
            writeb u_comma e;
            value l e))

let encode_ e = function
  | `Lexeme l -> if e.stack = [] then expect_end l else w_lexeme e l
  | `End as v ->
      if e.stack = [] then flush e ~stop:true
      else expect_lend (List.hd e.stack) v
  | `White w -> writes w 0 (String.length w) e
  | `Comment (`S, c) ->
      writes "//" 0 2 e;
      writes c 0 (String.length c) e;
      writeb u_nl e
  | `Comment (`M, c) ->
      writes "/*" 0 2 e;
      writes c 0 (String.length c) e;
      writes "*/" 0 2 e
  | `Await -> ()

(* let rec encode_loop e = e.k <- encode_ encode_loop; `Ok *)
let encode_json e = function
  (* first [k] to start with [`Os] or [`As]. *)
  | `Lexeme ((`Null | `Bool _ | `Float _ | `String _ | `As | `Os) as l) ->
      w_value ~in_obj:false l e;
      e.encode_ <- true
  | (`End | `Lexeme _) as v -> expect_json v
  | (`White _ | `Comment _) as v ->
      encode_ e v;
      e.encode_ <- false
  | `Await -> ()

let encoder ?(minify = true) dst =
  let o = Cstruct.create io_buffer_size in
  let o_max = Cstruct.length o - 1 in
  if o_max = 0 then invalid_arg "buf's length is empty"
  else
    {
      dst;
      minify;
      o;
      o_pos = 0;
      o_max;
      buf = Buffer.create 30;
      encode_ = false;
      started = false;
      stack = [];
      nest = 0;
      next_name = false;
      last_start = false;
    }

let encode e v =
  if (not e.started) || not e.encode_ then (
    e.started <- true;
    encode_json e (v :> [ encode | uncut ]);
    `Ok)
  else (
    encode_ e v;
    `Ok)

let encoder_dst e = e.dst
let encoder_minify e = e.minify

(* Manual *)

module Manual = struct
  let src d = Uutfe.Manual.src d.u
  let dst = dst
  let dst_rem = o_rem
end

(* Uncut *)

module Uncut = struct
  let decode = decode_uncut
  let pp_decode = pp_decode
  let encode e v = encode e (v :> [ encode | uncut ])
end

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
