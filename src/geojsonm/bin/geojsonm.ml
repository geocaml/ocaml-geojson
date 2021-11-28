(* Copyright (c) 2021 Patrick Ferris <patrick@sirref.org>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
*)
open Cmdliner

type actions = [ `Scale of float | `Shift of float (* | `Precision of float *) ]

let scale_term =
  Arg.value
  @@ Arg.(opt (some float) None)
  @@ Arg.info ~doc:"Scale all coordinates by a given factor" ~docv:"SCALE"
       [ "scale" ]

let shift_term =
  Arg.value
  @@ Arg.(opt (some float) None)
  @@ Arg.info ~doc:"Shift all coordinates by a given factor" ~docv:"SHIFT"
       [ "shift" ]

let action_to_function = function
  | `Scale f -> fun v -> f *. v
  | `Shift f -> fun v -> f +. v
(* | `Precision f -> fun v -> f *. v TODO *)

(* let apply_actions actions src =
   let functions = List.map action_to_function actions in
   Geojsonm.map_coords ~f:(fun f -> List.fold_left (fun facc f' -> f' facc) f functions) src *)
