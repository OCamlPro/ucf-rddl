(* RDDL - Browser introspection demo *)

(************************************************************************)
(*  RDDL - reactive design description language                         *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  RDDL (pronounce riddle) is distributed in the hope that it will be  *)
(*  useful, but WITHOUT ANY WARRANTY; without even the implied warranty *)
(*  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.             *)
(*  See the GNU General Public License for more details.                *)
(*                                                                      *)
(************************************************************************)

open Rddl_ast
open Rddl_profile

let pretty_print_profile profile =
  let open Format in
  let rec pp_range pp ppf = function
    | { min = None ; max = None } ->
      fprintf ppf "any"
    | { min = Some (min, `Closed) ; max = Some (max, `Closed) } when min = max ->
      fprintf ppf "only %a" pp min
    | { min = Some min ; max = Some max } ->
      fprintf ppf "%a &&& %a"
        (pp_range pp) { min = Some min ; max = None }
        (pp_range pp) { max = Some max ; min = None }
    | { min = Some (min, `Closed) ; max = None } ->
      fprintf ppf "from %a" pp min
    | { min = Some (min, `Open) ; max = None } ->
      fprintf ppf "above %a" pp min
    | { min = None ; max = Some (max, `Closed) } ->
      fprintf ppf "upto %a" pp max
    | { min = None ; max = Some (max, `Open) } ->
      fprintf ppf "below %a" pp max in
  let pp_output_level ppf = function
    | Textual -> fprintf ppf "Textual"
    | Simplified -> fprintf ppf "Simplified"
    | Fancy -> fprintf ppf "Fancy" in
  let pp_interactivity_level ppf = function
    | View_only -> fprintf ppf "View_only"
    | Pointer -> fprintf ppf "Pointer"
    | Single_touch -> fprintf ppf "Single_touch"
    | Multi_touch -> fprintf ppf "Multi_touch" in
  let pp_three_steps_level ppf = function
    | Low -> fprintf ppf "Low"
    | Normal -> fprintf ppf "Normal"
    | High -> fprintf ppf "High" in
  asprintf
    "@[<v 2>{ \
     output = %a ;@,\
     interactivity = %a ;@,\
     display_width = %a ;@,\
     physical_display_width = %a ;@,\
     display_aspect_ratio = %a ;@,\
     device_width = %a ;@,\
     physical_device_width = %a ;@,\
     device_aspect_ratio = %a ;@,\
     contrast = %a ;@,\
     ink = %a ;@,\
     zoom = %a }@]"
    (pp_range pp_output_level) profile.output
    (pp_range pp_interactivity_level) profile.interactivity
    (pp_range (fun ppf -> fprintf ppf "%d")) profile.display_width
    (pp_range (fun ppf -> fprintf ppf "%d")) profile.physical_display_width
    (pp_range (fun ppf -> fprintf ppf "%.3f")) profile.display_aspect_ratio
    (pp_range (fun ppf -> fprintf ppf "%d")) profile.device_width
    (pp_range (fun ppf -> fprintf ppf "%d")) profile.physical_device_width
    (pp_range (fun ppf -> fprintf ppf "%.3f")) profile.device_aspect_ratio
    (pp_range pp_three_steps_level) profile.contrast
    (pp_range pp_three_steps_level) profile.ink
    (pp_range pp_three_steps_level) profile.zoom

let profiles =
  [ "small-vertical",
    profile
      ~display_width: (below 400)
      ~display_aspect_ratio: (upto 1.) () ;
    "medium-vertical",
    profile
      ~display_width: (from 400 &&& below 768)
      ~display_aspect_ratio: (upto 1.) () ;
    "large-vertical",
    profile
      ~display_width: (from 768)
      ~display_aspect_ratio: (upto 1.) () ;
    "small-horizontal",
    profile
      ~display_width: (below 400)
      ~display_aspect_ratio: (from 1.) () ;
    "medium-horizontal",
    profile
      ~display_width: (from 400 &&& below 768)
      ~display_aspect_ratio: (from 1.) () ;
    "large-horizontal",
    profile
      ~display_width: (from 768)
      ~display_aspect_ratio: (from 1.) () ]

let () =
  let updates = Rddl_profiler.window in
  let changes = Rddl_profiler.changes updates profiles in
  Rddl_profiler.on_update updates @@ fun profile ->
  let (id, profile) = Rddl_profiler.selection changes in
  let text =
    List.fold_left
      (fun acc (pid, _) ->
         if pid = id then
           acc ^"\n> " ^ pid
         else
           acc ^"\n  " ^ pid)
      (pretty_print_profile profile ^ "\n") profiles in
  Js.Opt.iter
    (Dom_html.window##document##querySelector (Js.string "#output"))
    (fun elt ->
       let text = Dom_html.window##document##createTextNode (Js.string text) in
       elt##innerHTML <- Js.string "" ;
       ignore (elt##appendChild ((text :> Dom.node Js.t)))) ;
  Lwt.return ()
