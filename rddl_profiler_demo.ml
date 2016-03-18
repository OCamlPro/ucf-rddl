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

let pretty_print_profile profile =
  let open Format in
  let open Rddl_ast in
  let pp_range pp ppf = function
    | { min = None ; max = None } ->
      fprintf ppf "any"
    | { min = Some min ; max = Some max } when min = max ->
      fprintf ppf "only %a" pp min
    | { min = Some min ; max = Some max } ->
      fprintf ppf "between %a %a" pp min pp max
    | { min = Some min ; max = None } ->
      fprintf ppf "from %a" pp min
    | { min = None ; max = Some max } ->
      fprintf ppf "upto %a" pp max in
  let pp_output_level ppf = function
    | Textual ->
      fprintf ppf "Textual"
    | Simplified ->
      fprintf ppf "Simplified"
    | Fancy ->
      fprintf ppf "Fancy" in
  let pp_interactivity_level ppf = function
    | View_only ->
      fprintf ppf "View_only"
    | Pointer ->
      fprintf ppf "Pointer"
    | Single_touch ->
      fprintf ppf "Single_touch"
    | Multi_touch ->
      fprintf ppf "Multi_touch" in
  let pp_three_steps_level ppf = function
    | Low ->
      fprintf ppf "Low"
    | Normal ->
      fprintf ppf "Normal"
    | High ->
      fprintf ppf "High" in
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

let () =
  ignore @@ Rddl_profiler.on_update @@ fun profile ->
  let text = pretty_print_profile profile in
  Js.Opt.iter
    (Dom_html.window##document##querySelector (Js.string "#output"))
    (fun elt ->
       let text = Dom_html.window##document##createTextNode (Js.string text) in
       elt##innerHTML <- Js.string "" ;
       ignore (elt##appendChild ((text :> Dom.node Js.t))))
