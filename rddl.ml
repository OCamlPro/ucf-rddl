(** AST *)

(************************************************************************)
(*  rddl - reactive design description language                         *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  ocp-read is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

type ui =
  { pages : page table ;
    profiles : context table }

and page =
  { views : view list ;
    components : (component * flags) table ;
    containers : (container * flags) table ;
    sources : source table }

and view =
  { document : element ;
    compatible_profiles : string list }

and container =
  { extensible : bool }

and component =
  { ratio : float range }

and flags =
  | Required
  | Hideable
  | Optional

and source =
  | Uri of string
  | Custom

and 'a table =
  (string * 'a) list

(** Rules to describe the context profiles. *)
and context =
  | And of context list
  (** This rule applies if all its sub-rules apply. *)
  | Or of context list
  (** This rule applies if any of its sub-rules apply. *)
  (* Capabilities *)
  | Output of [ `textual | `simplified | `fancy ] range
  | Interactivity of [ `view_only | `pointer | `single_touch | `multi_touch  ]
  | Size of [ `display_width | `display_height | `device_width | `device_height ] * float range
  | Aspect_ratio of [ `device | `display ] * float range (* width / height *)
  | Resolution of float range (* dots per inch *)
  (* Accessibility *)
  | Contrast of [ `low | `normal | `high ] range
  | Ink of [ `low | `normal | `high ] range
  | Zoom of [ `low | `normal | `high ] range
  (* TODO: colors *)
  (* Environment *)
  | Connected of source * bool
  | Bandwitdh of source * int range (* kilobits per second *)

and 'a range =
  { min : 'a option ;
    max : 'a option }

and element =
  | Vertical of element list * string option
  | Horizontal of element list * string option
  | Flow of element list * string option
  | Menu of element list * string option
  | Label of string * string option
  | Custom_component of string
  | Custom_container of element list * string

(* Voir: material UI *)

(*
open Json_encoding

let element_encoding =
  mu "element" @@ fun self ->
  let component_encoding =
    conv
      (fun { name; hideable; optional } -> (name, hideable, optional))
      (fun (name, hideable, optional) -> { name; hideable; optional }) @@
    obj3
      (req "name" string)
      (req "hideable" bool)
      (req "optional" bool) in
  let properties_encoding =
    conv
      (fun { children; dynamic; component } -> (children, dynamic, component))
      (fun (children, dynamic, component) -> { children; dynamic; component }) @@
    obj3
      (req "children" (list self))
      (req "dynamic" bool)
      (opt "component" component_encoding) in
  let with_kind kind obj =
    conv (fun o -> ((), o)) (fun ((), o) -> o) @@
    merge_objs (obj1 (req "kind" (string_enum [ kind, () ]))) obj in
  union
    [ case
        (with_kind "vertical" properties_encoding)
        (function Vertical props -> Some props | _ -> None)
        (fun props -> Vertical props) ;
      case
        (with_kind "horizontal" properties_encoding)
        (function Horizontal props -> Some props | _ -> None)
        (fun props -> Horizontal props) ;
      case
        (with_kind "menu" properties_encoding)
        (function Menu props -> Some props | _ -> None)
        (fun props -> Menu props) ;
      case
        (with_kind "component" component_encoding)
        (function Component props -> Some props | _ -> None)
        (fun props -> Component props) ]

let () =
  Json_encoding.schema element_encoding |>
  Json_schema.to_json |>
  Ezjsonm.to_channel ~minify:false stdout
*)
