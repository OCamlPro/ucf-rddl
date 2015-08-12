(* AST *)

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

type screen =
  { name : string ;
    components : component list ;
    views : view list ;
    sources : string list }

and view =
  { name : string ;
    hierarchy : hierarchy ;
    context : context }

and component =
  { name : string ;
    hideable : bool ;
    optional : bool ;
    container : bool }

and context =
  (* Meta *)
  | And of context list
  | Or of context list
  (* Capabilities *)
  | Output of [ `textual | `simplified | `standard | `fancy ] range (* remove 1 ? *)
  | Interactivity of [ `view_only | `pointer | `single_touch | `multi_touch  ]
  | Size of [ `display_width | `display_height | `device_width | `device_height ] * float range
  | Aspect_ratio of [ `device | `display ] * float range (* width / height *)
  | Resolution of float range (* dots per inch *)
  (* Accessibility *)
  | Contrast of [ `low | `normal | `high ] range (* colorblind ? *)
  | Ink of [ `low | `normal | `high ] range
  | Zoom of [ `low | `normal | `high ] range
  (* Environment *)
  | Connected of source * bool
  | Bandwitdh of int range (* kilobits per second *) (* virtual ? which component decides ?*)

and 'a range =
  { min : 'a option ;
    max : 'a option }

and structure =
  { kind : component ;
    children : structure list }

and kind =
  | Vertical_split
  | Horizontal_split
  | Custom of string

open Json_encoding

let kind_json_value_encoding : kind value_encoding =
  value_encoding @@ string_enum
    [ "vertical-split", Vertical_split ;
      "horizontal-split", Horizontal_split ]
