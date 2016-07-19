(* RDDL - Page rendering *)

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

type context =
  { root : Dom_html.element Js.t ;
    components : string * string * Dom_html.element Js.t list ;
    containers : string * string * Dom_html.element Js.t list }

let register_component_constructor ~construct ~destruct name =
  assert false

let register_container_constructor ~construct ~destruct name =
  assert false

let window ui =
  assert false

let render ctx page profile =
  assert false
