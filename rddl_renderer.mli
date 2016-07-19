(** RDDL - Page rendering *)

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

type context

val register_component_constructor :
  construct:
    (Json_repr_browser.value option -> Dom_html.element Js.t -> unit Lwt.t) ->
  destruct:
    (Dom_html.element Js.t -> unit Lwt.t) ->
  string -> unit

val register_container_constructor :
  construct:
    (Json_repr_browser.value option -> Dom_html.element Js.t -> Dom_html.element Js.t list -> unit Lwt.t) ->
  destruct:
    (Dom_html.element Js.t -> unit Lwt.t) ->
  string -> unit

val window : ui -> context

val render : context -> page id -> profile id -> unit Lwt.t