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

type component_constructor =
  page: page id ->
  id: component id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  profile: profile id ->
  unit ->
  [ `Constructed of Dom_html.element Js.t | `Default ] Lwt.t

type component_destructor =
  page: page id ->
  id: component id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  Dom_html.element Js.t ->
  [ `Destructed | `Default ] Lwt.t

type component_rebinder =
  page: page id ->
  id: component id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  previous_profile: profile id ->
  new_profile: profile id ->
  Dom_html.element Js.t ->
  unit ->
  [ `Rebound | `Reconstruct | `Default ] Lwt.t

type container_constructor =
  page: page id ->
  id: container id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  profile: profile id ->
  Dom_html.element Js.t list ->
  [ `Constructed of Dom_html.element Js.t | `Default ] Lwt.t

type container_destructor =
  page: page id ->
  id: container id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  Dom_html.element Js.t ->
  [ `Destructed | `Default ] Lwt.t

type container_rebinder =
  page: page id ->
  id: container id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  previous_profile: profile id ->
  new_profile: profile id ->
  Dom_html.element Js.t ->
  Dom_html.element Js.t list ->
  [ `Rebound | `Reconstruct | `Default ] Lwt.t

val window :
  ?main_container_id: string ->
  ?construct_component : component_constructor ->
  ?destruct_component : component_destructor ->
  ?rebind_component : component_rebinder ->
  ?construct_container : container_constructor ->
  ?destruct_container : container_destructor ->
  ?rebind_container : container_rebinder ->
  ui -> context

val register_global_component_constructor :
  component_constructor -> unit

val register_global_component_destructor :
  component_destructor -> unit

val register_global_component_rebinder :
  component_rebinder -> unit

val register_global_container_constructor :
  container_constructor -> unit

val register_global_container_destructor :
  container_destructor -> unit

val register_global_container_rebinder :
  container_rebinder -> unit

val render : context -> page id -> profile id -> unit Lwt.t
