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

type ('param, 'result) rendering =
  [ `Immediate of 'param -> 'result
  | `Running of 'param -> 'result Lwt.t ]

type component_constructor =
  page_id: page id ->
  component_id: component id ->
  profile_id: profile id ->
  component ->
  [ (unit, Dom_html.element Js.t) rendering
  | `Default ]

type component_destructor =
  page_id: page id ->
  component_id: component id ->
  component ->
  [ (Dom_html.element Js.t, unit) rendering
  | `Default ]

type component_rebinder =
  page_id: page id ->
  component_id: component id ->
  previous_profile_id: profile id ->
  new_profile_id: profile id ->
  component ->
  [ (Dom_html.element Js.t, Dom_html.element Js.t) rendering
  | `Default
  | `Reconstruct ]

type rendered =
  { element : Dom_html.element Js.t ;
    kind : [ `Container of container | `Component of component ] }

type container_constructor =
  page_id: page id ->
  container_id: container id ->
  profile_id: profile id ->
  container ->
  [ (rendered list, Dom_html.element Js.t) rendering
  | `Default ]

type container_destructor =
  page_id: page id ->
  container_id: container id ->
  container ->
  [ (Dom_html.element Js.t * rendered list, unit) rendering
  | `Default ]

type container_rebinder =
  page_id: page id ->
  container_id: container id ->
  previous_profile_id: profile id ->
  new_profile_id: profile id ->
  container ->
  [ (Dom_html.element Js.t * rendered list, Dom_html.element Js.t) rendering
  | `Default
  | `Reconstruct ]

val window :
  ?main_container_id: string ->
  ?construct_component : component_constructor ->
  ?destruct_component : component_destructor ->
  ?rebind_component : component_rebinder ->
  ?construct_container : container_constructor ->
  ?destruct_container : container_destructor ->
  ?rebind_container : container_rebinder ->
  ui -> context

val div :
  Dom_html.divElement Js.t ->
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

val render : context ->
  page_id: page id ->
  profile_id: profile id ->
  ?transitions: ((unit -> unit Lwt.t) * (unit -> unit Lwt.t)) ->
  unit -> unit Lwt.t
