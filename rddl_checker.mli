(** RDDL - Checker *)

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

exception Duplicate_profile_id of profile id
exception Duplicate_page_id of page id
exception Duplicate_component_id of page id * component id
exception Duplicate_container_id of page id * container id
exception Profile_handled_by_two_views of page id * profile id * int * int
exception Profile_not_handled of page id * profile id
exception Clashing_profiles of profile id * profile id * profile
exception Unkown_profile of page id * int * profile id

val check : ui -> exn list
