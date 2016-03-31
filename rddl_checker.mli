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

(** A profile ID is used twice. *)
exception Duplicate_profile of profile id

(** Two profiles overlap.
    Gives the computed conflicting intersection. *)
exception Clashing_profiles of profile id * profile id * profile

(** A page ID is used twice. *)
exception Duplicate_page of page id

(** A component ID is used twice inside a page. *)
exception Duplicate_component of page id * component id

(** A container ID is used twice inside a page. *)
exception Duplicate_container of page id * container id

(** A required component is missing inside a view. *)
exception Missing_component of page id * int * component id

(** A required container is missing inside a view. *)
exception Missing_container of page id * int * container id

(** An undeclared component ID appears inside a view. *)
exception Unknown_component of page id * int * component id

(** An undeclared container ID appears inside a view. *)
exception Unknown_container of page id * int * container id

(** A component ID appears twice inside a view. *)
exception Component_appearing_twice of page id * int * component id

(** A container ID appears twice inside a view. *)
exception Container_appearing_twice of page id * int * container id

(** Two views are assigned the same profile ID . *)
exception Profile_handled_by_two_views of page id * profile id * int * int

(** No view is assigned some profile ID . *)
exception Profile_not_handled of page id * profile id

(** A view is assigned an unknown profile ID . *)
exception Unknown_profile of page id * int * profile id

(** Check an RDDL definition and return the full list of errors. *)
val check : ui -> exn list

(** Check an RDDL definition, stop and raise the first error. *)
val wellformed : ui -> unit
