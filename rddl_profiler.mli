(** RDDL - Browser introspection and profile selection  *)

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

(** Represents a dynamic profile update monitor. *)
type updates

(** Monitors the main window. *)
val window : updates

(** Stop updating the profile. *)
val stop : updates -> unit

(** The forged profile that represents the {!current} state of the
    monitored component. *)
val state : updates -> profile

(** Register a callback to trigger whenever the component is updated.
    The callback is called with the forged profile that represents the
    component's {!current} state. The callback is also called immediately
    with the initial profile. If a change arises while a callback is
    running, it is deferred to the end of the callback. If several
    changes arise, the last one is deferred, the previous ones are
    dropped. *)
val on_update : updates -> (profile -> unit Lwt.t) -> unit

(** One cycle of {!on_update}, to use in custom loops. *)
val wait_next_update : updates -> profile Lwt.t

(** Represents a dynamic profile selection change monitor. *)
type changes

(** Select the appropriate profile for a component at each update,
    from a given non empty table. *)
val changes : updates -> profile table -> changes

(** The currently selected profile id, and the result of the {!merge}
    of the profile from the table and the current {!state}. *)
val selection : changes -> profile id * profile

(** Register a callback to trigger whenever the profile id
    changes. Also called initially with the first selection. The given
    profile is the same as the one returned by {!selection}. Same
    buffering mechanism as {!on_update}. *)
val on_change : changes -> (profile id * profile -> unit Lwt.t) -> unit

(** One cycle of {!on_change}, to use in custom loops. *)
val wait_next_change : changes -> (profile id * profile) Lwt.t
