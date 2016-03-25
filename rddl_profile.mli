(** RDDL - Profile operations *)

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

(** {2 Profile builders and operations} *)

(** Completely generic profile *)
val generic : profile

(** Profile builder *)
val profile :
  ?output: output_level range ->
  ?interactivity: interactivity_level range ->
  ?display_width: int range ->
  ?physical_display_width: int range ->
  ?display_aspect_ratio: float range ->
  ?device_width: int range ->
  ?physical_device_width: int range ->
  ?device_aspect_ratio: float range ->
  ?contrast: three_steps_level range ->
  ?ink: three_steps_level range ->
  ?zoom: three_steps_level range ->
  ?connected: source id list ->
  ?bandwidth: (source id * int range) list ->
  unit -> profile

(** Computes the intersection of two profiles. *)
val meet : profile -> profile -> profile

(** Checks that the intersection of two profiles is not empty. *)
val compatible : profile -> profile -> bool

(** Checks that a profile is not empty. *)
val wellformed : profile -> bool

(** {2 Range builders and operations} *)

(** The full range. *)
val any : 'a range

(** A range with only its inclusive lower limit defined. *)
val from : 'a -> 'a range

(** A range with only its exclusive upper limit defined. *)
val above : 'a -> 'a range

(** A range with only its inclusive upper limit defined. *)
val upto : 'a -> 'a range

(** A range with only its exlusive lower limit defined. *)
val below : 'a -> 'a range

(** A singleton range. *)
val only : 'a -> 'a range

(** Computes the intersection of two ranges. *)
val (&&&) : 'a range -> 'a range -> 'a range

(** Checks that the intersection of two ranges is not empty. *)
val compatible_ranges : 'a range -> 'a range -> bool

(** Checks that a range is not empty. *)
val wellformed_range : 'a range -> bool
