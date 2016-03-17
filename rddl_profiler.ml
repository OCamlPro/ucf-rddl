(* RDDL - Browser introspection and profile selection  *)

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

let neutral =
  { output = only Fancy ;
    interactivity = between Pointer Multi_touch ;
    display_width = from 0 ;
    device_width = from 0 ;
    display_aspect_ratio = any ;
    device_aspect_ratio = any ;
    resolution = any ;
    contrast = only Normal ;
    ink = only Normal ;
    zoom = only Normal ;
    connected = [] ;
    bandwitdh = [] }

let current () = neutral

let on_update =
  let all = Hashtbl.create 10 in
  let last_id = ref 0 in
  fun cb ->
    let id = !last_id in
    incr last_id ;
    Hashtbl.add all id cb ;
    fun () ->
      Hashtbl.remove all id
