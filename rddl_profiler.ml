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
    physical_display_width = from 0 ;
    display_aspect_ratio = any ;
    device_width = from 0 ;
    physical_device_width = from 0 ;
    device_aspect_ratio = any ;
    contrast = only Normal ;
    ink = only Normal ;
    zoom = only Normal ;
    connected = [] ;
    bandwitdh = [] }

class type with_devicePixelRatio = object
  inherit Dom_html.window
  method devicePixelRatio : float Js.Optdef.t Js.readonly_prop
end

let window = (Js.Unsafe.coerce Dom_html.window :> with_devicePixelRatio Js.t)

let detect () =
  let device_pixel_ratio =
    Js.Optdef.case
      (window##devicePixelRatio)
      (fun () -> 1.0)
      (fun ratio -> ratio) in
  let unapply_device_pixel_ratio length =
    int_of_float (float length *. device_pixel_ratio) in
  let device_width =
    window##screen##width in
  let device_height =
    window##screen##height in
  let device_aspect_ratio =
    float device_width /. float device_height in
  let display_width =
    min
      (Js.Optdef.case
         (window##innerWidth)
         (fun () -> max_int)
         (fun width -> width))
      window##document##documentElement##clientWidth in
  let display_height =
    min
      (Js.Optdef.case
         (window##innerHeight)
         (fun () -> max_int)
         (fun height -> height))
      window##document##documentElement##clientHeight in
  let display_aspect_ratio =
    float display_width /. float display_height in
  { neutral with
    display_width = only display_width ;
    physical_display_width = only (unapply_device_pixel_ratio display_width) ;
    display_aspect_ratio = only display_aspect_ratio ;
    device_width = only device_width ;
    physical_device_width = only (unapply_device_pixel_ratio device_width) ;
    device_aspect_ratio = only device_aspect_ratio }

let current = ref (detect ())
let observers = Hashtbl.create 10

let update () =
  let profile = detect () in
  if profile <> !current then begin
    current := profile ;
    Hashtbl.iter (fun _ cb -> cb profile) observers
  end

let current () = !current

let () =
  ignore
    (Dom_events.listen
       ~capture:true window Dom_events.Typ.resize
       (fun _ _ -> update () ; true))

let on_update =
  let last_id = ref 0 in
  fun cb ->
    let id = !last_id in
    incr last_id ;
    Hashtbl.add observers id cb ;
    cb (current ()) ;
    fun () ->
      Hashtbl.remove observers id
