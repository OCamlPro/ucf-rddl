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
open Rddl_profile
open Lwt.Infix

(* state detection ******************************************************)

let build_state
    ~display_width
    ~physical_display_width
    ~display_aspect_ratio
    ~device_width
    ~physical_device_width
    ~device_aspect_ratio =
  { output = any ;
    interactivity = from Pointer ;
    display_width = only display_width ;
    physical_display_width = only physical_display_width ;
    display_aspect_ratio = only display_aspect_ratio ;
    device_width = only device_width ;
    physical_device_width = only physical_device_width ;
    device_aspect_ratio = only device_aspect_ratio ;
    contrast = any ;
    ink = any ;
    zoom = any ;
    connected = [] ;
    bandwidth = [] }

class type with_devicePixelRatio = object
  inherit Dom_html.window
  method devicePixelRatio : float Js.Optdef.t Js.readonly_prop
end

let window = (Js.Unsafe.coerce Dom_html.window :> with_devicePixelRatio Js.t)

let window_state () =
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
  build_state
    ~display_width
    ~physical_display_width: (unapply_device_pixel_ratio display_width)
    ~display_aspect_ratio
    ~device_width
    ~physical_device_width: (unapply_device_pixel_ratio device_width)
    ~device_aspect_ratio

(* update monitoring ****************************************************)

type updates =
  { state : profile ref ;
    stop : unit -> unit ;
    listeners : ([ `Stop | `Update of profile ] -> unit) list ref }

let window =
  let state = ref (window_state ()) in
  let listeners = ref [] in
  let stop () = List.iter (fun cb -> cb `Stop) !listeners in
  let update () =
    state := window_state () ;
    let pre = !listeners in
    listeners := [] ;
    List.iter (fun cb -> cb (`Update !state)) pre in
  ignore
    (Dom_events.listen
       ~capture:true window Dom_events.Typ.resize
       (fun _ _ -> ignore (update ()) ; true)) ;
  { state ; stop ; listeners }

(* update monitoring ****************************************************)

let state { state } = !state

let stop { stop } = stop ()

let wait_next_update { listeners } =
  let t, u = Lwt.task () in
  let cb = function
    | `Stop -> Lwt.cancel t
    | `Update profile -> Lwt.wakeup u profile in
  listeners := cb :: !listeners ;
  t

let on_update { listeners ; state } body =
  let last = ref (`Some !state) in
  let waiter = ref (Lwt.task ()) in
  let rec cb () = function
    | `Stop ->
      Lwt.cancel (fst !waiter) ;
      last := `Stop
    | `Update profile ->
      last := `Some profile ;
      listeners := cb () :: !listeners ;
      Lwt.wakeup (snd !waiter) () in
  listeners := cb () :: !listeners ;
  let rec loop () =
    match !last with
    | `Stop -> Lwt.return ()
    | `None ->
      let t, u = Lwt.task () in
      waiter := (t, u) ;
      t >>= loop
    | `Some profile ->
      last := `None ;
      body profile >>= fun () ->
      loop () in
  Lwt.async loop

(* changes monitoring ***************************************************)

type changes =
  { updates : updates ;
    current : profile id ref ;
    table : profile table }

let find_profile profile table =
  try
    fst @@ List.find
      (fun (id, rprofile) -> compatible profile rprofile)
      table
  with Not_found ->
    failwith "no compatible profile found"

let selection { updates ; current ; table} =
  let profile = state updates in
  current := find_profile profile table ;
  !current, meet (List.assoc !current table) profile

let changes updates table =
  { updates ; table ;
    current = ref (find_profile (state updates) table) }

let on_change ({ updates ; current ; table } as changes) cb =
  let first = ref true in
  on_update updates @@ fun profile ->
  let new_id = find_profile profile table in
  if !first || !current <> new_id then begin
    current := new_id ;
    first := false ;
    cb (selection changes)
  end else
    Lwt.return ()

let wait_next_change ({ updates ; current ; table } as changes) =
  let rec loop () =
    wait_next_update updates >>= fun profile ->
    let new_id = find_profile profile table in
    if !current <> new_id then begin
      current := new_id ;
      Lwt.return (selection changes)
    end else
      loop () in
  loop ()
