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
open Lwt.Infix

(* profile merging ******************************************************)

let wellformed p =
  let wellformed_range = function
    | { min = None } | { max = None } -> true
    | { min = Some min ; max = Some max } -> min <= max in
  wellformed_range p.output &&
  wellformed_range p.interactivity &&
  wellformed_range p.display_width &&
  wellformed_range p.physical_display_width &&
  wellformed_range p.display_aspect_ratio &&
  wellformed_range p.device_width &&
  wellformed_range p.physical_device_width &&
  wellformed_range p.device_aspect_ratio &&
  wellformed_range p.contrast &&
  wellformed_range p.ink &&
  wellformed_range p.zoom &&
  p.connected = [] &&
  p.bandwidth = []

let meet pa pb =
  assert (wellformed pa) ;
  assert (wellformed pb) ;
  let meet_range ra rb =
    { min = begin match ra.min, rb.min with
          | Some mina, Some minb -> Some (max mina minb)
          | Some m, None | None, Some m -> Some m
          | None, None -> None
        end ;
      max = begin match ra.max, rb.max with
        | Some maxa, Some maxb -> Some (min maxa maxb)
        | Some m, None | None, Some m -> Some m
        | None, None -> None
      end } in
  let result =
    { output =
        meet_range pa.output pb.output ;
      interactivity =
        meet_range pa.interactivity pb.interactivity ;
      display_width =
        meet_range pa.display_width pb.display_width ;
      physical_display_width =
        meet_range pa.physical_display_width pb.physical_display_width ;
      display_aspect_ratio =
        meet_range pa.display_aspect_ratio pb.display_aspect_ratio ;
      device_width =
        meet_range pa.device_width pb.device_width ;
      physical_device_width =
        meet_range pa.physical_device_width pb.physical_device_width ;
      device_aspect_ratio =
        meet_range pa.device_aspect_ratio pb.device_aspect_ratio ;
      contrast =
        meet_range pa.contrast pb.contrast ;
      ink =
        meet_range pa.ink pb.ink ;
      zoom =
        meet_range pa.zoom pb.zoom ;
      connected = [] ;
      bandwidth = [] } in
  assert (wellformed result) ;
  result

let compatible pa pb =
  assert (wellformed pa) ;
  assert (wellformed pb) ;
  let compatible_range ra rb =
    let min = match ra.min, rb.min with
      | Some mina, Some minb -> Some (max mina minb)
      | Some m, None | None, Some m -> Some m
      | None, None -> None
    and max = match ra.max, rb.max with
      | Some maxa, Some maxb -> Some (min maxa maxb)
      | Some m, None | None, Some m -> Some m
      | None, None -> None in
    match min, max with
    | Some min, Some max -> min <= max
    | _ -> true in
  compatible_range pa.output pb.output &&
  compatible_range pa.interactivity pb.interactivity &&
  compatible_range pa.display_width pb.display_width &&
  compatible_range pa.physical_display_width pb.physical_display_width &&
  compatible_range pa.display_aspect_ratio pb.display_aspect_ratio &&
  compatible_range pa.device_width pb.device_width &&
  compatible_range pa.physical_device_width pb.physical_device_width &&
  compatible_range pa.device_aspect_ratio pb.device_aspect_ratio &&
  compatible_range pa.contrast pb.contrast &&
  compatible_range pa.ink pb.ink &&
  compatible_range pa.zoom pb.zoom

(* state detection ******************************************************)

let build_state
    ~display_width
    ~physical_display_width
    ~display_aspect_ratio
    ~device_width
    ~physical_device_width
    ~device_aspect_ratio =
  { output = any ;
    interactivity = between Pointer Multi_touch ;
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

let selection { updates ; current ; table} =
  !current, meet (List.assoc !current table) (state updates)

let find_profile profile table =
  try
    fst @@ List.find
      (fun (id, rprofile) -> compatible profile rprofile)
      table
  with Not_found ->
    failwith "no compatible profile found"

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
