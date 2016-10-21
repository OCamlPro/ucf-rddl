(* RDDL - Browser introspection demo *)

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

let letter_box_workers = Hashtbl.create 100
let letter_box_worker_id = ref 0

let enter_mode child = function
  | `Fill ->
    child##.style##.position := Js.string "absolute" ;
    child##.style##.width := Js.string "100%" ;
    child##.style##.height := Js.string "100%"
  | `Hbars ->
    child##.style##.position := Js.string "absolute" ;
    child##.style##.width := Js.string "100%"
  | `Vbars ->
    child##.style##.position := Js.string "absolute" ;
    child##.style##.height := Js.string "100%"

let leave_mode child = function
  | `Fill ->
    child##.style##.position := Js.string "" ;
    child##.style##.width := Js.string "" ;
    child##.style##.height := Js.string ""
  | `Hbars ->
    child##.style##.position := Js.string "" ;
    child##.style##.width := Js.string ""
  | `Vbars ->
    child##.style##.position := Js.string "" ;
    child##.style##.height := Js.string ""

let construct_container ~page_id ~container_id ~profile_id container =
  match container.container_constructor with
  | "letter-box" ->
    let construct = function
      | [ { Rddl_renderer.element = child ; kind = `Component component } ] ->
        let wrapper = Dom_html.createDiv Dom_html.document in
        wrapper##.style##.backgroundColor := Js.string "black" ;
        wrapper##.style##.position := Js.string "relative" ;
        wrapper##.style##.width := Js.string "100%" ;
        wrapper##.style##.height := Js.string "100%" ;
        Dom.appendChild wrapper child ;
        begin match component.component_aspect_ratio with
          | { min = None ; max = None } ->
            enter_mode child `Fill
          | range ->
            let max = match range.max with None -> infinity | Some (max, _) -> max in
            let mode = ref `Fill in
            enter_mode child !mode ;
            let updates = Rddl_profiler.div wrapper in
            incr letter_box_worker_id ;
            let id = string_of_int !letter_box_worker_id in
            Hashtbl.add letter_box_workers id (updates, mode) ;
            wrapper##setAttribute (Js.string "worker_id") (Js.string id) ;
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () ->
                     Rddl_profiler.on_update updates @@ fun profile ->
                     let nmode =
                       match profile.display_aspect_ratio with
                       | { min = Some (aspect, _) } ->
                         if (aspect = aspect) = false
                         || Rddl_profile.inside aspect range then `Fill
                         else if aspect < max then `Hbars
                         else `Vbars
                       | _unknown -> `Fill in
                     if nmode <> !mode then begin
                       leave_mode child !mode ;
                       mode := nmode ;
                       enter_mode child !mode
                     end ;
                     Lwt.return ())
                  (fun _exn -> Lwt.return ())) ;
            ()
        end ;
        wrapper
      | _ -> failwith "letter-box constructor expects a single child component" in
    `Immediate construct
  | "vertical-box" ->
    let construct children =
      let div = Dom_html.createDiv Dom_html.document in
      div##.style##.display := Js.string "flex" ;
      Js.Unsafe.set (div##.style) (Js.string "flexDirection") (Js.string "column") ;
      div##.style##.width := Js.string "100%" ;
      List.iter
        (fun { Rddl_renderer.element = child } ->
           Js.Unsafe.set (child##.style) (Js.string "flex") (Js.string "0 1 auto") ;
           child##.style##.width := Js.string "100%" ;
           let child = (child :> Dom.node Js.t) in
           ignore (div##appendChild (child)))
        children ;
      div in
    `Immediate construct
  | "horizontal-box" ->
    let construct children =
      let div = Dom_html.createDiv Dom_html.document in
      div##.style##.display := Js.string "flex" ;
      Js.Unsafe.set (div##.style) (Js.string "flexDirection") (Js.string "row") ;
      div##.style##.height := Js.string "100%" ;
      List.iter
        (fun { Rddl_renderer.element = child } ->
           Js.Unsafe.set (child##.style) (Js.string "flex") (Js.string "0 1 auto") ;
           child##.style##.height := Js.string "100%" ;
           let child = (child :> Dom.node Js.t) in
           ignore (div##appendChild (child)))
        children ;
      div in
    `Immediate construct
  | _ -> `Default

let destruct_container ~page_id ~container_id container =
  match container.container_constructor with
  | "vertical-box" ->
    let destruct (elt, children) =
      List.iter
        (fun { Rddl_renderer.element = child } ->
           Js.Unsafe.set (child##.style) (Js.string "flex") (Js.string "") ;
           Js.Unsafe.set (child##.style) (Js.string "width") (Js.string ""))
        children in
    `Immediate destruct
  | "horizontal-box" ->
    let destruct (elt, children) =
      List.iter
        (fun { Rddl_renderer.element = child } ->
           Js.Unsafe.set (child##.style) (Js.string "flex") (Js.string "") ;
           Js.Unsafe.set (child##.style) (Js.string "height") (Js.string ""))
        children in
    `Immediate destruct
  | "letter-box" ->
    let destruct (elt, children) =
      Js.Opt.case
        (elt##getAttribute (Js.string "worker_id"))
        (fun () -> ())
        (fun id ->
           let id = Js.to_string id in
           try
             let updates, mode = Hashtbl.find letter_box_workers id in
             Hashtbl.remove letter_box_workers id ;
             Rddl_profiler.stop updates ;
             List.iter
               (fun { Rddl_renderer.element = child } -> leave_mode child !mode)
               children
           with
             Not_found -> ()) in
    `Immediate destruct
  | _ -> `Default

let () =
  Rddl_renderer.register_global_container_constructor construct_container ;
  Rddl_renderer.register_global_container_destructor destruct_container
