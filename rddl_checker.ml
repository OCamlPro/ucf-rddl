(* RDDL - Checker *)

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

exception Duplicate_profile of profile id
exception Clashing_profiles of profile id * profile id * profile
exception Duplicate_page of page id
exception Duplicate_component of page id * component id
exception Duplicate_container of page id * container id
exception Missing_component of page id * int * component id
exception Missing_container of page id * int * container id
exception Unknown_component of page id * int * component id
exception Unknown_container of page id * int * container id
exception Component_appearing_twice of page id * int * component id
exception Container_appearing_twice of page id * int * container id
exception Profile_handled_by_two_views of page id * profile id * int * int
exception Profile_not_handled of page id * profile id
exception Unknown_profile of page id * int * profile id

(* The generic checker, which may stop upon the first error or not,
   depending on the [raise] callback. *)
let check raise ui =
  let htable table error =
    let htable = Hashtbl.create 10 in
    List.iter
      (fun (id, elt) ->
         if Hashtbl.mem htable id then
           raise (error id)
         else
           Hashtbl.add htable id elt)
      table ;
    htable in
  let profiles =
    htable ui.profiles
      (fun id -> Duplicate_profile id) in
  Hashtbl.iter (fun id profile ->
      Hashtbl.iter (fun id' profile' ->
          if compatible profile profile' then
            let overlap = meet profile profile' in
            raise (Clashing_profiles (id, id', overlap)))
        profiles)
    profiles ;
  let pages =
    htable ui.pages
      (fun id -> Duplicate_page id) in
  Hashtbl.iter (fun page_id page ->
      let components =
        htable page.components
          (fun id -> Duplicate_component (page_id, id)) in
      let containers =
        htable page.containers
          (fun id -> Duplicate_container (page_id, id)) in
      let handled_profiles = Hashtbl.create 10 in
      List.iteri
        (fun i view ->
           let appearing_containers = Hashtbl.create 10 in
           let appearing_components = Hashtbl.create 10 in
           let rec traverse = function
             | Container (_cstr, children, None) ->
               List.iter traverse children
             | Container (_cstr, children, Some id) ->
               if not (Hashtbl.mem containers id) then
                 raise (Unknown_container (page_id, i, id)) ;
               if Hashtbl.mem appearing_containers id then
                 raise (Container_appearing_twice (page_id, i, id)) ;
               Hashtbl.add appearing_containers id () ;
               List.iter traverse children
             | Component (_cstr, id) ->
               if not (Hashtbl.mem components id) then
                 raise (Unknown_component (page_id, i, id)) ;
               if Hashtbl.mem appearing_components id then
                 raise (Component_appearing_twice (page_id, i, id)) ;
               Hashtbl.add appearing_components id () in
           traverse view.document ;
           Hashtbl.iter
             (fun id { container_priority = prio } -> match prio with
                | Optional -> ()
                | Required | Hideable ->
                  if not (Hashtbl.mem appearing_containers id) then
                    raise (Missing_container (page_id, i, id)))
             containers ;
           Hashtbl.iter
             (fun id { component_priority = prio } -> match prio with
                | Optional -> ()
                | Required | Hideable ->
                  if not (Hashtbl.mem appearing_components id) then
                    raise (Missing_component (page_id, i, id)))
             components ;
           List.iter
             (fun id ->
                begin try
                    let prev = Hashtbl.find handled_profiles id in
                    raise (Profile_handled_by_two_views (page_id, id, prev, i))
                  with Not_found -> ()
                end ;
                Hashtbl.add handled_profiles id i)
             view.compatible_profiles)
        page.views ;
      Hashtbl.iter (fun id _ ->
          if not (Hashtbl.mem handled_profiles id) then
            raise (Profile_not_handled (page_id, id)))
        profiles)
    pages

let wellformed ui =
  check raise ui

let check ui =
  let recoverables = ref [] in
  let dont_raise exn =
    recoverables := exn :: !recoverables in
  check dont_raise ui ;
  !recoverables
