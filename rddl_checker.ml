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

exception Duplicate_profile_id of profile id
exception Duplicate_page_id of page id
exception Duplicate_component_id of page id * component id
exception Duplicate_container_id of page id * container id
exception Profile_handled_by_two_views of page id * profile id * int * int
exception Profile_not_handled of page id * profile id
exception Clashing_profiles of profile id * profile id * profile
exception Unkown_profile of page id * int * profile id

let check ui =
  let recoverables = ref [] in
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
  try
    let profiles =
      htable ui.profiles
        (fun id -> Duplicate_profile_id id) in
    Hashtbl.iter (fun id profile ->
        Hashtbl.iter (fun id' profile' ->
            if compatible profile profile' then
              let overlap = meet profile profile' in
              let exn = Clashing_profiles (id, id', overlap) in
              recoverables := exn :: !recoverables)
          profiles)
      profiles ;
    let pages =
      htable ui.pages
        (fun id -> Duplicate_page_id id) in
    Hashtbl.iter (fun page_id page ->
        let _components =
          htable page.components
            (fun id -> Duplicate_component_id (page_id, id)) in
        let _containers =
          htable page.containers
            (fun id -> Duplicate_container_id (page_id, id)) in
        let handled_profiles = Hashtbl.create 10 in
        List.iteri
          (fun i view ->
             List.iter
               (fun id ->
                  begin try
                      let prev = Hashtbl.find handled_profiles id in
                      let exn = Profile_handled_by_two_views (page_id, id, prev, i) in
                      recoverables := exn :: !recoverables
                    with Not_found -> ()
                  end ;
                  Hashtbl.add handled_profiles id i)
               view.compatible_profiles)
          page.views ;
        Hashtbl.iter (fun id _ ->
            if not (Hashtbl.mem handled_profiles id) then
              raise (Profile_not_handled (page_id, id)))
          profiles)
      pages ;
    !recoverables
  with
    fatal -> fatal :: !recoverables
