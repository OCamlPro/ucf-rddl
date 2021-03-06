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
exception Uncovered_profile of profile
exception Duplicate_page of page id
exception Duplicate_component of page id * component id
exception Duplicate_container of page id * container id
exception Missing_component of page id * int * component id
exception Missing_container of page id * int * container id
exception Unknown_component of page id * int * component id
exception Unknown_container of page id * int * container id
exception Component_appearing_twice of page id * int * component id
exception Container_appearing_twice of page id * int * container id
exception Component_never_appears of page id * component id
exception Container_never_appears of page id * container id
exception Profile_handled_by_two_views of page id * profile id * int * int
exception Profile_not_handled of page id * profile id
exception Unknown_profile of page id * int * profile id

type 'a criteria_impl =
  { hotpoints : profile list -> 'a list ;
    assign : profile -> 'a -> profile }

type criteria = Criteria : _ criteria_impl -> criteria

exception Found of profile

let enum_criteria access assign cases =
  let hotpoints profiles =
    let ranges = List.map access profiles in
    List.filter
      (fun x -> List.exists (fun range -> not (inside x range)) ranges)
      cases in
  Criteria { hotpoints ; assign }

let range_criteria access assign filter =
  let hotpoints profiles =
    let ranges =
      List.map access profiles in
    let bounds =
      List.fold_left
        (fun acc { min ; max } ->
           begin match min with
             | None -> []
             | Some (v, `Open) -> [ (v, `Left) ]
             | Some (v, `Closed) -> [ (v, `Right) ]
           end @
           begin match max with
             | None -> []
             | Some (v, `Open) -> [ (v, `Right) ]
             | Some (v, `Closed) -> [ (v, `Left) ]
           end @
           acc)
        [] ranges in
    let bounds =
      List.sort compare bounds in
    let rec ranges acc = function
      | [] -> acc
      | (last, `Right) :: [] ->
        { min = Some (last, `Closed) ; max = None } :: acc
      | (last, `Left) :: [] ->
        { min = Some (last, `Open) ; max = None } :: acc
      | (vleft, `Right) :: ((vright, `Left) :: _ as rest) when vleft = vright ->
        ranges
          ({ min = Some (vleft, `Closed) ;
             max = Some (vright, `Closed) } ::
           acc)
          rest
      | (vleft, bleft) :: ((vright, bright) :: _ as rest) when vleft = vright ->
        ranges acc rest
      | (vleft, bleft) :: ((vright, bright) :: _ as rest) ->
        ranges
          ({ min = Some (vleft,
                         match bleft with
                         | `Left -> `Open
                         | `Right -> `Closed) ;
             max = Some (vright,
                         match bright with
                         | `Left -> `Closed
                         | `Right -> `Open) } ::
           acc)
          rest in
    let first = begin match bounds with
      | [] -> []
      | (first, `Right) :: _ ->
        [ { min = None ; max = Some (first, `Open) } ]
      | (first, `Left) :: _ ->
        [ { min = None ; max = Some (first, `Closed) } ]
    end in
    List.filter filter ({ min = None ; max = None } :: ranges first bounds) in
  Criteria { hotpoints ; assign }

let int_criteria access assign =
  range_criteria access assign
    (function
      | { min = Some (v, `Open) ; max = Some (v', `Open) } ->
        (* filter empty integer ranges *)
        v' <> v + 1
      | _ -> true)

let float_criteria access assign =
  range_criteria access assign
    (fun _ -> true)

let criteria =
  [ enum_criteria
      (fun { output = v } -> v)
      (fun r v -> { r with output = only v })
      [ Textual ; Simplified ; Fancy ] ;
    enum_criteria
      (fun { interactivity = v } -> v)
      (fun r v -> { r with interactivity = only v })
      [ View_only ; Pointer ; Single_touch ; Multi_touch ] ;
    enum_criteria
      (fun { contrast = v } -> v)
      (fun r v -> { r with contrast = only v })
      [ Low ; Normal ; High ] ;
    enum_criteria
      (fun { ink = v } -> v)
      (fun r v -> { r with ink = only v })
      [ Low ; Normal ; High ] ;
    enum_criteria
      (fun { zoom = v } -> v)
      (fun r v -> { r with zoom = only v })
      [ Low ; Normal ; High ] ;
    float_criteria
      (fun { display_aspect_ratio = v } -> v)
      (fun r v -> { r with display_aspect_ratio = v }) ;
    int_criteria
      (fun { display_width = v } -> v)
      (fun r v -> { r with display_width = v }) ;
    int_criteria
      (fun { physical_display_width = v } -> v)
      (fun r v -> { r with physical_display_width = v }) ;
    float_criteria
      (fun { device_aspect_ratio = v } -> v)
      (fun r v -> { r with device_aspect_ratio = v }) ;
    int_criteria
      (fun { device_width = v } -> v)
      (fun r v -> { r with device_width = v }) ;
    int_criteria
      (fun { physical_device_width = v } -> v)
      (fun r v -> { r with physical_device_width = v }) ]

let find_unhandled_profile profiles =
  let rec find profile = function
    | [] ->
      if not (List.exists (compatible profile) profiles) then
        raise (Found profile)
    | (Criteria crit) :: crits ->
      match crit.hotpoints profiles with
      | [] -> find profile crits
      | hotpoints ->
        List.iter
          (fun x -> find (crit.assign profile x) crits)
          hotpoints in
  try find (profile ()) criteria ; raise Not_found with Found p -> p

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
          if id <> id' && compatible profile profile' then
            let overlap = meet profile profile' in
            raise (Clashing_profiles (id, id', overlap)))
        profiles)
    profiles ;
  begin try
      let profiles = snd (List.split ui.profiles) in
      raise (Uncovered_profile (find_unhandled_profile profiles)) ;
    with Not_found -> () end ;
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
      let appearing_anywhere_containers = Hashtbl.create 10 in
      let appearing_anywhere_components = Hashtbl.create 10 in
      List.iteri
        (fun i view ->
           let appearing_containers = Hashtbl.create 10 in
           let appearing_components = Hashtbl.create 10 in
           let rec traverse = function
             | Anonymous_container (_, _, children) ->
               List.iter traverse children
             | Container (id, children) ->
               if not (Hashtbl.mem containers id) then
                 raise (Unknown_container (page_id, i, id)) ;
               if Hashtbl.mem appearing_containers id then
                 raise (Container_appearing_twice (page_id, i, id)) ;
               Hashtbl.add appearing_anywhere_containers id () ;
               Hashtbl.add appearing_containers id () ;
               List.iter traverse children
             | Anonymous_component _ -> ()
             | Component id ->
               if not (Hashtbl.mem components id) then
                 raise (Unknown_component (page_id, i, id)) ;
               if Hashtbl.mem appearing_components id then
                 raise (Component_appearing_twice (page_id, i, id)) ;
               Hashtbl.add appearing_anywhere_components id () ;
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
      Hashtbl.iter
        (fun id { container_priority = prio } -> match prio with
           | Required | Hideable -> ()
           | Optional ->
             if not (Hashtbl.mem appearing_anywhere_containers id) then
               raise (Container_never_appears (page_id, id)))
        containers ;
      Hashtbl.iter
        (fun id { component_priority = prio } -> match prio with
           | Required | Hideable -> ()
           | Optional ->
             if not (Hashtbl.mem appearing_anywhere_components id) then
               raise (Component_never_appears (page_id, id)))
        components ;
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


let pretty_print_profile ppf profile =
  let json =
    Json_repr.to_yojson
      (Json_encoding.construct profile_encoding profile) in
  Yojson.Safe.pretty_print ppf json

let print_error ?print_unknown ppf = function
  | Duplicate_profile id ->
    Format.fprintf ppf "Duplicate profile identifier %s" id
  | Clashing_profiles (id, id', p) ->
    Format.fprintf ppf "Profiles %s and %s overlap,@,\
                        here is the computed intersection:@,%a" id id'
      pretty_print_profile p
  | Uncovered_profile p ->
    Format.fprintf ppf "@[<v 2>Profiles are not exhaustive,@,\
                        here is an example profile that is not handled:@,%a"
      pretty_print_profile p
  | Duplicate_page id ->
    Format.fprintf ppf "Duplicate page identifier %s" id
  | Duplicate_component (page_id, id) ->
    Format.fprintf ppf "In page %s, duplicate component identifier %s" page_id id
  | Duplicate_container (page_id, id) ->
    Format.fprintf ppf "In page %s, duplicate container identifier %s" page_id id
  | Missing_component (page_id, view, id) ->
    Format.fprintf ppf "In page %s, view %d, missing component %s" page_id view id
  | Missing_container (page_id, view, id) ->
    Format.fprintf ppf "In page %s, view %d, missing container %s" page_id view id
  | Unknown_component (page_id, view, id) ->
    Format.fprintf ppf "In page %s, view %d, unknown component %s" page_id view id
  | Unknown_container (page_id, view, id) ->
    Format.fprintf ppf "In page %s, view %d, unknown container %s" page_id view id
  | Component_appearing_twice (page_id, view, id) ->
    Format.fprintf ppf "In page %s, view %d, component %s appears twice" page_id view id
  | Container_appearing_twice (page_id, view, id) ->
    Format.fprintf ppf "In page %s, view %d, container %s appears twice" page_id view id
  | Component_never_appears (page_id, id) ->
    Format.fprintf ppf "In page %s, optional component %s never appears" page_id id
  | Container_never_appears (page_id, id) ->
    Format.fprintf ppf "In page %s, optional container %s never appears" page_id id
  | Profile_handled_by_two_views (page_id, id, view1, view2) ->
    Format.fprintf ppf "In page %s, profile %s handled by both views %d and %d" page_id id view1 view2
  | Profile_not_handled (page_id, id) ->
    Format.fprintf ppf "In page %s, profile %s not handled by any view" page_id id
  | Unknown_profile (page_id, view, id) ->
    Format.fprintf ppf "In page %s, view %d, unknown profile %s" page_id view id
  | exn ->
    Json_encoding.print_error ?print_unknown ppf exn
