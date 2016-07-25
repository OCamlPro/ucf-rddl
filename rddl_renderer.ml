(* RDDL - Page rendering *)

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

type resolved_element =
  | Resolved_container of container id * container * resolved_element list
  | Resolved_component of component id * component

let resolve page { containers ; components } profile root =
  let genid =
    let cur = ref 0 in
    fun () -> incr cur ; "anon" ^ string_of_int !cur in
  let rec resolve = function
    | Container (id, children) ->
      let container = try
          List.assoc id containers
        with Not_found ->
          failwith ("Unbound container " ^ id ^
                    " in page " ^ page ^
                    " and profile " ^ profile) in
      Resolved_container (id, container, List.map resolve children)
    | Component id ->
      let component = try
          List.assoc id components
        with Not_found ->
          failwith ("Unbound component " ^ id ^
                    " in page " ^ page ^
                    " and profile " ^ profile) in
      Resolved_component (id, component)
    | Anonymous_container (constructor, parameters, children) ->
      let container =
        { container_constructor = constructor ;
          container_extensible = false ;
          container_parameters = parameters ;
          container_priority = Hideable } in
      Resolved_container (genid (), container, List.map resolve children)
    | Anonymous_component (constructor, parameters) ->
      let component =
        { component_constructor = constructor ;
          component_parameters = parameters ;
          component_priority = Hideable ;
          component_aspect_ratio = Rddl_profile.any } in
      Resolved_component (genid (), component) in
  resolve root

type ('a, 'b) constructor =
  page: page id ->
  id: 'a id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  profile: profile id ->
  'b ->
  [ `Constructed of Dom_html.element Js.t | `Default ] Lwt.t

type 'a destructor =
  page: page id ->
  id: 'a id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  Dom_html.element Js.t ->
  [ `Destructed | `Default ] Lwt.t

type ('a, 'b) rebinder =
  page: page id ->
  id: 'a id ->
  constructor: string ->
  parameters: Json_repr_browser.value option ->
  previous_profile: profile id ->
  new_profile: profile id ->
  Dom_html.element Js.t ->
  'b ->
  [ `Rebound | `Reconstruct | `Default ] Lwt.t

type component_constructor =
  (component, unit) constructor
type component_destructor =
  component destructor
type component_rebinder =
  (component, unit) rebinder
type container_constructor =
  (container, Dom_html.element Js.t list) constructor
type container_destructor =
  container destructor
type container_rebinder =
  (container, Dom_html.element Js.t list) rebinder

type context =
  { container : Dom_html.element Js.t ;
    mutable root : Dom_html.element Js.t ;
    components : ((page id * component id), Dom_html.element Js.t *
                                            (string *
                                             Json_repr_browser.value option)) Hashtbl.t ;
    containers : ((page id * component id), Dom_html.element Js.t *
                                            (string *
                                             Json_repr_browser.value option)) Hashtbl.t ;
    construct_component : (component, unit) constructor ;
    destruct_component : component destructor ;
    rebind_component : (component, unit) rebinder ;
    construct_container : (container, Dom_html.element Js.t list) constructor ;
    destruct_container : container destructor ;
    rebind_container : (container, Dom_html.element Js.t list) rebinder ;
    ui : ui ;
    mutable page_and_profile_ids : (page id * profile id) option }

let component_constructors
  : component_constructor list ref
  = ref []

let register_global_component_constructor construct_component =
  component_constructors := construct_component :: !component_constructors

let component_destructors
  : component_destructor list ref
  = ref []

let register_global_component_destructor destruct_component =
  component_destructors := destruct_component :: !component_destructors

let component_rebinders
  : component_rebinder list ref
  = ref []

let register_global_component_rebinder rebind_component =
  component_rebinders := rebind_component :: !component_rebinders

let container_constructors
  : container_constructor list ref
  = ref []

let register_global_container_constructor construct_container =
  container_constructors := construct_container :: !container_constructors

let container_destructors
  : container_destructor list ref
  = ref []

let register_global_container_destructor destruct_container =
  container_destructors := destruct_container :: !container_destructors

let container_rebinders
  : container_rebinder list ref
  = ref []

let register_global_container_rebinder rebind_container =
  container_rebinders := rebind_container :: !container_rebinders

let main_container_id =
  "rddl-main-container"

let main_container_style =
  "position: absolute; \
   left: 0px; right: 0px; top: 0px; bottom: 0px;"

let div
    container
    ?(construct_component =
      fun ~page ~id ~constructor ~parameters ~profile () ->
        Lwt.return `Default)
    ?(destruct_component =
      fun ~page ~id ~constructor ~parameters elt ->
        Lwt.return `Default)
    ?(rebind_component =
      fun ~page ~id ~constructor ~parameters ~previous_profile ~new_profile elt () ->
        Lwt.return `Default)
    ?(construct_container =
      fun ~page ~id ~constructor ~parameters ~profile children ->
        Lwt.return `Default)
    ?(destruct_container =
      fun ~page ~id ~constructor ~parameters elt ->
        Lwt.return `Default)
    ?(rebind_container =
      fun ~page ~id ~constructor ~parameters ~previous_profile ~new_profile elt children ->
        Lwt.return `Default)
    ui =
  { container ;
    root = container (* dummy *) ;
    ui ; page_and_profile_ids = None ;
    components = Hashtbl.create 10 ;
    containers = Hashtbl.create 10 ;
    construct_component ;
    destruct_component ;
    rebind_component ;
    construct_container ;
    destruct_container ;
    rebind_container }

let window
    ?main_container_id: (id = main_container_id)
    ?construct_component
    ?destruct_component
    ?rebind_component
    ?construct_container
    ?destruct_container
    ?rebind_container ui =
  let container =
    Js.Opt.case
      (Dom_html.window##document##getElementById (Js.string id))
      (fun () ->
         let div = Dom_html.createDiv (Dom_html.window##document) in
         div##setAttribute (Js.string "id", Js.string id) ;
         div##setAttribute (Js.string "style", Js.string main_container_style) ;
         ignore (Dom_html.window##document##body##appendChild ((div :> Dom.node Js.t))) ;
         div)
      (fun div -> div) in
  div
    container
    ?construct_component
    ?destruct_component
    ?rebind_component
    ?construct_container
    ?destruct_container
    ?rebind_container ui

let render ctx page_id profile_id =
  let rec chained_call = function
    | [] -> Lwt.return `Default
    | f :: fs ->
      f () >>= function
      | `Default -> chained_call fs
      | res -> Lwt.return res in
  let construct_container page profile ~id ~constructor ~parameters children =
    (* Firebug.console##debug (Js.string (Format.asprintf "construct %s/%s" page id)) ; *)
    chained_call @@
    List.map
      (fun f () -> f ~page ~id ~constructor ~parameters ~profile children)
      (ctx.construct_container :: !container_constructors) >>= function
    | `Constructed elt -> Lwt.return elt
    | `Default ->
      Lwt.fail_with
        ("Cannot find a suitable container constructor for " ^ constructor ^
         " with id " ^ id ^
         " in page " ^ page ^
         " and profile " ^ profile) in
  let construct_component page profile ~id ~constructor ~parameters children =
    (* Firebug.console##debug (Js.string (Format.asprintf "construct %s/%s" page id)) ; *)
    chained_call @@
    List.map
      (fun f () -> f ~page ~id ~constructor ~parameters ~profile children)
      (ctx.construct_component :: !component_constructors) >>= function
    | `Constructed elt -> Lwt.return elt
    | `Default ->
      Lwt.fail_with
        ("Cannot find a suitable component constructor for " ^ constructor ^
         " with id " ^ id ^
         " in page " ^ page ^
         " and profile " ^ profile) in
  let destruct_container page ~id ~constructor ~parameters elt =
    (* Firebug.console##debug (Js.string (Format.asprintf "destruct %s/%s" page id)) ; *)
    chained_call @@
    List.map
      (fun f () -> f ~page ~id ~constructor ~parameters elt)
      (ctx.destruct_container :: !container_destructors) >>= function
    | `Destructed
    | `Default -> Lwt.return () in
  let destruct_component page ~id ~constructor ~parameters elt =
    (* Firebug.console##debug (Js.string (Format.asprintf "destruct %s/%s" page id)) ; *)
    chained_call @@
    List.map
      (fun f () -> f ~page ~id ~constructor ~parameters elt)
      (ctx.destruct_component :: !component_destructors) >>= function
    | `Destructed
    | `Default -> Lwt.return () in
  let rebind_container page ~id ~constructor ~parameters ~previous_profile ~new_profile elt children =
    (* Firebug.console##debug (Js.string (Format.asprintf "rebind %s/%s" page id)) ; *)
    chained_call @@
    List.map
      (fun f () -> f ~page ~id ~constructor ~parameters ~previous_profile ~new_profile elt children)
      (ctx.rebind_container :: !container_rebinders) in
  let rebind_component page ~id ~constructor ~parameters ~previous_profile ~new_profile elt () =
    (* Firebug.console##debug (Js.string (Format.asprintf "rebind %s/%s" page id)) ; *)
    chained_call @@
    List.map
      (fun f () -> f ~page ~id ~constructor ~parameters ~previous_profile ~new_profile elt ())
      (ctx.rebind_component :: !component_rebinders) in
  let find_view page_id profile =
    try
      let page = List.assoc page_id ctx.ui.pages in
      let { document } =
        List.find
          (fun { compatible_profiles = cps } -> List.mem profile cps)
          page.views in
      Lwt.return (resolve page_id page profile document)
    with
    | Not_found ->
      Lwt.fail_with ("No view for page " ^ page_id ^
                     " and profile " ^ profile)
    | exn -> Lwt.fail exn in
  let rec traverse ~do_component ~do_container = function
    | Resolved_container (id, container, children) ->
      Lwt_list.map_p
        (traverse ~do_component ~do_container)
        children >>= fun results ->
      let { container_constructor = constructor ;
            container_parameters = parameters }
        = container in
      let parameters = match parameters with
        | Some parameters ->
          Some
            (Json_repr.any_to_repr
               (module Json_repr_browser.Repr)
               parameters)
        | None -> None in
      do_container ~id ~constructor ~parameters results
    | Resolved_component (id, component) ->
      let { component_constructor = constructor ;
            component_parameters = parameters }
        = component in
      let parameters = match parameters with
        | Some parameters ->
          Some
            (Json_repr.any_to_repr
               (module Json_repr_browser.Repr)
               parameters)
        | None -> None in
      do_component ~id ~constructor ~parameters () in
  let construct ~page_id ~profile_id = traverse
      ~do_container:
        (fun ~id ~constructor ~parameters children ->
           construct_container page_id profile_id ~id ~constructor ~parameters children >>= fun elt ->
           Hashtbl.add ctx.containers (page_id, id) (elt, (constructor, parameters)) ;
           Lwt.return elt)
      ~do_component:
        (fun ~id ~constructor ~parameters () ->
           construct_component page_id profile_id ~id ~constructor ~parameters () >>= fun elt ->
           Hashtbl.add ctx.components (page_id, id) (elt, (constructor, parameters)) ;
           Lwt.return elt) in
  let rebind ~previous_page_id ~page_id ~previous_profile ~new_profile = traverse
      ~do_container:
        (fun ~id ~constructor ~parameters children ->
           try
             let elt, (previous_constructor, previous_parameters) =
               Hashtbl.find ctx.containers (page_id, id) in
             rebind_container page_id ~id ~constructor ~parameters ~previous_profile ~new_profile elt children >>= function
             | `Rebound -> Lwt.return elt
             | `Default
               when previous_constructor = constructor
                 && previous_parameters == parameters -> Lwt.return elt
             | `Default | `Reconstruct ->
               destruct_container previous_page_id ~id
                 ~constructor: previous_constructor
                 ~parameters: previous_parameters elt >>= fun () ->
               Hashtbl.remove ctx.containers (previous_page_id, id) ;
               construct_container page_id profile_id ~id ~constructor ~parameters children >>= fun elt ->
               Hashtbl.add ctx.containers (page_id, id) (elt, (constructor, parameters)) ;
               Lwt.return elt
           with Not_found ->
             construct_container page_id profile_id~id ~constructor ~parameters children >>= fun elt ->
             Hashtbl.add ctx.containers (page_id, id) (elt, (constructor, parameters)) ;
             Lwt.return elt)
      ~do_component:
        (fun ~id ~constructor ~parameters () ->
           try
             let elt, (previous_constructor, previous_parameters) =
               Hashtbl.find ctx.components (page_id, id) in
             rebind_component page_id ~id ~constructor ~parameters ~previous_profile ~new_profile elt () >>= function
             | `Rebound -> Lwt.return elt
             | `Default
               when previous_constructor = constructor
                 && previous_parameters == parameters -> Lwt.return elt
             | `Default | `Reconstruct ->
               destruct_component previous_page_id ~id
                 ~constructor: previous_constructor
                 ~parameters: previous_parameters elt >>= fun () ->
               Hashtbl.remove ctx.components (previous_page_id, id) ;
               construct_component page_id profile_id ~id ~constructor ~parameters () >>= fun elt ->
               Hashtbl.add ctx.components (page_id, id) (elt, (constructor, parameters)) ;
               Lwt.return elt
           with Not_found ->
             construct_component page_id profile_id ~id ~constructor ~parameters () >>= fun elt ->
             Hashtbl.add ctx.components (page_id, id) (elt, (constructor, parameters)) ;
             Lwt.return elt) in
  match ctx.page_and_profile_ids with
  | Some (previous_page_id, previous_profile_id)
    when previous_page_id = page_id
      && previous_profile_id = profile_id ->
    Lwt.return ()
  | Some (previous_page_id, previous_profile_id) ->
    let previous_containers = Hashtbl.copy ctx.containers in
    let previous_components = Hashtbl.copy ctx.components in
    ignore (ctx.container##removeChild ((ctx.root :> Dom.node Js.t))) ;
    find_view page_id profile_id >>= fun view ->
    rebind
      ~previous_page_id
      ~page_id
      ~previous_profile: previous_profile_id
      ~new_profile: profile_id view >>= fun elt ->
    ctx.page_and_profile_ids <- Some (page_id, profile_id) ;
    ctx.root <- elt ;
    traverse
      ~do_container:
        (fun ~id ~constructor ~parameters _ ->
           Hashtbl.remove previous_containers (previous_page_id, id) ;
           Lwt.return ())
      ~do_component:
        (fun ~id ~constructor ~parameters () ->
           Hashtbl.remove previous_components (previous_page_id, id) ;
           Lwt.return ())
      view >>= fun () ->
    let previous_containers =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) previous_containers [] in
    let previous_components =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) previous_components [] in
    Lwt_list.iter_p
      (fun ((page, id), (elt, (constructor, parameters))) ->
         destruct_component page ~id ~constructor ~parameters elt >>= fun _ ->
         Hashtbl.remove ctx.components (page, id) ;
         Lwt.return ())
      previous_components >>= fun () ->
    Lwt_list.iter_p
      (fun ((page, id), (elt, (constructor, parameters))) ->
         destruct_container page ~id ~constructor ~parameters elt >>= fun _ ->
         Hashtbl.remove ctx.containers (page, id) ;
         Lwt.return ())
      previous_containers >>= fun () ->
    ignore (ctx.container##appendChild ((ctx.root :> Dom.node Js.t))) ;
    Lwt.return ()
  | None ->
    ctx.page_and_profile_ids <- Some (page_id, profile_id) ;
    find_view page_id profile_id >>= fun view ->
    construct page_id profile_id view >>= fun elt ->
    ctx.root <- elt ;
    ignore (ctx.container##appendChild ((ctx.root :> Dom.node Js.t))) ;
    Lwt.return ()
