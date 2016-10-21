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

let genid =
  let cur = ref 0 in
  fun () -> incr cur ; "anon" ^ string_of_int !cur

let resolve page { containers ; components } profile root =
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

type ('param, 'result) rendering =
  [ `Immediate of 'param -> 'result
  | `Running of 'param -> 'result Lwt.t ]

let (>>>) x f =
  match f, x with
  | `Immediate f, `Immediate x ->
    `Immediate (fun a -> f (x a))
  | `Immediate f, `Running x ->
    `Running (fun a ->
        x a >>= fun x ->
        Lwt.return (f x))
  | `Running f, `Running x ->
    `Running (fun a ->
        x a >>= fun x ->
        f x)
  | `Running f, `Immediate x ->
    `Running (fun a -> f (x a))

let tee fl fr =
  match fl, fr with
  | `Running fl, `Running fr ->
    `Running (fun x -> fl x >>= fun v -> fr (x, v))
  | `Immediate fl, `Running fr ->
    `Running (fun x -> fr (x, fl x))
  | `Running fl, `Immediate fr ->
    `Running (fun x -> fl x >>= fun v -> Lwt.return (fr (x, v)))
  | `Immediate fl, `Immediate fr ->
    `Immediate (fun x -> let v = fl x in fr (x, v))

type component_constructor =
  page_id: page id ->
  component_id: component id ->
  profile_id: profile id ->
  component ->
  [ (unit, Dom_html.element Js.t) rendering
  | `Default ]

type component_destructor =
  page_id: page id ->
  component_id: component id ->
  component ->
  [ (Dom_html.element Js.t, unit) rendering
  | `Default ]

type component_rebinder =
  page_id: page id ->
  component_id: component id ->
  previous_profile_id: profile id ->
  new_profile_id: profile id ->
  component ->
  [ (Dom_html.element Js.t, Dom_html.element Js.t) rendering
  | `Default
  | `Reconstruct ]

type rendered =
  { element : Dom_html.element Js.t ;
    kind : [ `Container of container | `Component of component ] }

type container_constructor =
  page_id: page id ->
  container_id: container id ->
  profile_id: profile id ->
  container ->
  [ (rendered list, Dom_html.element Js.t) rendering
  | `Default ]

type container_destructor =
  page_id: page id ->
  container_id: container id ->
  container ->
  [ (Dom_html.element Js.t * rendered list, unit) rendering
  | `Default ]

type container_rebinder =
  page_id: page id ->
  container_id: container id ->
  previous_profile_id: profile id ->
  new_profile_id: profile id ->
  container ->
  [ (Dom_html.element Js.t * rendered list, Dom_html.element Js.t) rendering
  | `Default
  | `Reconstruct ]

type context =
  { container : Dom_html.element Js.t ;
    mutable root : Dom_html.element Js.t ;
    components : ((page id * component id), Dom_html.element Js.t * component) Hashtbl.t ;
    containers : ((page id * component id), Dom_html.element Js.t * rendered list * container) Hashtbl.t ;
    construct_component : component_constructor ;
    destruct_component : component_destructor ;
    rebind_component : component_rebinder ;
    construct_container : container_constructor ;
    destruct_container : container_destructor ;
    rebind_container : container_rebinder ;
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

let div container
    ?(construct_component =
      fun ~page_id ~component_id ~profile_id component -> `Default)
    ?(destruct_component =
      fun ~page_id ~component_id component -> `Default)
    ?(rebind_component =
      fun ~page_id ~component_id ~previous_profile_id ~new_profile_id component -> `Default)
    ?(construct_container =
      fun ~page_id ~container_id ~profile_id container -> `Default)
    ?(destruct_container =
      fun ~page_id ~container_id container -> `Default)
    ?(rebind_container =
      fun ~page_id ~container_id ~previous_profile_id ~new_profile_id container -> `Default)
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
      (Dom_html.window##.document##getElementById (Js.string id))
      (fun () ->
         let div = Dom_html.createDiv (Dom_html.window##.document) in
         div##setAttribute (Js.string "id") (Js.string id) ;
         div##setAttribute (Js.string "style") (Js.string main_container_style) ;
         ignore (Dom_html.window##.document##.body##appendChild ((div :> Dom.node Js.t))) ;
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

let rec chained_call = function
  | [] -> `Default
  | f :: fs ->
    match f () with
    | `Default -> chained_call fs
    | handled -> handled

let construct_container ctx ~page_id ~container_id ~profile_id container =
  match
    chained_call @@
    List.map
      (fun f () -> f ~page_id ~container_id ~profile_id container)
      (ctx.construct_container :: !container_constructors)
  with
  | `Default ->
    failwith
      ("Cannot find a suitable container constructor" ^
       " for " ^ container.container_constructor ^
       " with id " ^ container_id ^
       " in page " ^ page_id ^
       " and profile " ^ profile_id)
  | `Immediate _ | `Running _ as handled -> handled

let construct_component ctx ~component_id ~page_id ~profile_id component =
  match
    chained_call @@
    List.map
      (fun f () -> f ~page_id ~component_id ~profile_id component)
      (ctx.construct_component :: !component_constructors)
  with
  | `Default ->
    failwith
      ("Cannot find a suitable component constructor" ^
       " for " ^ component.component_constructor ^
       " with id " ^ component_id ^
       " in page " ^ page_id ^
       " and profile " ^ profile_id)
  | `Immediate _ | `Running _ as handled -> handled

let destruct_container ctx ~page_id ~container_id container =
  match
    chained_call @@
    List.map
      (fun f () -> f ~page_id ~container_id container)
      (ctx.destruct_container :: !container_destructors)
  with
  | `Default -> `Immediate (fun _ -> ())
  | `Immediate _ | `Running _ as handled -> handled

let destruct_component ctx ~page_id ~component_id component =
  match
    chained_call @@
    List.map
      (fun f () -> f ~page_id ~component_id component)
      (ctx.destruct_component :: !component_destructors)
  with
  | `Default -> `Immediate (fun _ -> ())
  | `Immediate _ | `Running _ as handled -> handled

let rebind_container ctx ~page_id ~container_id ~previous_profile_id ~new_profile_id container =
  match
    chained_call @@
    List.map
      (fun f () -> f ~page_id ~container_id ~previous_profile_id ~new_profile_id container)
      (ctx.rebind_container :: !container_rebinders)
  with
  | `Default -> `Immediate (fun (elt, _) -> elt)
  | `Reconstruct ->
    tee
      (destruct_container ctx ~page_id ~container_id container)
      (`Immediate (fun ((_, children), ()) -> children) >>>
       construct_container ctx ~page_id ~container_id ~profile_id: new_profile_id container)
  | `Immediate _ | `Running _ as handled -> handled

let rebind_component ctx ~page_id ~component_id ~previous_profile_id ~new_profile_id component =
  match
    chained_call @@
    List.map
      (fun f () -> f ~page_id ~component_id ~previous_profile_id ~new_profile_id component)
      (ctx.rebind_component :: !component_rebinders)
  with
  | `Default -> `Immediate (fun elt -> elt)
  | `Reconstruct ->
    destruct_component ctx ~page_id ~component_id component >>>
    construct_component ctx ~page_id ~component_id ~profile_id: new_profile_id component
  | `Immediate _ | `Running _ as handled -> handled

let find_view ctx ~page_id ~profile_id =
  try
    let page = List.assoc page_id ctx.ui.pages in
    let { document } =
      List.find
        (fun { compatible_profiles = cps } -> List.mem profile_id cps)
        page.views in
    Lwt.return (resolve page_id page profile_id document)
  with
  | Not_found ->
    Lwt.fail_with ("No view for page " ^ page_id ^
                   " and profile " ^ profile_id)
  | exn -> Lwt.fail exn

let rec traverse_view ~do_component ~do_container = function
  | Resolved_container (container_id, container, children) ->
    let results =
      List.map
        (traverse_view ~do_component ~do_container)
        children in
    let parameters =
      match container.container_parameters with
      | Some p -> Some (Json_repr.any_to_repr (module Json_repr_browser.Repr) p)
      | None -> None in
    do_container ~container_id container parameters results
  | Resolved_component (component_id, component) ->
    let parameters =
      match component.component_parameters with
      | Some p -> Some (Json_repr.any_to_repr (module Json_repr_browser.Repr) p)
      | None -> None in
    do_component ~component_id component parameters

let render ctx
    ~page_id: new_page_id ~profile_id: new_profile_id
    ?transitions () =
  let run_with_transitions f x = match f with
    | `Immediate f -> Lwt.return (f x)
    | `Running f ->
      match transitions with
      | None -> f x
      | Some (tr_in, tr_out) ->
        tr_in () >>= fun () ->
        f x >>= fun r ->
        tr_out () >>= fun () ->
        Lwt.return r in
  let run f x = match f with
    | `Immediate f -> Lwt.return (f x)
    | `Running f -> f x in
  let children_rendering children =
    let rec loop acc rest =
      match acc, rest with
      | l, [] ->
        `Immediate (fun () -> List.map (fun f -> f ()) (List.rev l))
      | l, `Immediate child :: rest ->
        loop (child :: acc) rest
      | _, `Running _ :: _ ->
        `Running (fun () ->
            Lwt_list.map_p
              (function
                | `Immediate f -> Lwt.return (f ())
                | `Running f -> f ())
              children)
    in loop [] children in
  let construct ~page_id ~profile_id =
    traverse_view
      ~do_container:
        (fun ~container_id container parameters children ->
           children_rendering children >>>
           tee
             (construct_container ctx
                ~page_id ~profile_id ~container_id
                container)
             (`Immediate
                (fun (children, elt) ->
                   Hashtbl.add ctx.containers (page_id, container_id) (elt, children, container) ;
                   { element = elt ; kind = `Container container })))
      ~do_component:
        (fun ~component_id component parameters ->
           construct_component ctx
             ~page_id ~profile_id ~component_id
             component >>>
           `Immediate
             (fun elt ->
                Hashtbl.add ctx.components (page_id, component_id) (elt, component) ;
                { element = elt ; kind = `Component component })) in
  let rebind ~previous_page_id ~page_id ~previous_profile_id ~new_profile_id =
    traverse_view
      ~do_container:
        (fun ~container_id container parameters children ->
           try
             let elt, _, _ =
               Hashtbl.find ctx.containers (page_id, container_id) in
             children_rendering children >>>
             tee
               (`Immediate (fun children -> (elt, children)) >>>
                rebind_container ctx
                  ~page_id ~container_id ~previous_profile_id ~new_profile_id
                  container)
               (`Immediate (fun (children, elt) ->
                    Hashtbl.remove ctx.containers (previous_page_id, container_id) ;
                    Hashtbl.add ctx.containers (page_id, container_id) (elt, children, container) ;
                    { element = elt ; kind = `Container container }))
           with Not_found ->
             children_rendering children >>>
             tee
               (construct_container ctx
                  ~page_id ~profile_id: new_profile_id ~container_id
                  container)
               (`Immediate
                  (fun (children, elt) ->
                     Hashtbl.add ctx.containers (page_id, container_id) (elt, children, container) ;
                     { element = elt ; kind = `Container container })))
      ~do_component:
        (fun ~component_id component parameters ->
           try
             let elt, previous_component =
               Hashtbl.find ctx.components (page_id, component_id) in
             `Immediate (fun () -> elt) >>>
             rebind_component ctx
               ~page_id ~component_id ~previous_profile_id ~new_profile_id
               component >>>
             `Immediate (fun elt ->
                 Hashtbl.remove ctx.components (previous_page_id, component_id) ;
                 Hashtbl.add ctx.components (page_id, component_id) (elt, component) ;
                 { element = elt ; kind = `Component component })
           with Not_found ->
             construct_component ctx
               ~page_id ~profile_id: new_profile_id ~component_id
               component >>>
             `Immediate
               (fun elt ->
                  Hashtbl.add ctx.components (page_id, component_id) (elt, component) ;
                  { element = elt ; kind = `Component component })) in
  match ctx.page_and_profile_ids with
  | Some (previous_page_id, previous_profile_id)
    when previous_page_id = new_page_id
      && previous_profile_id = new_profile_id ->
    Lwt.return ()
  | Some (previous_page_id, previous_profile_id) ->
    let previous_containers = Hashtbl.copy ctx.containers in
    let previous_components = Hashtbl.copy ctx.components in
    find_view ctx ~page_id: new_page_id ~profile_id: new_profile_id >>= fun view ->
    traverse_view
      ~do_container:
        (fun ~container_id container parameters children ->
           Hashtbl.remove previous_containers (previous_page_id, container_id) ;
           Lwt.return ())
      ~do_component:
        (fun ~component_id component parameters ->
           Hashtbl.remove previous_components (previous_page_id, component_id) ;
           Lwt.return ())
      view >>= fun () ->
    let previous_containers =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) previous_containers [] in
    let previous_components =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) previous_components [] in
    Lwt_list.iter_p
      (fun ((page_id, component_id), (elt, component)) ->
         run (destruct_component ctx ~page_id ~component_id component) elt >>= fun _ ->
         Hashtbl.remove ctx.components (page_id, component_id) ;
         Lwt.return ())
      previous_components >>= fun () ->
    Lwt_list.iter_p
      (fun ((page_id, container_id), (elt, children, container)) ->
         run (destruct_container ctx ~page_id ~container_id container) (elt, children) >>= fun _ ->
         Hashtbl.remove ctx.containers (page_id, container_id) ;
         Lwt.return ())
      previous_containers >>= fun () ->
    run_with_transitions
      (`Immediate (fun () -> ignore (ctx.container##removeChild ((ctx.root :> Dom.node Js.t)))) >>>
       rebind ~previous_page_id ~page_id: new_page_id ~previous_profile_id ~new_profile_id view >>>
       `Immediate (fun { element = elt } -> ignore (ctx.container##appendChild ((elt :> Dom.node Js.t))) ; elt))
      () >>= fun elt ->
    ctx.page_and_profile_ids <- Some (new_page_id, new_profile_id) ;
    ctx.root <- elt ;
    Lwt.return ()
  | None ->
    ctx.page_and_profile_ids <- Some (new_page_id, new_profile_id) ;
    find_view ctx ~page_id: new_page_id ~profile_id: new_profile_id >>= fun view ->
    run_with_transitions
      (construct ~page_id: new_page_id ~profile_id: new_profile_id view)
      () >>= fun { element = elt } ->
    ctx.root <- elt ;
    ignore (ctx.container##appendChild ((ctx.root :> Dom.node Js.t))) ;
    Lwt.return ()
