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

module Browser_encoding =
  Json_encoding.Make (Json_repr_browser.Repr)

let pretty_print_profile profile =
  let json = Browser_encoding.construct Rddl_ast.profile_encoding profile in
  Json_repr_browser.stringify ~indent: 2 json

let do_anim t ~start ~step ~stop =
  start () >>= fun () ->
  step 0. >>= fun () ->
  let t0 = Unix.gettimeofday () in
  let rec loop () =
    let now = Unix.gettimeofday () in
    if now -. t0 > t then
      Lwt.return ()
    else
      step (now -. t0) >>= fun () ->
      Lwt_js.sleep 0.02 >>= fun () ->
      loop () in
  loop () >>= fun () ->
  step t >>= fun () ->
  stop ()

let construct_component ~page_id ~component_id ~profile_id component =
  match component.component_constructor with
  | "video-player" ->
    let construct () =
      let elt =
        let open Tyxml_js.Svg in
        Tyxml_js.Html.svg
          ~a: [ a_viewBox (0., 0., 1920., 1080.) ]
          [ rect ~a: [ a_width (1920., None) ;
                       a_height (1080., None) ;
                       a_x (0., None);
                       a_y (0., None) ;
                       a_fill (`Color ("black", None)) ]
              [] ;
            text ~a: [ a_x_list [ (810., None) ];
                       a_y_list [ (540., None) ] ;
                       a_font_size "200" ;
                       a_text_anchor `Middle ;
                       a_fill (`Color ("red", None)) ]
              [ pcdata "Video Player" ] ;
          ] in
      Tyxml_js.To_dom.of_element elt in
    `Immediate construct
  | "video-information" ->
    let construct () =
      let div = Tyxml_js.Html.(div [ pcdata "Video infos." ]) in
      Tyxml_js.To_dom.of_element div in
    `Immediate construct
  | _ -> `Default

let () =
  Random.self_init () ;
  Lwt.async @@ fun () ->
  let hash = Url.Current.get_fragment () in
  Lwt.catch
    (fun () ->
       if hash = "" then raise Not_found ;
       let url = "samples/" ^ hash ^ ".rddl.json" in
       XmlHttpRequest.perform_raw
         ~response_type:XmlHttpRequest.Text
         url >>= fun { XmlHttpRequest.code ; content } ->
       if code <> 200 then
         Lwt.fail_with ("HTTP code " ^ string_of_int code)
       else
         let { profiles ; pages } as ui =
           let json = Json_repr_browser.parse_js_string content in
           Browser_encoding.destruct Rddl_ast.ui_encoding json in
         let state_text_signal, set_state_text = React.S.create "" in
         let profile_id_signal, set_profile_id = React.S.create "" in
         let panel_contents =
           let open Tyxml_js.Html in
           [ h3 [ pcdata "Pages:" ] ;
             div (List.map (fun (id, _) -> button [ pcdata id ]) pages) ;
             h3 [ pcdata "State:" ] ;
             pre [ Tyxml_js.R.Html.pcdata state_text_signal ] ;
             h3 [ pcdata "Profile:" ] ;
             ul (List.map
                   (fun (id, _) ->
                      let style =
                        React.S.map
                          (fun id' -> if id = id' then "" else "list-style-type: none;")
                          profile_id_signal in
                      li ~a: [ Tyxml_js.R.Html.a_style style ] [ pcdata id ])
                   profiles)
           ]
         in
         Tyxml_js.Register.id "rddl-demo-panel" panel_contents ;
         let container =
           Js.Opt.case
             (Dom_html.window##.document##getElementById (Js.string "rddl-demo-container"))
             (fun () -> assert false)
             (fun div -> div) in
         let updates = Rddl_profiler.div container in
         let changes = Rddl_profiler.changes updates profiles in
         let renderer_ctx = Rddl_renderer.div container ~construct_component ui in
         let transition_div =
           let transition_div_style =
             "background-color: black; \
              position: absolute; display: none; z-index: 1000;\
              left: 0; right: 0; bottom: 0; top:0;" in
           let div = Dom_html.createDiv Dom_html.document in
           div##setAttribute (Js.string "style") (Js.string transition_div_style) ;
           ignore (container##appendChild ((div :> Dom.node Js.t))) ;
           div in
         let tt = 0.2 in
         let rec start_transition () =
           do_anim tt
             ~start:(fun () ->
                 transition_div##.style##.display := Js.string "block" ;
                 Lwt.return ())
             ~step:(fun t ->
                 let d = Printf.sprintf "%0.2f" (t /. tt) in
                 transition_div##.style##.opacity := Js.Optdef.return (Js.string d) ;
                 Lwt.return ())
             ~stop: Lwt.return in
         let rec end_transition () =
           do_anim tt
             ~start: Lwt.return
             ~step:(fun t ->
                 let d = Printf.sprintf "%0.2f" (1. -. (t /. tt)) in
                 transition_div##.style##.opacity := Js.Optdef.return (Js.string d) ;
                 Lwt.return ())
             ~stop:(fun () ->
                 transition_div##.style##.display := Js.string "none" ;
                 Lwt.return ()) in
         Lwt.pick
           [ (Rddl_profiler.on_update updates @@ fun profile ->
              set_state_text (pretty_print_profile profile) ;
              Lwt.return ()) ;
             (Rddl_profiler.on_change changes @@ fun _ ->
              let (id, profile) = Rddl_profiler.selection changes in
              set_profile_id id ;
              Rddl_renderer.render renderer_ctx
                ~transitions: (start_transition, end_transition)
                ~page_id: (fst (List.hd pages))
                ~profile_id: id
                ()) ])
    (function exn ->
       let message =
         Format.asprintf "@[<v 0>%a@." (fun ppf -> Json_encoding.print_error ppf) exn in
       Firebug.console##error (Js.string message);
       Lwt.return ())
