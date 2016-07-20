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
         let updates = Rddl_profiler.window in
         let changes = Rddl_profiler.changes updates profiles in
         let renderer_ctx =
           let genstyle () =
             let r = Random.int 60 + 120 in
             let g = Random.int 60 + 120 in
             let b = Random.int 60 + 120 in
             Format.asprintf
               "display: block; \
                background-color: #%02X%02X%02X;" r g b in
           let construct_component ~page ~id ~constructor ~parameters ~profile () =
             let div = Dom_html.createDiv Dom_html.document in
             div##setAttribute (Js.string "style", Js.string (genstyle ())) ;
             div##innerHTML <- Js.string
                 (Format.asprintf "%s/%s/%s/%s" page id constructor profile) ;
             Lwt.return (`Constructed div) in
           let construct_container ~page ~id ~constructor ~parameters ~profile children =
             let div = Dom_html.createDiv Dom_html.document in
             div##setAttribute (Js.string "style", Js.string (genstyle ())) ;
             div##innerHTML <- Js.string
                 (Format.asprintf "%s/%s/%s/%s" page id constructor profile) ;
             List.iter
               (fun child ->
                  let child = (child :> Dom.node Js.t) in
                  ignore (div##appendChild (child)))
               children ;
             Lwt.return (`Constructed div) in
           Rddl_renderer.window
             ~construct_component
             ~construct_container
             ui in
         Rddl_profiler.on_update updates
           (fun profile ->
              let (id, profile) = Rddl_profiler.selection changes in
              Firebug.console##debug (Js.string ("Profile: `" ^ id ^ "`."));
              Rddl_renderer.render renderer_ctx (fst (List.hd pages)) id))
    (function exn ->
       let message =
         Format.asprintf "@[<v 2>Error loading `%s.rddl.json`:@,%a@]"
           hash
           (fun ppf -> Json_encoding.print_error ppf) exn in
       Firebug.console##error (Js.string message);
       Lwt.return ())
