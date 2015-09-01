(** AST *)

(************************************************************************)
(*  rddl - reactive design description language                         *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  rddl is distributed in the hope that it will be useful,             *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

(** Describes the set of user and device profiles supported by an app,
    and the toplevel structure of its UI for each of these profiles.

    A set of profiles is defined for the complete app, which describe
    both device capabilities and user preferences (for instance: small
    screen with high contrast, big screen without ads, etc.).

    The UI is split into pages (for instance: a login page, a media
    playback page or a settings page). Each page is implemented by
    several views that correspond to a given profile (or several
    ones). Views should be provided for all of the profiles.

    A view is a tree of components and containers. It arranges
    predefined elements (for instance a tool bar or a reflowable
    succession) and a set of application specific components as
    defined by the page it implements.

    Each page defines required components and containers, which must
    appear on every view of the page. It also defines optional ones,
    which can appear in views, but are not mandatory. No other
    component should appear in the views implementing the page. *)

(** The toplevel definition. *)
type ui =
  { pages : page table ;
    (** The set of pages. *)
    profiles : profile table
    (** The set of supported profiles. *) }

(** Definition of a page. *)
and page =
  { views : view list ;
    (** The list of views that implement this page. *)
    components : component table ;
    (** The set of identified components of the page. *)
    containers : container table ;
    (** The set of identified containers of the page. *)
    sources : source table
    (** External data sources whose disponibility may impact the UI. *) }

(** Definition of a view. *)
and view =
  { document : element ;
    (** The root container or component. *)
    compatible_profiles : profile handle list
    (** The profiles to which this view belongs. *) }

(** Container specific attributes. *)
and container =
  { container_extensible : bool ;
    (** Tells if the contents can be updated from the code.*)
    container_priority : priority
    (** The element's display priority. *) }

(** Component specific attributes. *)
and component =
  { component_aspect_ratio : float range ;
    (** The aspect ratios supported by the code. *)
    component_priority : priority
    (** The element's display priority. *) }

(** Discriminates between main and accessory elements. *)
and priority =
  (** Can be omitted in some views of the page. *)
  | Optional
  (** Must be present in all views but can be scrolled out of sight. *)
  | Hideable
  (** Must be present in all views and displayed at all times. *)
  | Required

(** An external data source. *)
and source =
  (** A known host to use a predefined polling method. *)
  | Uri of string
  (** A custom source that must be instanciated by the code. *)
  | Custom

(** A profile description. *)
and profile =
  { output : [ `textual | `simplified | `fancy ] range ;
    interactivity : [ `view_only | `pointer | `single_touch | `multi_touch  ] ;
    size : [ `display_width | `display_height | `device_width | `device_height ] * float range ;
    aspect_ratio : [ `device | `display ] * float range (* width / height *) ;
    resolution : float range (* dots per inch *) ;
    contrast : [ `low | `normal | `high ] range ;
    ink : [ `low | `normal | `high ] range ;
    zoom : [ `low | `normal | `high ] range ;
    connected : source handle list ;
    bandwitdh : (source handle * int range) list (* kilobits per second *) }

(** The tree type of containers and components. *)
and element =
  | Vertical of element list * container handle option
  | Horizontal of element list * container handle option
  | Flow of element list * container handle option
  | Menu of element list * container handle option
  | Custom_container of element list * container handle
  | Label of string * component handle option
  | Custom_component of component handle


(** An explicitly named ['a] node. *)
and 'a handle =
  { id : string ;
    (** The machine readable name. *)
    name : string ;
    (** The human readable name / description. *)
    resolved : 'a
    (** The boxed value. *) }

(** A set of named values. *)
and 'a table = 'a handle list

(** A range of ['a]s. [{ min = None ; max = None }] means any value. *)
and 'a range =
  { min : 'a option ;
    (** The inclusive minimum. *)
    max : 'a option
    (** The inclusive maximum. *) }

(* Voir: material UI *)

(*
open Json_encoding

let element_encoding =
  mu "element" @@ fun self ->
  let component_encoding =
    conv
      (fun { name; hideable; optional } -> (name, hideable, optional))
      (fun (name, hideable, optional) -> { name; hideable; optional }) @@
    obj3
      (req "name" string)
      (req "hideable" bool)
      (req "optional" bool) in
  let properties_encoding =
    conv
      (fun { children; dynamic; component } -> (children, dynamic, component))
      (fun (children, dynamic, component) -> { children; dynamic; component }) @@
    obj3
      (req "children" (list self))
      (req "dynamic" bool)
      (opt "component" component_encoding) in
  let with_kind kind obj =
    conv (fun o -> ((), o)) (fun ((), o) -> o) @@
    merge_objs (obj1 (req "kind" (string_enum [ kind, () ]))) obj in
  union
    [ case
        (with_kind "vertical" properties_encoding)
        (function Vertical props -> Some props | _ -> None)
        (fun props -> Vertical props) ;
      case
        (with_kind "horizontal" properties_encoding)
        (function Horizontal props -> Some props | _ -> None)
        (fun props -> Horizontal props) ;
      case
        (with_kind "menu" properties_encoding)
        (function Menu props -> Some props | _ -> None)
        (fun props -> Menu props) ;
      case
        (with_kind "component" component_encoding)
        (function Component props -> Some props | _ -> None)
        (fun props -> Component props) ]

let () =
  Json_encoding.schema element_encoding |>
  Json_schema.to_json |>
  Ezjsonm.to_channel ~minify:false stdout
*)
