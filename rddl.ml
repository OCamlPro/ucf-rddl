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
    compatible_profiles : profile id list
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
  { output : output_level range ;
    interactivity : interactivity_level range ;
    display_width : int range ;
    device_width : int range ;
    display_aspect_ratio : float range (* width / height *) ;
    device_aspect_ratio : float range (* width / height *) ;
    resolution : float range (* dots per inch *) ;
    contrast : three_steps_level range ;
    ink : three_steps_level range ;
    zoom : three_steps_level range ;
    connected : source id list ;
    bandwitdh : (source id * int range) list (* kilobits per second *) }

and output_level = Textual | Simplified | Fancy
and interactivity_level = View_only | Pointer | Single_touch | Multi_touch
and three_steps_level = Low | Normal | High

(** The tree type of containers and components. *)
and element =
  | Vertical of element list * container id option
  | Horizontal of element list * container id option
  | Flow of element list * container id option
  | Menu of element list * container id option
  | Custom_container of element list * container id
  | Label of string * component id option
  | Custom_component of component id

(** An identifier for a value of a given type. *)
and 'a id = string

(** A table of identified values of a given type. *)
and 'a table = ('a id * 'a) list

(** A range of ['a]s. [{ min = None ; max = None }] means any value. *)
and 'a range =
  { min : 'a option ;
    (** The inclusive minimum. *)
    max : 'a option
    (** The inclusive maximum. *) }

let empty_range =
  { min = None ; max = None }

open Json_encoding

let id_encoding value_encoding = string

let table_encoding value_encoding = assoc value_encoding

let range_encoding ?(compare = Pervasives.compare) enc =
  union
    [ case enc
        (function { min = Some v ; max = Some v' } when v = v' -> Some v | _ -> None)
        (fun v -> { min = Some v ; max = Some v }) ;
      case (obj2 (opt "min" enc) (opt "max" enc))
        (fun { min ; max } -> Some (min, max))
        (fun (min, max) -> { min ; max }) ]

let source_encoding =
  def "source" @@
  union
    [ case
        (obj1 (req "uri" string))
        (function Uri uri -> Some uri | Custom -> None)
        (function uri -> Uri uri) ;
      case
        (string_enum [ "custom", () ])
        (function Uri _ -> None | Custom -> Some ())
        (function () -> Custom) ]

let priority_encoding =
  string_enum
    [ "optional", Optional ;
      "hideable", Hideable ;
      "required", Required ]

let container_encoding =
  conv
    (fun { container_extensible ; container_priority } ->
       (container_extensible, container_priority))
    (fun (container_extensible, container_priority) ->
       { container_extensible ; container_priority }) @@
  obj2
    (dft "extensible" bool false)
    (req "priority" priority_encoding)

let component_encoding =
  conv
    (fun { component_aspect_ratio ; component_priority } ->
       (component_aspect_ratio, component_priority))
    (fun (component_aspect_ratio, component_priority) ->
       { component_aspect_ratio ; component_priority }) @@
  obj2
    (dft "aspect_ratio" (range_encoding float) empty_range)
    (req "priority" priority_encoding)

let profile_encoding =
  def "profile" @@
  conv
    (fun { output ; interactivity ;
           display_width ; display_aspect_ratio ; device_width ; device_aspect_ratio ; resolution ;
           contrast ; ink ; zoom ;
           connected ; bandwitdh } ->
      (((output, interactivity),
        (display_width, display_aspect_ratio, device_width, device_aspect_ratio, resolution)),
       ((contrast, ink, zoom),
        (connected, bandwitdh))))
    (fun
      (((output, interactivity),
        (display_width, display_aspect_ratio, device_width, device_aspect_ratio, resolution)),
       ((contrast, ink, zoom),
        (connected, bandwitdh))) ->
      { output ; interactivity ;
        display_width ; display_aspect_ratio ; device_width ; device_aspect_ratio ; resolution ;
        contrast ; ink ; zoom ;
        connected ; bandwitdh }) @@
  merge_objs
    (merge_objs
       (obj2
          (dft "output"
             (range_encoding
                (string_enum [ "textual", Textual ;
                               "simplified", Simplified ;
                               "fancy", Fancy ]))
             empty_range)
          (dft "interactivity"
             (range_encoding
                (string_enum [ "view only", View_only ;
                               "pointer", Pointer ;
                               "single touch", Single_touch ;
                               "multi touch", Multi_touch ]))
             empty_range))
       (obj5
          (dft "displayWidth" (range_encoding int) empty_range)
          (dft "displayAspectRatio" (range_encoding float) empty_range)
          (dft "deviceWidth" (range_encoding int) empty_range)
          (dft "deviceAspectRatio" (range_encoding float) empty_range)
          (dft "resolution" (range_encoding float) empty_range)))
    (merge_objs
       (let three_steps_level_encoding =
          string_enum [ "low", Low ; "normal", Normal ; "high", High ] in
        obj3
          (dft "contrast" (range_encoding three_steps_level_encoding) empty_range)
          (dft "ink" (range_encoding three_steps_level_encoding) empty_range)
          (dft "zoom" (range_encoding three_steps_level_encoding) empty_range))
       (obj2
          (dft "connected" (list (id_encoding source_encoding)) [])
          (dft "bandwidth" (assoc (range_encoding int)) [])))

let element_encoding =
  mu "element" @@ fun element_encoding ->
  union
    [ case
        (obj3
           (req "type" (string_enum ["vertical", ()]))
           (req "children" (list element_encoding))
           (opt "id" (id_encoding container_encoding)))
        (function Vertical (children, id) -> Some ((), children, id) | _ -> None)
        (fun ((), children, id) -> Vertical (children, id)) ;
      case
        (obj3
           (req "type" (string_enum ["horizontal", ()]))
           (req "children" (list element_encoding))
           (opt "id" (id_encoding container_encoding)))
        (function Horizontal (children, id) -> Some ((), children, id) | _ -> None)
        (fun ((), children, id) -> Horizontal (children, id)) ;
      case
        (obj3
           (req "type" (string_enum ["flow", ()]))
           (req "children" (list element_encoding))
           (opt "id" (id_encoding container_encoding)))
        (function Flow (children, id) -> Some ((), children, id) | _ -> None)
        (fun ((), children, id) -> Flow (children, id)) ;
      case
        (obj3
           (req "type" (string_enum ["menu", ()]))
           (req "children" (list element_encoding))
           (opt "id" (id_encoding container_encoding)))
        (function Menu (children, id) -> Some ((), children, id) | _ -> None)
        (fun ((), children, id) -> Menu (children, id)) ;
      case
        (obj3
           (req "type" (string_enum ["custom_container", ()]))
           (req "children" (list element_encoding))
           (req "id" (id_encoding container_encoding)))
        (function Custom_container (children, id) -> Some ((), children, id) | _ -> None)
        (fun ((), children, id) -> Custom_container (children, id)) ;
      case
        (obj3
           (req "type" (string_enum ["label", ()]))
           (dft "text" string "")
           (opt "id" (id_encoding component_encoding)))
        (function Label (text, id) -> Some ((), text, id) | _ -> None)
        (fun ((), text, id) -> Label (text, id)) ;
      case
        (obj2
           (req "type" (string_enum ["custom_component", ()]))
           (req "id" (id_encoding component_encoding)))
        (function Custom_component id -> Some ((), id) | _ -> None)
        (fun ((), id) -> Custom_component id) ]

let view_encoding =
  conv
    (fun { document ; compatible_profiles } ->
       (document, compatible_profiles))
    (fun (document, compatible_profiles) ->
       { document ; compatible_profiles }) @@
  obj2
    (req "document" element_encoding)
    (req "profiles" (list (id_encoding profile_encoding)))

let page_encoding =
  def "page" @@
  conv
    (fun { views ; components ; containers ; sources } ->
       (views, components, containers, sources))
    (fun (views, components, containers, sources) ->
       { views ; components ; containers ; sources }) @@
  obj4
    (req "views" (list view_encoding))
    (dft "components" (table_encoding component_encoding) [])
    (dft "containers" (table_encoding container_encoding) [])
    (dft "sources" (table_encoding source_encoding) [])

let ui_encoding =
  conv
    (fun { pages ; profiles } -> (pages, profiles))
    (fun (pages, profiles) -> { pages ; profiles }) @@
  obj2
    (req "pages" (table_encoding page_encoding))
    (req "profiles" (table_encoding profile_encoding))

let () =
  Json_encoding.schema ui_encoding |>
  Json_schema.to_json |>
  Ezjsonm.to_channel ~minify:false stdout ;
  Printf.fprintf stdout "\n%!" ;
  let rec main () =
    try
      let json = Ezjsonm.from_channel stdin in
      ignore (destruct ui_encoding json)
    with err ->
      let print_unknown ppf = function
        | Ezjsonm.Parse_error (_, err) ->
          Format.fprintf ppf "%s" err
        | exn -> raise exn in
      Format.eprintf "%a@." (print_error ~print_unknown) err ;
      main () in
  main ()
