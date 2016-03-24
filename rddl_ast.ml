(* RDDL - OCaml Abstract Syntax Tree and JSON encoding *)

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

type ui =
  { pages : page table ;
    profiles : profile table }

and page =
  { views : view list ;
    components : component table ;
    containers : container table ;
    sources : source table }

and view =
  { document : element ;
    compatible_profiles : profile id list }

and container =
  { container_extensible : bool ;
    container_priority : priority }

and component =
  { component_aspect_ratio : float range ;
    component_priority : priority }

and priority =
  | Optional
  | Hideable
  | Required

and source =
  | Uri of string
  | Custom

and profile =
  { output : output_level range ;
    interactivity : interactivity_level range ;
    display_width : int range ;
    physical_display_width : int range ;
    display_aspect_ratio : float range (* width / height *) ;
    device_width : int range ;
    physical_device_width : int range ;
    device_aspect_ratio : float range (* width / height *) ;
    contrast : three_steps_level range ;
    ink : three_steps_level range ;
    zoom : three_steps_level range ;
    connected : source id list ;
    bandwidth : (source id * int range) list (* kilobits per second *) }

and output_level = Textual | Simplified | Fancy
and interactivity_level = View_only | Pointer | Single_touch | Multi_touch
and three_steps_level = Low | Normal | High

and element =
  | Container of constructor * element list * container id option
  | Component of constructor * component id

and constructor = string

and 'a id = string
and 'a table = ('a id * 'a) list

and 'a range =
  { min : 'a option ;
    max : 'a option }

open Json_encoding

let any =
  { min = None ; max = None }

let between min max =
  { min = Some min ; max = Some max }

let from min =
  { min = Some min ; max = None }

let upto max =
  { min = None ; max = Some max }

let only v =
  { min = Some v ; max = Some v }

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
    (dft "aspect_ratio" (range_encoding float) any)
    (req "priority" priority_encoding)

let profile_encoding =
  def "profile" @@
  conv
    (fun { output ; interactivity ;
           display_width ; physical_display_width ; display_aspect_ratio ;
           device_width ; physical_device_width ; device_aspect_ratio ;
           contrast ; ink ; zoom ;
           connected ; bandwidth } ->
      (((output, interactivity),
        (display_width, physical_display_width, display_aspect_ratio,
         device_width, physical_device_width, device_aspect_ratio)),
       ((contrast, ink, zoom),
        (connected, bandwidth))))
    (fun
      (((output, interactivity),
        (display_width, physical_display_width, display_aspect_ratio,
         device_width, physical_device_width, device_aspect_ratio)),
       ((contrast, ink, zoom),
        (connected, bandwidth))) ->
      { output ; interactivity ;
        display_width ; physical_display_width ; display_aspect_ratio ;
        device_width ; physical_device_width ; device_aspect_ratio ;
        contrast ; ink ; zoom ;
        connected ; bandwidth }) @@
  merge_objs
    (merge_objs
       (obj2
          (dft "output"
             (range_encoding
                (string_enum [ "textual", Textual ;
                               "simplified", Simplified ;
                               "fancy", Fancy ]))
             any)
          (dft "interactivity"
             (range_encoding
                (string_enum [ "view only", View_only ;
                               "pointer", Pointer ;
                               "single touch", Single_touch ;
                               "multi touch", Multi_touch ]))
             any))
       (obj6
          (dft "displayWidth" (range_encoding int) any)
          (dft "physicalDisplayWidth" (range_encoding int) any)
          (dft "displayAspectRatio" (range_encoding float) any)
          (dft "deviceWidth" (range_encoding int) any)
          (dft "physicalDeviceWidth" (range_encoding int) any)
          (dft "deviceAspectRatio" (range_encoding float) any)))
    (merge_objs
       (let three_steps_level_encoding =
          string_enum [ "low", Low ; "normal", Normal ; "high", High ] in
        obj3
          (dft "contrast" (range_encoding three_steps_level_encoding) any)
          (dft "ink" (range_encoding three_steps_level_encoding) any)
          (dft "zoom" (range_encoding three_steps_level_encoding) any))
       (obj2
          (dft "connected" (list (id_encoding source_encoding)) [])
          (dft "bandwidth" (assoc (range_encoding int)) [])))

let element_encoding =
  mu "element" @@ fun element_encoding ->
  union
    [ case
        (obj4
           (req "type" (string_enum ["container", ()]))
           (req "constructor" string)
           (req "children" (list element_encoding))
           (opt "id" (id_encoding container_encoding)))
        (function Container (constructor, children, id) -> Some ((), constructor, children, id) | _ -> None)
        (fun ((), constructor, children, id) -> Container (constructor, children, id)) ;
      case
        (obj3
           (req "type" (string_enum ["component", ()]))
           (req "constructor" string)
           (req "id" (id_encoding component_encoding)))
        (function Component (constructor, id) -> Some ((), constructor, id) | _ -> None)
        (fun ((), constructor, id) -> Component (constructor, id)) ]

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
