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
    container_constructor : string ;
    container_parameters : Json_repr.value option ;
    container_priority : priority }

and component =
  { component_aspect_ratio : float range ;
    component_constructor : string ;
    component_parameters : Json_repr.value option ;
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
  | Container of container id * element list
  | Component of component id
  | Anonymous_container of string * Json_repr.value option * element list
  | Anonymous_component of string * Json_repr.value option

and constructor = string

and 'a id = string
and 'a table = ('a id * 'a) list

and 'a range =
  { min : ('a * [ `Closed | `Open ]) option ;
    max : ('a * [ `Closed | `Open ]) option }

open Json_encoding

let any =
  { min = None ; max = None }

let id_encoding value_encoding = string

let table_encoding value_encoding = assoc value_encoding

let range_encoding ?(compare = Pervasives.compare) enc =
  let bound =
  union
    [ case (obj1 (req "excluded" enc))
        (function (v, `Open) -> Some v | (_, `Closed) -> None)
        (fun v -> (v, `Open)) ;
      case enc
        (function (v, `Closed) -> Some v | (_, `Open) -> assert false)
        (fun v -> (v, `Closed)) ;
      case (obj1 (req "included" enc))
        (function (v, `Closed) -> Some v | (_, `Open) -> None)
        (fun v -> (v, `Closed)) ] in
  union
    [ case enc
        (function { min = Some (v, `Closed) ; max = Some (w, `Closed) } when v = w -> Some v | _ -> None)
        (fun v -> { min = Some (v, `Closed) ; max = Some (v, `Closed) }) ;
      case (obj2 (opt "min" bound) (opt "max" bound))
        (fun { min ; max } -> Some (min, max))
        (fun (min, max) -> { min ; max }) ]

let int_range_encoding =
  conv
    (fun { min ; max } ->
       { min = begin match min with
             | Some (v, `Closed) -> Some (v, `Closed)
             | Some (v, `Open) -> Some (succ v, `Closed)
             | None -> None
           end ;
         max = begin match max with
           | Some (v, `Closed) -> Some (v, `Closed)
           | Some (v, `Open) -> Some (pred v, `Closed)
           | None -> None
         end })
    (fun v -> v)
    (range_encoding int)

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
    (fun { container_extensible ;
           container_constructor ;
           container_parameters ;
           container_priority } ->
      (container_extensible,
       container_constructor,
       container_parameters ,
       container_priority))
    (fun (container_extensible,
          container_constructor,
          container_parameters,
          container_priority) ->
      { container_extensible ;
        container_constructor ;
        container_parameters ;
        container_priority }) @@
  obj4
    (dft "extensible" bool false)
    (req "constructor" string)
    (opt "parameters" any_value)
    (req "priority" priority_encoding)

let component_encoding =
  conv
    (fun { component_aspect_ratio ;
           component_constructor ;
           component_parameters ;
           component_priority } ->
      (component_aspect_ratio,
       component_constructor,
       component_parameters,
       component_priority))
    (fun (component_aspect_ratio,
          component_constructor,
          component_parameters,
          component_priority) ->
      { component_aspect_ratio ;
        component_constructor ;
        component_parameters ;
        component_priority }) @@
  obj4
    (dft "aspect_ratio" (range_encoding float) any)
    (req "constructor" string)
    (opt "parameters" any_value)
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
          (dft "displayWidth" (int_range_encoding) any)
          (dft "physicalDisplayWidth" (int_range_encoding) any)
          (dft "displayAspectRatio" (range_encoding float) any)
          (dft "deviceWidth" (int_range_encoding) any)
          (dft "physicalDeviceWidth" (int_range_encoding) any)
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
        (obj3
           (req "type" (string_enum ["container", ()]))
           (req "id" (id_encoding container_encoding))
           (req "children" (list element_encoding)))
        (function Container (id, children) -> Some ((), id, children) | _ -> None)
        (fun ((), id, children) -> Container (id, children)) ;
      case
        (obj4
           (req "type" (string_enum ["container", ()]))
           (req "constructor" string)
           (opt "parameters" any_value)
           (req "children" (list element_encoding)))
        (function Anonymous_container (params, cstr, children) ->
           Some ((), params, cstr,children) | _ -> None)
        (fun ((), params, cstr, children) ->
           Anonymous_container (params, cstr, children)) ;
      case
        (obj2
           (req "type" (string_enum ["component", ()]))
           (req "id" (id_encoding component_encoding)))
        (function Component id -> Some ((), id) | _ -> None)
        (fun ((), id) -> Component id) ;
      case
        (obj3
           (req "type" (string_enum ["component", ()]))
           (req "constructor" string)
           (opt "parameters" any_value))
        (function Anonymous_component (params, cstr) ->
           Some ((), params, cstr) | _ -> None)
        (fun ((), params, cstr) ->
           Anonymous_component (params, cstr)) ]

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
