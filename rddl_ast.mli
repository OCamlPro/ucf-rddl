(** RDDL - OCaml Abstract Syntax Tree and JSON encoding *)

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

(** Output selector *)
and output_level = Textual | Simplified | Fancy

(** Interactivity selector *)
and interactivity_level = View_only | Pointer | Single_touch | Multi_touch

(** A simple three steps intensity selector *)
and three_steps_level = Low | Normal | High

(** The tree type of containers and components. *)
and element =
  | Container of constructor * element list * container id option
  | Component of constructor * component id

(** The backend-dependent container/component kind. *)
and constructor = string

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

(** The full range. *)
val any : 'a range

(** A range with only its lower limit defined. *)
val from : 'a -> 'a range

(** A range with only its upper limit defined. *)
val upto : 'a -> 'a range

(** A range with both limits defined. *)
val between : 'a -> 'a -> 'a range

(** A singleton range. *)
val only : 'a -> 'a range

(** The JSON encoding. *)
val ui_encoding : ui Json_encoding.encoding
