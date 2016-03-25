(* RDDL - Profile operations *)

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

(* range builders *******************************************************)

let any =
  { min = None ; max = None }

let from min =
  { min = Some (min, `Closed) ; max = None }

let above min =
  { min = Some (min, `Open) ; max = None }

let upto max =
  { min = None ; max = Some (max, `Closed) }

let below max =
  { min = None ; max = Some (max, `Open) }

let only v =
  { min = Some (v, `Closed) ; max = Some (v, `Closed) }

(* range operations *****************************************************)

let wellformed_range = function
  | { min = None } | { max = None } -> true
  | { min = Some (min, `Closed) ; max = Some (max, `Closed) } -> min <= max
  | { min = Some (min, _) ; max = Some (max, _) } -> min < max

let meet_ranges ra rb =
  { min =
      begin match ra.min, rb.min with
        | Some (mina, ba), Some (minb, bb) when mina = minb ->
          Some (max mina minb,
                match ba, bb with `Closed, `Closed -> `Closed | _ -> `Open)
        | Some (mina, ba), Some (minb, _) when mina > minb ->
          Some (mina, ba)
        | Some (mina, _), Some (minb, bb) ->
          Some (minb, bb)
        | Some m, None | None, Some m -> Some m
        | None, None -> None
      end ;
    max =
      begin match ra.max, rb.max with
        | Some (mina, ba), Some (minb, bb) when mina = minb ->
          Some (max mina minb,
                match ba, bb with `Closed, `Closed -> `Closed | _ -> `Open)
        | Some (mina, ba), Some (minb, _) when mina < minb ->
          Some (mina, ba)
        | Some (mina, _), Some (minb, bb) ->
          Some (minb, bb)
        | Some m, None | None, Some m -> Some m
        | None, None -> None
      end }

let (&&&) ra rb =
  assert (wellformed_range ra) ;
  assert (wellformed_range rb) ;
  let result = meet_ranges ra rb in
  assert (wellformed_range result) ;
  result

let compatible_ranges ra rb =
  assert (wellformed_range ra) ;
  assert (wellformed_range rb) ;
  let result = meet_ranges ra rb in
  wellformed_range result

(* profile builders *****************************************************)

let generic =
  { output = any ;
    interactivity = any ;
    display_width = any ;
    physical_display_width = any ;
    display_aspect_ratio = any ;
    device_width = any ;
    physical_device_width = any ;
    device_aspect_ratio = any ;
    contrast = any ;
    ink = any ;
    zoom = any ;
    connected = [] ;
    bandwidth = [] }

let profile
    ?(output = any)
    ?(interactivity = any)
    ?(display_width = any)
    ?(physical_display_width = any)
    ?(display_aspect_ratio = any)
    ?(device_width = any)
    ?(physical_device_width = any)
    ?(device_aspect_ratio = any)
    ?(contrast = any)
    ?(ink = any)
    ?(zoom = any)
    ?(connected = [])
    ?(bandwidth = [])
    () =
  { output ; interactivity ;
    display_width ; physical_display_width ; display_aspect_ratio ;
    device_width ; physical_device_width ; device_aspect_ratio ;
    contrast ; ink ; zoom ;
    connected ; bandwidth }

(* profile operations ***************************************************)

let wellformed p =
  wellformed_range p.output &&
  wellformed_range p.interactivity &&
  wellformed_range p.display_width &&
  wellformed_range p.physical_display_width &&
  wellformed_range p.display_aspect_ratio &&
  wellformed_range p.device_width &&
  wellformed_range p.physical_device_width &&
  wellformed_range p.device_aspect_ratio &&
  wellformed_range p.contrast &&
  wellformed_range p.ink &&
  wellformed_range p.zoom &&
  p.connected = [] &&
  p.bandwidth = []

let meet pa pb =
  assert (wellformed pa) ;
  assert (wellformed pb) ;
  let result =
    { output =
        meet_ranges pa.output pb.output ;
      interactivity =
        meet_ranges pa.interactivity pb.interactivity ;
      display_width =
        meet_ranges pa.display_width pb.display_width ;
      physical_display_width =
        meet_ranges pa.physical_display_width pb.physical_display_width ;
      display_aspect_ratio =
        meet_ranges pa.display_aspect_ratio pb.display_aspect_ratio ;
      device_width =
        meet_ranges pa.device_width pb.device_width ;
      physical_device_width =
        meet_ranges pa.physical_device_width pb.physical_device_width ;
      device_aspect_ratio =
        meet_ranges pa.device_aspect_ratio pb.device_aspect_ratio ;
      contrast =
        meet_ranges pa.contrast pb.contrast ;
      ink =
        meet_ranges pa.ink pb.ink ;
      zoom =
        meet_ranges pa.zoom pb.zoom ;
      connected = [] ;
      bandwidth = [] } in
  assert (wellformed result) ;
  result

let compatible pa pb =
  assert (wellformed pa) ;
  assert (wellformed pb) ;
  compatible_ranges pa.output pb.output &&
  compatible_ranges pa.interactivity pb.interactivity &&
  compatible_ranges pa.display_width pb.display_width &&
  compatible_ranges pa.physical_display_width pb.physical_display_width &&
  compatible_ranges pa.display_aspect_ratio pb.display_aspect_ratio &&
  compatible_ranges pa.device_width pb.device_width &&
  compatible_ranges pa.physical_device_width pb.physical_device_width &&
  compatible_ranges pa.device_aspect_ratio pb.device_aspect_ratio &&
  compatible_ranges pa.contrast pb.contrast &&
  compatible_ranges pa.ink pb.ink &&
  compatible_ranges pa.zoom pb.zoom
