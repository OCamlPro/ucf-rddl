(* RDDL - IOs using Yojson *)

(************************************************************************)
(*  RDDL - reactive design description language - IOs using Yojson      *)
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

open Json_encoding
open Rddl_ast

let schema_to_file ~filename =
  let json =
    Json_repr.to_yojson (Json_schema.to_json (schema ui_encoding)) in
  match open_out filename with
  | exception exn -> raise exn
  | fp ->
    match Yojson.Safe.pretty_to_channel fp json with
    | exception exn -> close_out fp ; raise exn
    | res -> close_out fp ; res

let to_file ~filename ui =
  let json =
    Json_repr.to_yojson (construct ui_encoding ui) in
  match open_out filename with
  | exception exn -> raise exn
  | fp ->
    match Yojson.Safe.pretty_to_channel fp json with
    | exception exn -> close_out fp ; raise exn
    | res -> close_out fp ; res

let from_file ~filename =
  let json = Yojson.Safe.from_channel stdin in
  destruct ui_encoding (Json_repr.from_yojson json)
