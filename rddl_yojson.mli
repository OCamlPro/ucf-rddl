(** RDDL - IOs using Yojson *)

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

(** Write the main JSON schema for RDDL in a file *)
val schema_to_file: filename: string -> unit

(** Write the concrete JSON of an RDDL ui to a file *)
val to_file: filename: string -> Rddl_ast.ui -> unit

(** Read the concrete JSON of an RDDL ui from a file *)
val from_file: filename: string -> Rddl_ast.ui
