(* RDDL - Command line checker *)

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

let () =
  try
    match Sys.argv with
    | [| _exe ; file |] ->
      let ui = Rddl_yojson.from_file file in
      Rddl_checker.wellformed ui
    | _ ->
      Printf.eprintf "Usage:\n  rddl-check <file.rddl.json>\n%!" ;
      exit 2
  with exn ->
    Format.eprintf "%a@." (fun ppf -> Rddl_checker.print_error ppf) exn ;
    exit 1
