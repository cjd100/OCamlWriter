open Printf
open Core
open Yojson
open String

type filename = string

(* the abstract type that represents a file *)
type f = {
  name : filename;
  contents : string;
}

(* combine string list into string *)
let string_of_string_list lst sep =
  let rec string_of_string_list_helper lt s =
    match lt with
    | [] -> []
    | [ h ] -> [ h ]
    | h :: t -> (h ^ s) :: string_of_string_list_helper t s
  in
  let separated = string_of_string_list_helper lst sep in
  Stdlib.List.fold_left ( ^ ) "" separated

(* let channel for reading file to strings *)
let open_to_string name =
  string_of_string_list (In_channel.read_lines name) " "

let create_file_type name = { name; contents = open_to_string name }

let json_to_file name json = Yojson.to_file name json
