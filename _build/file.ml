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
let open_to_string name = In_channel.read_all name

let save_to_file name text = Out_channel.write_all name text

let create_file name =
  let out = Out_channel.create name in
  Out_channel.output_string out ""

let create_file_type name = { name; contents = open_to_string name }

let json_to_file name json = Yojson.to_file name json
