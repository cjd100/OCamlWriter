open Printf
open Core

type filename = string

(* the abstract type that represents a file *)
type f = {
  name : filename;
  contents : string list;
}

(* let channel for reading file to strings *)
let open_to_string name = In_channel.read_lines name

let create_file_type name = { name; contents = open_to_string name }
