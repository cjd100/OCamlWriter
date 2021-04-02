(* This module implements all file I/O, including writing, reading, and
   creating files*)

(* abstract type that represents a file *)
type f

(* the type of filenames *)
type filename = string

(** [open_to_string filename] creates a list of strings that represents
    a text file that has the name [filename]*)
val open_to_string : string -> string

val save_to_file : string -> string -> unit

val create_file : string -> buf:bytes -> pos:int -> len:int -> unit

(** [create_file_type filename] creates a value of type f that
    represents the file with name [filename] and text [contents]*)
val create_file_type : string -> f
