(* This module implements all file I/O, including writing, reading, and
   creating files*)

(* abstract type that represents a file *)
type f

(* the type of filenames *)
type filename = string

(** [open_to_string filename] creates a strings that represents the
    contents a text file that has the name [filename]*)
val open_to_string : string -> string

(** [save_to_file filename text] takes a string [text] and saves it to
    the file specified with [filename], replacing its contents with the
    string. *)
val save_to_file : string -> string -> unit

(** [create_file filename] creates an empty file in the same directory
    as the source code with filename [filename]*)
val create_file : string -> unit

(** [create_file_type filename] creates a value of type f that
    represents the file with name [filename]*)
val create_file_type : string -> f

(** [json_to_file filename json] takes in a json object [json] and saves
    it to a new file with the name [filename] in the directory *)
val json_to_file : string -> Yojson.t -> unit
