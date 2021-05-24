(** Markdown is a module that handles formatting and writing text into
    HTML *)

(** Takes all of the text within a file and produces a .html file *)
val write_html : string -> string -> unit

(** Formats html text into a more readable form *)
val format_html : string -> string
