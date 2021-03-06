(** Markdown is a module that handles formatting and writing text into
    HTML *)

(** [write_html] takes all of the text [data] within a file [path] and
    produces a .html file *)
val write_html : string -> string -> unit

(** [format_html] formats html text [data] into a more readable form *)
val format_html : string -> string

(** [to_html] formats text [data] into syntax that can be interpreted by
    a .html file, for the allowance of shortcut [symbol] by the user *)
val to_html : char -> string -> string
