(** Customize is a module that implements functions for customization of
    the text editor, including background color, font, and text color *)

(** [preset_theme] modifies the background color [bg] and the text color
    [text] for a text field [textarea] *)
val preset_theme :
  < misc :
      < modify_base :
          ( [> `NORMAL ]
          * [ `BLACK
            | `COLOR of Gdk.color
            | `NAME of string
            | `RGB of int * int * int
            | `WHITE
            ] )
          list ->
          'a
      ; modify_text :
          ( [> `NORMAL ]
          * [ `BLACK
            | `COLOR of Gdk.color
            | `NAME of string
            | `RGB of int * int * int
            | `WHITE
            ] )
          list ->
          'b
      ; .. >
  ; .. > ->
  [ `BLACK
  | `COLOR of Gdk.color
  | `NAME of string
  | `RGB of int * int * int
  | `WHITE
  ] ->
  [ `BLACK
  | `COLOR of Gdk.color
  | `NAME of string
  | `RGB of int * int * int
  | `WHITE
  ] ->
  unit

(** [background_color_change] opens the color palette and outputs
    changes in color for modification of the text field [textarea]
    background *)
val background_color_change :
  < misc :
      < modify_base : ([> `NORMAL ] * GDraw.color) list -> 'a ; .. >
  ; .. > ->
  unit

(** [text_color_change] opens the color palette and outputs changes in
    color for modification of the text field [textarea] text *)
val text_color_change :
  < misc :
      < modify_text : ([> `NORMAL ] * GDraw.color) list -> 'a ; .. >
  ; .. > ->
  unit

(** [font_change] opens the font selector and outputs changes in font
    for modification of the text field [textarea] text *)
val font_change :
  < misc : < modify_font_by_name : string -> 'a ; .. > ; .. > -> unit

(** [from_json] retrieves settings previously applied before exiting the
    editor from a json file [json] *)
val from_json : Yojson.Basic.t -> string
