(** This module implements functions for customization of the text
    editor, including background color, font, and text color *)

(** Modifies the background color and the text color *)
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

(** Opens the color palette and outputs changes in color for
    modification of the text area background *)
val background_color_change :
  < misc :
      < modify_base : ([> `NORMAL ] * GDraw.color) list -> 'a ; .. >
  ; .. > ->
  unit

(** Opens the color palette and outputs changes in color for
    modification of the text area text *)
val text_color_change :
  < misc :
      < modify_text : ([> `NORMAL ] * GDraw.color) list -> 'a ; .. >
  ; .. > ->
  unit
