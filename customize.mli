(* This module implements functions for customization of the text
   editor, including background color, font, and text color *)

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
