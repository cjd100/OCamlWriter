open GMain
open GdkKeysyms

(* Initializes Gtk *)
let init = GtkMain.Main.init ()

(* Name that shows up at the top of the GUI window*)
let win_title = "Text Editor GUI"

(* Initial width and height of the main window*)
let win_dim = (400, 400)

(* [Insert_text text field] inserts the text in the string [text] into
   the text field [field] *)
let insert_text text field = field#buffer#set_text text

let main () =
  let editor_window =
    GWindow.window ~width:(fst win_dim) ~height:(snd win_dim)
      ~title:win_title ()
  in
  (* The main container that widgets will be packed in*)
  let container = GPack.vbox ~packing:editor_window#add () in
  editor_window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let topmenu = GMenu.menu_bar ~packing:container#pack () in
  let factory = new GMenu.factory topmenu in
  (* Keeps track of accelerators (Keyboard/Menu shortcuts) *)
  let accel_group = factory#accel_group in
  (* Submenus *)
  let file_menu = factory#add_submenu "File" in
  let theme_menu = factory#add_submenu "Themes" in

  (* Scroll bar for text widget. hpolicy and vpolicy cause the scrollbar
     to only show up when needed *)
  let text_scroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:container#add ()
  in
  (* Main text widget *)
  let text_field =
    GText.view ~packing:text_scroll#add_with_viewport ()
  in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Save" ~key:_S ~callback:(fun () ->
      failwith "unimplemented");
  factory#add_item "Open file" ~key:_O ~callback:(fun () ->
      insert_text
        "Unimplemented, this string should come from a file read \
         function"
        text_field);
  factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

  (* Theme menu *)
  (* TODO: Create theme names and add a callback function that changes
     colors*)
  let factory = new GMenu.factory theme_menu ~accel_group in
  factory#add_item "Theme name 1" ~callback:(fun () ->
      failwith "unimplemented");
  factory#add_item "Theme name 2" ~callback:(fun () ->
      failwith "unimplemented");
  factory#add_item "Custom Theme" ~callback:(fun () ->
      failwith "unimplemented");

  (* Displays the main window and continues the main loop, this should
     always be the last part *)
  editor_window#add_accel_group accel_group;
  editor_window#show ();
  Main.main ()

(* Calls the main loop *)
let () = main ()
