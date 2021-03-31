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

(** [load_file parent] opens up a new window, with the parent window
    [parent]. The new window is a file selection GUI that loads the
    selected file into the document. *)
let load_file parent =
  let get_filename = function Some f -> f | None -> "" in
  (* window that appears when the request file command is issued*)
  let open_file_window =
    GWindow.file_chooser_dialog ~action:`OPEN ~width:300 ~height:300
      ~title:"Open file" ~parent ()
  in
  open_file_window#add_button_stock `CANCEL `CANCEL;
  open_file_window#add_select_button_stock `OPEN `OPEN;
  begin
    match open_file_window#run () with
    | `OPEN ->
        let filename = get_filename open_file_window#filename in
        (* TODO: Replace the print statement below with something that
           loads the file [filename] into the document*)
        print_endline ("The file you selected was: " ^ filename)
    | `DELETE_EVENT | `CANCEL -> ()
  end;
  open_file_window#destroy ()

let dialog_ref = ref None

let color = ref (`RGB (0, 65535, 0)) (* GDraw.color ref type *)

let response dlg textarea resp =
  let colorsel = dlg#colorsel in
  begin
    match resp with
    | `OK -> color := `COLOR colorsel#color
    | _ -> ()
  end;
  textarea#misc#modify_base [ (`NORMAL, !color) ];
  dlg#misc#hide ()

(* Drawingarea button_press event handler *)
let button_pressed textarea =
  (* Create color selection dialog *)
  let colordlg =
    match !dialog_ref with
    | None ->
        let dlg =
          GWindow.color_selection_dialog
            ~title:"Select background color" ()
        in
        dialog_ref := Some dlg;
        dlg
    | Some dlg -> dlg
  in

  (* Get the ColorSelection widget *)
  let colorsel = colordlg#colorsel in

  colorsel#set_color (GDraw.color !color);
  (* requires Gdk.color type *)
  colorsel#set_has_palette true;

  colordlg#connect#response ~callback:(response colordlg textarea);

  (* Show the dialog *)
  colordlg#run ();
  ()

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

  (* Modifies the background color to [bg] and the text color to [text] *)
  let theme bg text =
    text_field#misc#modify_base [ (`NORMAL, bg) ];
    text_field#misc#modify_text [ (`NORMAL, text) ]
  in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "New file" ~key:_N ~callback:(fun () ->
      failwith "unimplemented");
  factory#add_item "Save" ~key:_S ~callback:(fun () ->
      failwith "unimplemented");
  factory#add_item "Open file" ~key:_O ~callback:(fun () ->
      load_file editor_window);
  factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

  (* Theme menu *)
  (* TODO: Create theme names and add a callback function that changes
     colors*)
  let factory = new GMenu.factory theme_menu ~accel_group in
  factory#add_item "Dark Mode" ~callback:(fun () -> theme `BLACK `WHITE);
  factory#add_item "Light Mode" ~callback:(fun () ->
      theme `WHITE `BLACK);
  factory#add_item "Custom Theme" ~callback:(fun () ->
      button_pressed text_field);

  (* Displays the main window and continues the main loop, this should
     always be the last part *)
  editor_window#add_accel_group accel_group;
  editor_window#show ();
  Main.main ()

(* Calls the main loop *)
let () = main ()
