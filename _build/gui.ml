open GMain
open GdkKeysyms
open Core

(* Initializes Gtk *)
let init = GtkMain.Main.init ()

(* Name that shows up at the top of the GUI window*)
let win_title = "Text Editor GUI"

(* Initial width and height of the main window*)
let win_dim = (400, 400)

(* [Insert_text text field] inserts the text in the string [text] into
   the text field [field] *)
let insert_text text field = field#buffer#set_text text

let string_of_string_list lst sep =
  let rec string_of_string_list_helper lt s =
    match lt with
    | [] -> []
    | [ h ] -> [ h ]
    | h :: t -> (h ^ s) :: string_of_string_list_helper t s
  in
  let separated = string_of_string_list_helper lst sep in
  Stdlib.List.fold_left ( ^ ) "" separated

(* let channel for reading file to strings *)
let open_to_string name =
  string_of_string_list (In_channel.read_lines name) " "

(** [load_file parent] opens up a new window, with the parent window
    [parent]. The new window is a file selection GUI that loads the
    selected file into the document. *)
let load_file parent text_field =
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
        (* get a string from the file and then out it in the text field *)
        insert_text (File.open_to_string filename) text_field;
        print_endline ("The file you selected was: " ^ filename)
    | `DELETE_EVENT | `CANCEL -> ()
  end;
  open_file_window#destroy ()

(** A reference to the mutable palette dialogue *)
let bg_dialog_ref = ref None

(** The color of the background broken up into RGB values *)
let bg_color = ref (`RGB (0, 0, 0))

(** Matches the feedback from the [dialogue] palette, and modifies the
    background of [textarea] accordingly *)
let background_response dialogue textarea resp =
  let colorsel = dialogue#colorsel in
  begin
    match resp with
    | `OK -> bg_color := `COLOR colorsel#color
    | _ -> ()
  end;
  textarea#misc#modify_base [ (`NORMAL, !bg_color) ];
  dialogue#misc#hide ()

(* Opens the color palette and outputs changes in color for modification
   of the [textarea] background *)
let background_color_change textarea =
  let colordlg =
    match !bg_dialog_ref with
    | None ->
        let dlg =
          GWindow.color_selection_dialog
            ~title:"Select Background Color" ()
        in
        bg_dialog_ref := Some dlg;
        dlg
    | Some dlg -> dlg
  in

  let colorsel = colordlg#colorsel in

  colorsel#set_color (GDraw.color !bg_color);
  colorsel#set_has_palette true;

  colordlg#connect#response
    ~callback:(background_response colordlg textarea);

  colordlg#run ();
  ()

(** A reference to the mutable palette dialogue *)
let text_dialog_ref = ref None

(** The color of the background broken up into RGB values *)
let text_color = ref (`RGB (0, 0, 0))

(** Matches the feedback from the [dialogue] palette, and modifies the
    text of [textarea] accordingly *)
let text_response dialogue textarea resp =
  let colorsel = dialogue#colorsel in
  begin
    match resp with
    | `OK -> text_color := `COLOR colorsel#color
    | _ -> ()
  end;
  textarea#misc#modify_text [ (`NORMAL, !text_color) ];
  dialogue#misc#hide ()

(* Opens the color palette and outputs changes in color for modification
   of the [textarea] text *)
let text_color_change textarea =
  let colordlg =
    match !text_dialog_ref with
    | None ->
        let dlg =
          GWindow.color_selection_dialog ~title:"Select Text Color" ()
        in
        text_dialog_ref := Some dlg;
        dlg
    | Some dlg -> dlg
  in

  let colorsel = colordlg#colorsel in

  colorsel#set_color (GDraw.color !text_color);
  colorsel#set_has_palette true;

  colordlg#connect#response ~callback:(text_response colordlg textarea);

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
  let preset_theme bg text =
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
      load_file editor_window text_field);
  factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

  (* Theme menu *)
  let factory = new GMenu.factory theme_menu ~accel_group in
  factory#add_item "Dark Mode" ~callback:(fun () ->
      preset_theme `BLACK `WHITE);
  factory#add_item "Light Mode" ~callback:(fun () ->
      preset_theme `WHITE `BLACK);
  factory#add_item "Custom Background" ~callback:(fun () ->
      background_color_change text_field);
  factory#add_item "Custom Text Color" ~callback:(fun () ->
      text_color_change text_field);

  (* Displays the main window and continues the main loop, this should
     always be the last part *)
  editor_window#add_accel_group accel_group;
  editor_window#show ();
  Main.main ()

(* Calls the main loop *)
let () = main ()
