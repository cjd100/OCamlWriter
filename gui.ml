open GMain
open GdkKeysyms
open File
open Customize
open State
open Stack

let curr_file = ref ""

(* probably want to create new state for each load_file. Also integrate
   into main *)
let state = Stack.create ()

let word_count = ref 0

let set_curr_val name = curr_file := name

(* create a current file type *)
type curr_file_state = {
  name : string;
  func : unit;
}

(* Initializes Gtk *)
let init = GtkMain.Main.init ()

(* Name that shows up at the top of the GUI window*)
let win_title = "Text Editor GUI"

(* Initial width and height of the main window*)
let win_dim = (400, 400)

(* [Insert_text text field] inserts the text in the string [text] into
   the text field [field] *)
let insert_text text field = field#buffer#set_text text

let insert_label_text text label = label#set_text text

let insert_count count label =
  label#set_text ("Words: " ^ string_of_int count)

(* [new_file parent text_area] opens up a new window from the parent
   window [parent]. The new window is a file creation widget that
   creates an empty new file *)
let new_file parent text_area =
  (* window that appears when the request file command is issued*)
  let open_file_window =
    GWindow.dialog ~width:200 ~height:100 ~title:"Create new file"
      ~parent ()
  in

  let entry =
    GEdit.entry ~text:"Enter Filename"
      ~packing:open_file_window#vbox#add ()
  in
  open_file_window#add_button_stock `OK `OK;
  begin
    match open_file_window#run () with
    | `OK ->
        curr_file.contents <- entry#text;
        let text = entry#text in
        curr_file := text;
        ignore (File.create_file text)
    | `DELETE_EVENT -> ()
  end;
  open_file_window#destroy

(** [load_file parent] opens up a new window, with the parent window
    [parent]. The new window is a file selection GUI that loads the
    selected file into the document. *)
let load_file parent file_label word_label text_area =
  let get_filename = function Some f -> f | None -> "" in
  (* window that appears when the request file command is issued*)
  let open_file_window =
    GWindow.file_chooser_dialog ~action:`OPEN ~width:300 ~height:300
      ~title:"Open file" ~parent ()
  in
  open_file_window#add_button_stock `CANCEL `CANCEL;
  open_file_window#add_button_stock `NEW `NEW;
  open_file_window#add_select_button_stock `OPEN `OPEN;
  begin
    match open_file_window#run () with
    | `OPEN ->
        let filename = get_filename open_file_window#filename in

        (* TODO: Replace the print statement below with something that
           loads the file [filename] into the document*)
        (* get a string from the file and then out it in the text field *)
        print_endline ("OPEN The file you selected was: " ^ filename);
        curr_file := filename;
        word_count := Words.word_count (File.open_to_string filename);
        insert_label_text filename file_label;
        insert_count !word_count word_label;
        ignore (insert_text (File.open_to_string filename) text_area)
    | `NEW -> ignore (new_file open_file_window text_area)
    (*why is it partial?*)
    (* TODO: MAKE IT WORK SOMEHOW *)
    | `DELETE_EVENT | `CANCEL -> ()
  end;
  open_file_window#destroy ()

let save word_label name text =
  Stack.push text state;
  word_count := Words.word_count text;
  insert_count !word_count word_label;
  File.save_to_file name text

let undo parent text_area =
  match Stack.pop_opt state with
  | None -> ()
  | Some text -> insert_text text text_area

let rgbtuple_of_string str =
  let values = String.split_on_char ' ' str in
  match values with
  | h :: i :: j :: t ->
      ( `RGB
          ( int_of_string (String.trim h),
            int_of_string (String.trim i),
            int_of_string (String.trim j) )
        : GDraw.color )
  | _ -> `WHITE

let load_settings textarea =
  let previous_settings =
    Yojson.Basic.from_file "current_state.json"
    |> Customize.from_json
    |> String.split_on_char '%'
  in
  match previous_settings with
  | h :: i :: j :: t ->
      let background = rgbtuple_of_string h in
      let text = rgbtuple_of_string i in
      let font = j in
      textarea#misc#modify_base [ (`NORMAL, background) ];
      textarea#misc#modify_text [ (`NORMAL, text) ];
      textarea#misc#modify_font_by_name font
  | _ -> ()

let main () =
  let editor_window =
    GWindow.window ~width:(fst win_dim) ~height:(snd win_dim)
      ~title:win_title ()
  in
  (* The main container that widgets will be packed in*)
  let container = GPack.vbox ~packing:editor_window#add () in
  ignore (editor_window#connect#destroy ~callback:Main.quit);

  (* Menu bar *)
  let topmenu = GMenu.menu_bar ~packing:container#pack () in
  let factory = new GMenu.factory topmenu in
  (* Keeps track of accelerators (Keyboard/Menu shortcuts) *)
  let accel_group = factory#accel_group in
  (* Submenus *)
  let file_menu = factory#add_submenu "File" in
  let theme_menu = factory#add_submenu "Themes" in

  (* word count label *)

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

  (* opens the file chooser GUI at the start and makes you choose a file
     to use *)
  let file_label =
    GMisc.label ~text:!curr_file ~packing:container#pack ~height:20 ()
  in

  let word_label =
    GMisc.label
      ~text:("Words: " ^ string_of_int !word_count)
      ~packing:container#pack ~height:20 ()
  in
  load_file editor_window file_label word_label text_field;

  (* Loads previous settings *)
  load_settings text_field;

  (* consider using a meny item and then updating the label text to be
     the word count *)

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore
    (factory#add_item "New file" ~key:_N ~callback:(fun () ->
         load_file editor_window file_label word_label text_field));
  ignore
    (factory#add_item "Save" ~key:_S ~callback:(fun () ->
         save word_label !curr_file (text_field#buffer#get_text ())));
  ignore
    (factory#add_item "Open file" ~key:_O ~callback:(fun () ->
         load_file editor_window file_label word_label text_field));
  ignore
    (factory#add_item "Undo" ~key:_Z ~callback:(fun () ->
         undo editor_window text_field));
  ignore (factory#add_item "Quit" ~key:_Q ~callback:Main.quit);

  (* Theme menu *)
  let factory = new GMenu.factory theme_menu ~accel_group in
  ignore
    (factory#add_item "Dark Mode" ~callback:(fun () ->
         Customize.preset_theme text_field `BLACK `WHITE));
  ignore
    (factory#add_item "Light Mode" ~callback:(fun () ->
         Customize.preset_theme text_field `WHITE `BLACK));
  ignore
    (factory#add_item "Custom Background" ~callback:(fun () ->
         Customize.background_color_change text_field));
  ignore
    (factory#add_item "Custom Text Color" ~callback:(fun () ->
         Customize.text_color_change text_field));
  ignore
    (factory#add_item "Custom Font" ~callback:(fun () ->
         Customize.font_change text_field));

  (* Displays the main window and continues the main loop, this should
     always be the last part *)
  editor_window#add_accel_group accel_group;

  editor_window#show ();
  Main.main ()

(* Calls the main loop *)
let () = main ()
