open GMain
open GdkKeysyms
open File
open Customize
open State
open Stack
open Regex
open Markdown

let curr_file = ref ""

(* probably want to create new state for each load_file. Also integrate
   into main *)
let state = Stack.create ()

let undone = ref ""

let word_count = ref 0

let char_count = ref 0

let uniq_count = ref 0

let set_curr_val name = curr_file := name

let changed = ref false

(* create a current file type *)
type curr_file_state = {
  name : string;
  func : unit;
}

let initialize_states =
  Stack.clear state;
  ignore (state = Stack.create ());
  undone := ""

(* Initializes Gtk *)
let init = GtkMain.Main.init ()

(* Name that shows up at the top of the GUI window*)
let win_title = "Text Editor GUI"

(* Initial width and height of the main window*)
let win_dim = (400, 400)

let message_window title message =
  let message_win = GWindow.window ~title ~show:true () in
  let container =
    GPack.vbox ~packing:message_win#add ~border_width:10 ()
  in
  ignore (message_win#connect#destroy ~callback:message_win#destroy);
  let _ = GMisc.label ~text:message ~packing:container#pack () in
  let close_button =
    GButton.button ~stock:`CLOSE ~packing:container#add ()
  in
  ignore
    (close_button#connect#clicked ~callback:(fun () ->
         message_win#destroy ()))

(** [message_window_from_file f] reads from file [f] and creates a new
    window, where file [f] has the title on the first line, and the
    message on subsequent lines. Raise: Failure "Invalid message file"
    if the message file inputted by the user is invalid. *)
let message_window_from_file f =
  let contents = File.open_to_string f in
  let content_split = String.split_on_char '\n' contents in
  match content_split with
  | h :: t ->
      let merged_tail =
        List.fold_left (fun s acc -> acc ^ "\n" ^ s) "" (List.rev t)
      in
      message_window h merged_tail
  | [] -> failwith "Invalid message file"

(** [message_window_from_file_rep f rep] is the same as
    [message_window_from_file f], except it also accepts a list of
    strings [rep] so that the nth element of [rep] replaces the string
    "(n)" in the message content. *)
let rec message_window_from_file_rep f rep =
  let contents = File.open_to_string f in
  let content_split = String.split_on_char '\n' contents in
  match content_split with
  | h :: t ->
      let merged_tail =
        List.fold_left (fun s acc -> acc ^ "\n" ^ s) "" (List.rev t)
      in
      message_window h (list_replace 0 rep merged_tail)
  | [] -> failwith "Invalid message file"

and list_replace ind r str =
  match r with
  | [] -> str
  | h :: t ->
      let new_str =
        Regex.replace_exact ("(" ^ string_of_int ind ^ ")") h str
      in
      list_replace (ind + 1) t new_str

(** [Insert_text text field] inserts the text in the string [text] into
    the text field [field] *)
let insert_text text field = field#buffer#set_text text

(** [highlight_test f a b] highlights the text in text field [f] from
    index [a] to index [b]*)
let highlight_text (field : GText.view) (a : int) (b : int) =
  field#buffer#select_range
    (field#buffer#get_iter_at_char b)
    (field#buffer#get_iter_at_char a)

(** [set_cursor_pos field p] sets the position of the cursor in the text
    field [field] to [p]*)
let set_cursor_pos (field : GText.view) p =
  field#buffer#place_cursor (field#buffer#get_iter_at_char p)

(** [insert_label_text text label] updatest the text contained in
    [label] to [text]*)
let insert_label_text text label = label#set_text text

let update_insert_counts text label =
  word_count := Words.word_count text;
  char_count := Words.char_count text;
  uniq_count := Words.uniq_count text;
  label#set_text
    ( "Words: "
    ^ string_of_int !word_count
    ^ " Characters: "
    ^ string_of_int !char_count
    ^ " Unique Words: "
    ^ string_of_int !uniq_count )

(* [new_file parent text_area] opens up a new window from the parent
   window [parent]. The new window is a file creation widget that
   creates an empty new file *)
let new_file parent text_area =
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
  open_file_window#destroy ()

(** [load_file parent] opens up a new window, with the parent window
    [parent]. The new window is a file selection GUI that loads the
    selected file into the document. *)
let rec load_file parent file_label word_label text_area =
  (* window that appears when the request file command is issued*)
  let open_file_window =
    GWindow.file_chooser_dialog ~action:`OPEN ~width:300 ~height:300
      ~title:"Open file" ~parent ()
  in
  open_file_window#add_button_stock `CANCEL `CANCEL;
  open_file_window#add_button_stock `NEW `NEW;
  open_file_window#add_select_button_stock `OPEN `OPEN;
  run_win open_file_window file_label word_label text_area;
  open_file_window#destroy ()

(** [run_win file_dia f_label w_label t_area] calls the run command on
    the file chooser dialogue [file_dia] and pattern matches the output,
    either opening a file, creating a new file, or exiting the window. *)
and run_win file_dia f_label w_label t_area =
  let get_filename = function Some f -> f | None -> "" in
  match file_dia#run () with
  | `OPEN ->
      let filename = get_filename file_dia#filename in
      (* get a string from the file and then out it in the text field *)
      print_endline ("OPEN The file you selected was: " ^ filename);
      curr_file := filename;
      insert_label_text filename f_label;
      update_insert_counts (File.open_to_string filename) w_label;
      initialize_states;
      (* Push initial text onto save stack *)
      Stack.push (File.open_to_string filename) state;
      ignore (insert_text (File.open_to_string filename) t_area)
  | `NEW -> ignore (new_file file_dia t_area)
  | `DELETE_EVENT | `CANCEL -> ()

(** [encrypt_file pass text_area text parent] encrypts the [text] using
    password [pass], closing the window [parent] at the end *)
let encrypt_file pass text_area text parent =
  if String.length pass < 6 then
    message_window_from_file
      "data/message dialogues/password_length_error.txt"
  else insert_text (Cipher.encrypt pass text) text_area;
  parent#destroy ()

(** [decrypt_file pass text_area text parent] encrypts [text] using
    password [pass], then puts closing the window [parent] at the end *)
let decrypt_file pass text_area text parent =
  try
    insert_text (Cipher.decrypt pass text) text_area;
    parent#destroy ()
  with _ ->
    message_window_from_file
      "data/message dialogues/password_incorrect.txt"

let cipher_window text_area text (encrypt : bool) =
  let title =
    (if encrypt then "En" else "De") ^ "cryption: Input password"
  in
  let password_input =
    GWindow.window ~width:400 ~height:200 ~title ()
  in
  let container = GPack.vbox ~packing:password_input#add () in
  ignore
    (password_input#connect#destroy ~callback:password_input#destroy);
  let text_entry =
    GEdit.entry ~packing:container#add ~width:350 ~height:100 ()
  in
  let confirm_button =
    GButton.button ~stock:`APPLY ~packing:container#add ()
  in
  ignore
    (confirm_button#connect#clicked ~callback:(fun () ->
         (if encrypt then encrypt_file else decrypt_file)
           text_entry#text text_area text password_input));
  password_input#show

let matched_length text reg exact =
  let replace_first =
    if exact then Regex.replace_exact_first else Regex.replace_reg_first
  in
  String.length text - String.length (replace_first reg "" text)

(** [regex_find field reg exact] Highlights the first instance of [reg]
    in the text box [field] past the cursor location. If [exact] is
    true, it will treat [reg] as a normal string, otherwise [reg] is
    treated as a regular expression *)
let regex_find (text_area : GText.view) (reg : string) (exact : bool) =
  let find = if exact then Regex.find_exact else Regex.find_reg in

  let text = text_area#buffer#get_text () in
  let cursor_pos = text_area#buffer#cursor_position in
  let first_ind = find reg text cursor_pos in

  let matched_len = matched_length text reg exact in
  let end_pos = first_ind + matched_len in
  if first_ind = -1 then (
    set_cursor_pos text_area (String.length text);
    message_window_from_file_rep
      "data/message dialogues/regex_not_found.txt" [ reg ] )
  else highlight_text text_area first_ind end_pos

let regex_find_window (text_area : GText.view) =
  let title = "Find" in
  let reg_window = GWindow.window ~width:400 ~height:200 ~title () in
  let container = GPack.vbox ~packing:reg_window#add () in
  ignore (reg_window#connect#destroy ~callback:reg_window#destroy);
  let _ =
    GMisc.label ~text:"Find: " ~packing:container#pack ~height:20 ()
  in
  let text_entry =
    GEdit.entry ~packing:container#add ~width:350 ~height:50 ()
  in

  let mode =
    GButton.check_button ~label:"Search by regex" ~packing:container#add
      ~active:false ()
  in
  let confirm_button =
    GButton.button ~stock:`FIND ~packing:container#add ()
  in
  ignore
    (confirm_button#connect#clicked ~callback:(fun () ->
         regex_find text_area text_entry#text (not mode#active)));
  reg_window#show

(** [regex_replace area reg new_s exact all] is the same string
    contained by the text area [area], except [reg] is replaced with the
    string [new_s]. If [exact] is true, [reg] is treated like a normal
    string, otherwise [reg] is treated like a regular expression. If
    [all] is true, all instances of [reg] will be replaced, otherwise
    only the next instance of [reg] is replaced. *)
let rec regex_replace (text_area : GText.view) reg new_s exact all =
  let replace = rep_fun exact all in
  let cursor_pos = text_area#buffer#cursor_position in
  let t = text_area#buffer#get_text () in
  let text =
    if all then t
    else String.sub t cursor_pos (String.length t - cursor_pos)
  in
  let matched_len = matched_length text reg exact in
  let text_head = if all then "" else String.sub t 0 cursor_pos in
  insert_text (text_head ^ replace reg new_s text) text_area;
  set_cursor_pos text_area (cursor_pos + matched_len)

(** [rep_fun e a] is the proper replace function to use for the
    [regex_replace] function*)
and rep_fun e a =
  match (e, a) with
  | true, true -> Regex.replace_exact
  | true, false -> Regex.replace_exact_first
  | false, true -> Regex.replace_reg
  | false, false -> Regex.replace_reg_first

let regex_replace_window (text_area : GText.view) =
  let title = "Replace" in
  let reg_window = GWindow.window ~width:400 ~height:300 ~title () in
  let container = GPack.vbox ~packing:reg_window#add () in
  ignore (reg_window#connect#destroy ~callback:reg_window#destroy);
  let _ =
    GMisc.label ~text:"Replace: " ~packing:container#pack ~height:20 ()
  in
  let regex_entry =
    GEdit.entry ~packing:container#add ~width:350 ~height:50 ()
  in
  let _ =
    GMisc.label ~text:"With: " ~packing:container#pack ~height:20 ()
  in
  let with_entry =
    GEdit.entry ~packing:container#add ~width:350 ~height:50 ()
  in

  let reg_mode =
    GButton.check_button ~label:"Replace regex" ~packing:container#add
      ~active:false ()
  in
  let all_mode =
    GButton.check_button ~label:"Replace all" ~packing:container#add
      ~active:false ()
  in
  let confirm_button =
    GButton.button ~stock:`FIND_AND_REPLACE ~packing:container#add ()
  in
  ignore
    (confirm_button#connect#clicked ~callback:(fun () ->
         regex_replace text_area regex_entry#text with_entry#text
           (not reg_mode#active) all_mode#active));
  reg_window#show

let save word_label name text_area text =
  word_count := Words.word_count text;
  char_count := Words.char_count text;
  uniq_count := Words.uniq_count text;
  update_insert_counts text word_label;
  File.save_to_file name text;
  if text != Stack.top state then Stack.push text state else ()

(** [undo parent text_area curr_text] reverts the text in [text_area] in
    the GUI to the prior save state. Can be done successively, reverting
    file contents to what they were opened as *)
let undo parent text_area curr_text =
  undone := curr_text;
  if Stack.length state = 1 then
    let text = Stack.top state in
    insert_text text text_area
  else ignore (Stack.pop_opt state);
  if Stack.length state = 1 then
    let text = Stack.top state in
    insert_text text text_area
  else
    match Stack.pop_opt state with
    | None -> ()
    | Some text -> insert_text text text_area

(** [redo parent text_area curr_text] reverts the text in [text_area]
    such that it "redoes" the undo action taken previously*)
let redo parent text_area curr_text =
  Stack.push curr_text state;
  if !undone = "" then () else insert_text !undone text_area

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
  let edit_menu = factory#add_submenu "Edit" in
  let theme_menu = factory#add_submenu "Themes" in
  let encryption_menu = factory#add_submenu "Encryption" in
  let html_menu = factory#add_submenu "HTML" in
  let help_menu = factory#add_submenu "Help" in

  (* word count label *)

  (* Scroll bar for text widget. hpolicy and vpolicy cause the scrollbar
     to only show up when needed *)
  let text_scroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:container#add ~border_width:5 ()
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
         save word_label !curr_file text_field
           (text_field#buffer#get_text ())));
  ignore
    (factory#add_item "Open file" ~key:_O ~callback:(fun () ->
         load_file editor_window file_label word_label text_field));
  ignore (factory#add_item "Quit" ~key:_Q ~callback:Main.quit);

  (* Edit menu *)
  let factory = new GMenu.factory edit_menu ~accel_group in
  ignore
    (factory#add_item "Undo" ~key:_Z ~callback:(fun () ->
         undo editor_window text_field (text_field#buffer#get_text ())));
  ignore
    (factory#add_item "Redo" ~key:_Y ~callback:(fun () ->
         redo editor_window text_field (text_field#buffer#get_text ())));
  ignore
    (factory#add_item "Find" ~key:_F ~callback:(fun () ->
         regex_find_window text_field ()));
  ignore
    (factory#add_item "Replace" ~key:_R ~callback:(fun () ->
         regex_replace_window text_field ()));

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

  (* Encryption menu *)
  let factory = new GMenu.factory encryption_menu ~accel_group in
  ignore
    (factory#add_item "Encrypt" ~callback:(fun () ->
         cipher_window text_field
           (text_field#buffer#get_text ())
           true ()));
  ignore
    (factory#add_item "Decrypt" ~callback:(fun () ->
         cipher_window text_field
           (text_field#buffer#get_text ())
           false ()));
  print_endline (text_field#buffer#get_text ());

  (* HTML menu *)
  let factory = new GMenu.factory html_menu ~accel_group in
  ignore
    (factory#add_item "Create HTML" ~key:_H ~callback:(fun () ->
         Markdown.write_html !curr_file
           ( text_field#buffer#get_text ()
           |> Markdown.to_html '_' |> Markdown.to_html '*'
           |> Markdown.to_html '\n' )));
  ignore
    (factory#add_item "Format HTML" ~callback:(fun () ->
         insert_text
           (Markdown.format_html
              ( text_field#buffer#get_text ()
              |> Markdown.to_html '_' |> Markdown.to_html '*'
              |> Markdown.to_html '\n' ))
           text_field));

  let factory = new GMenu.factory help_menu ~accel_group in
  ignore
    (factory#add_item "Encryption Help" ~callback:(fun () ->
         message_window_from_file "data/help/encryption.txt"));
  ignore
    (factory#add_item "Regex Help" ~callback:(fun () ->
         message_window_from_file "data/help/regex.txt"));

  (* Displays the main window and continues the main loop, this should
     always be the last part *)
  editor_window#add_accel_group accel_group;

  editor_window#show ();
  Main.main ()

(* Calls the main loop *)
let () = main ()
