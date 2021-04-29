let update_json bg t fn fs =
  let t =
    {|{"background color" : "bg", "text color": "t", "font" : {"name" : "fn", "size" : "fs"}}|}
  in
  Yojson.Basic.to_file "current_state.json" (Yojson.Basic.from_string t)

let preset_theme textarea bg text =
  textarea#misc#modify_base
    [
      ( `NORMAL,
        (bg
          : [ `BLACK
            | `COLOR of Gdk.color
            | `NAME of string
            | `RGB of int * int * int
            | `WHITE
            ]) );
    ];
  textarea#misc#modify_text
    [
      ( `NORMAL,
        (text
          : [ `BLACK
            | `COLOR of Gdk.color
            | `NAME of string
            | `RGB of int * int * int
            | `WHITE
            ]) );
    ];
  ()

(* A reference to the mutable palette dialogue *)
let bg_dialog_ref = ref None

(* The color of the background broken up into RGB values *)
let bg_color = ref (`RGB (0, 0, 0))

(* Matches the feedback from the [dialogue] palette, and modifies the
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

  ignore
    (colordlg#connect#response
       ~callback:(background_response colordlg textarea));

  ignore (colordlg#run ());
  ()

(* A reference to the mutable palette dialogue *)
let text_dialog_ref = ref None

(* The color of the background broken up into RGB values *)
let text_color = ref (`RGB (0, 0, 0))

(* Matches the feedback from the [dialogue] palette, and modifies the
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

  ignore
    (colordlg#connect#response
       ~callback:(text_response colordlg textarea));

  ignore (colordlg#run ());
  ()

(* A reference to the mutable font dialogue *)
let font_dialog_ref = ref None

(* The font name of the text *)
let font_name = ref ""

(* Matches the feedback from the [dialogue] selection, and modifies the
   text of [textarea] accordingly *)
let font_response dialogue textarea resp =
  let fontsel = dialogue#selection in
  begin
    match resp with
    | `OK -> font_name := fontsel#font_name
    | _ -> ()
  end;
  textarea#misc#modify_font_by_name !font_name;
  update_json "test" "test" !font_name "test";
  dialogue#misc#hide ()

let font_change textarea =
  let fontdlg =
    match !font_dialog_ref with
    | None ->
        let dlg =
          GWindow.font_selection_dialog ~title:"Select Font Type" ()
        in
        font_dialog_ref := Some dlg;
        dlg
    | Some dlg -> dlg
  in

  let fontsel = fontdlg#selection in

  fontsel#set_font_name !font_name;

  ignore
    (fontdlg#connect#response
       ~callback:(font_response fontdlg textarea));

  ignore (fontdlg#run ());
  ()
