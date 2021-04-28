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
