open Yojson.Basic.Util

let update_json bg t f =
  let str =
    {|{"background color" : |} ^ bg ^ {|, "text color": |} ^ t
    ^ {|, "font" : |} ^ f ^ "}"
  in
  Yojson.Basic.to_file "current_state.json"
    (Yojson.Basic.from_string str)

let string_of_color = function
  | (`COLOR c : GDraw.color) ->
      string_of_int (Gdk.Color.red c)
      ^ {| |}
      ^ string_of_int (Gdk.Color.green c)
      ^ {| |}
      ^ string_of_int (Gdk.Color.blue c)
  | `RGB (r, g, b) ->
      string_of_int r ^ {| |} ^ string_of_int g ^ {| |}
      ^ string_of_int b
  | `BLACK -> "0 0 0"
  | `WHITE -> "65535 65535 65535"
  | `NAME s -> s

(* A reference to the mutable palette dialogue *)
let bg_dialog_ref = ref None

(* The color of the background broken up into RGB values *)
let bg_color = ref (`RGB (0, 0, 0))

(* A reference to the mutable palette dialogue *)
let text_dialog_ref = ref None

(* The color of the background broken up into RGB values *)
let text_color = ref (`RGB (0, 0, 0))

(* A reference to the mutable font dialogue *)
let font_dialog_ref = ref None

(* The font name of the text *)
let font_name = ref ""

let preset_theme textarea bg text =
  textarea#misc#modify_base
    [
      ( `NORMAL,
        ( bg
          : [ `BLACK
            | `COLOR of Gdk.color
            | `NAME of string
            | `RGB of int * int * int
            | `WHITE
            ] ) );
    ];
  textarea#misc#modify_text
    [
      ( `NORMAL,
        ( text
          : [ `BLACK
            | `COLOR of Gdk.color
            | `NAME of string
            | `RGB of int * int * int
            | `WHITE
            ] ) );
    ];
  update_json
    ({|"|} ^ string_of_color bg ^ {|"|})
    ({|"|} ^ string_of_color text ^ {|"|})
    ({|"|} ^ !font_name ^ {|"|});
  ()

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
  update_json
    ({|"|} ^ string_of_color !bg_color ^ {|"|})
    ({|"|} ^ string_of_color !text_color ^ {|"|})
    ({|"|} ^ !font_name ^ {|"|});
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
  update_json
    ({|"|} ^ string_of_color !bg_color ^ {|"|})
    ({|"|} ^ string_of_color !text_color ^ {|"|})
    ({|"|} ^ !font_name ^ {|"|});
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
  update_json
    ({|"|} ^ string_of_color !bg_color ^ {|"|})
    ({|"|} ^ string_of_color !text_color ^ {|"|})
    ({|"|} ^ !font_name ^ {|"|});
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

let from_json json =
  let background = json |> member "background color" |> to_string in
  let text = json |> member "text color" |> to_string in
  let font = json |> member "font" |> to_string in
  background ^ "%" ^ text ^ "%" ^ font
