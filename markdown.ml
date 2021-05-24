open Soup

let write_html path data =
  let modded_path =
    match String.split_on_char '.' path with
    | [] -> failwith "Violates precondition"
    | h :: t -> h
  in
  Soup.write_file (modded_path ^ ".html") data

let format_html data = data |> parse |> pretty_print

let rec to_html_helper symbol datalist =
  match (symbol, datalist) with
  | '*', [] -> ""
  | '*', [ data ] -> data
  | '*', [ ""; data ] -> "*" ^ data
  | '*', [ data; "" ] -> data ^ "*"
  | '*', [ ""; data; "" ] -> "<b>" ^ data ^ "</b>"
  | '*', [ text; text2 ] -> text ^ "*" ^ text2
  | '*', h1 :: h2 :: t ->
      h1 ^ "<b>" ^ h2 ^ "</b>" ^ to_html_helper symbol t
  | '_', [] -> ""
  | '_', [ data ] -> data
  | '_', [ ""; data ] -> "_" ^ data
  | '_', [ data; "" ] -> data ^ "_"
  | '_', [ ""; data; "" ] -> "<i>" ^ data ^ "</i>"
  | '_', [ text; text2 ] -> text ^ "_" ^ text2
  | '_', h1 :: h2 :: t ->
      h1 ^ "<i>" ^ h2 ^ "</i>" ^ to_html_helper symbol t
  | '\n', [] -> ""
  | '\n', [ data ] -> data
  | '\n', [ ""; data ] -> "<br>" ^ data
  | '\n', [ data; "" ] -> data ^ "<br>"
  | '\n', [ ""; data; "" ] -> "<br>" ^ data ^ "<br>"
  | '\n', [ text; text2 ] -> text ^ "<br>" ^ text2
  | '\n', h1 :: h2 :: t ->
      h1 ^ "<br>" ^ h2 ^ "<br>" ^ to_html_helper symbol t
  | _ -> failwith "Violates precondition"

let to_html symbol data =
  let split = String.split_on_char symbol data in
  to_html_helper symbol split
