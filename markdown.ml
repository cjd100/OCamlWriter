open Soup

let write_html path data =
  let modded_path =
    match String.split_on_char '.' path with
    | [] -> failwith "Violates precondition"
    | h :: t -> h
  in
  Soup.write_file (modded_path ^ ".html") data

let format_html data = data |> parse |> pretty_print
