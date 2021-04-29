(* This compilation unit handles word counting and other word functions
   for the text editor *)

let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* splits a string into a list of strings separated by a space and trims
   whitespace around *)
let split_and_trim s =
  let escaped = String.escaped s in
  let split = String.split_on_char ' ' escaped in
  let x = List.map (fun s -> String.trim s) split in
  x

(* count length of list of words *)

let word_count s =
  let filtered =
    List.filter
      (fun s -> s <> "" && s <> "\t" && s <> "\n")
      (Str.split (Str.regexp "[ \n\r\x0c\t]+") s)
  in
  List.length filtered

(* counts the length of any string in characters, minus any whitespace
   characters. Needs to start at 1, not 0 *)
let char_count str =
  let filtered =
    Str.global_replace (Str.regexp "[ \n\r\x0c\t]+") "" str
  in
  String.length filtered
