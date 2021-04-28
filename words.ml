(* This compilation unit handles word counting and other word functions
   for the text editor *)

(* splits a string into a list of strings separated by a space and trims
   whitespace around *)
let split_and_trim s =
  let split = String.split_on_char ' ' s in
  List.map (fun s -> String.trim s) split

(* count length of list of words *)

let word_count s =
  let filtered =
    List.filter
      (fun s -> s = "" && s = "\t" && s = "\n")
      (split_and_trim s)
  in
  List.length filtered
