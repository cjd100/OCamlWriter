open Str

(** Introduces additional regex constructs that are not included in
    OCaml's Str module by default. New constructs:

    <A>{I} will match the string A, repeated I times.

    \d Matches any digit character, equivalent to [0-9]

    \D Matches any non-digit character, equivalent to [^0-9]

    \w Matches any alphabet character, equivalent to [A-Za-z]

    \W Matches any non-alphabet character, equivalent to [^A-Za-z] *)
let rec extended_regex str =
  let multi_match =
    mult str
      (matching_strings (regexp "<[^\\|<\\|>\\|{\\|}]+>{[0-9]+}") str
      |> List.rev)
  in
  let word_group =
    global_replace (regexp_string "\w") "[A-Za-z]" multi_match
  in
  let non_word_group =
    global_replace (regexp_string "\W") "[^A-Za-z]" word_group
  in
  let digit_group =
    global_replace (regexp_string "\d") "[0-9]" non_word_group
  in
  let non_digit_group =
    global_replace (regexp_string "\D") "[^0-9]" digit_group
  in
  non_digit_group

(** [mult str lst] replaces the first occurence of each string in [lst]
    from [str] with an OCaml Str regex representation of a string
    repeated multiple times. *)
and mult str lst =
  match lst with
  | [] -> str
  | h :: t ->
      let split = mult_split h in
      let rep_str = repeat_string (fst split) (snd split) "" in
      let new_s =
        replace_first
          (regexp "<[^\\|<\\|>\\|{\\|}]+>{[0-9]+}")
          rep_str str
      in
      mult new_s t

(** [mult_split str] splits the string [str] of format \"<A>{B}\" into
    the pair (A,int_of_string B). Precondition: [str] must be of the
    format \"<A>{B}\", where A is a string and B is a string that can be
    converted into an integer" *)
and mult_split str =
  let angle_inds = (String.index str '<', String.index str '>') in
  let curly_inds = (String.index str '{', String.index str '}') in
  let t1 =
    String.sub str
      (fst angle_inds + 1)
      (snd angle_inds - fst angle_inds - 1)
  in
  let t2 =
    String.sub str
      (fst curly_inds + 1)
      (snd curly_inds - fst curly_inds - 1)
  in
  (t1, int_of_string t2)

and repeat_string str n acc =
  if n <= 0 then acc else repeat_string str (n - 1) (str ^ acc)

(** [matching_strings reg str] is a list of the substrings of [str] that
    match [reg] *)
and matching_strings reg str =
  let lst = full_split reg str in
  List.fold_left
    (fun acc s ->
      match s with Delim str -> str :: acc | Text str -> acc)
    [] lst

(** [replace reg n str] replaces the regular expression [reg] with [n]
    in [str] *)
let replace reg n str = global_replace reg n str

(** [replace_first reg n str] replaces the first instance of regular
    expression [reg] with [n] in [str] *)
let replace_first reg n str = replace_first reg n str

(** [find reg str ind] finds the first occurence of the regular
    expression [reg] in [str] after index [ind] *)
let find reg str ind =
  try search_forward reg str ind with Not_found -> -1

let replace_reg reg_s n str =
  let reg = regexp (extended_regex reg_s) in
  replace reg n str

let find_reg reg_s str ind =
  let reg = regexp (extended_regex reg_s) in
  find reg str ind

let replace_exact r n str =
  let reg = regexp_string r in
  replace reg n str

let find_exact r str ind =
  let reg = regexp_string r in
  find reg str ind

let replace_reg_first reg_s n str =
  let reg = regexp (extended_regex reg_s) in
  replace_first reg n str

let replace_exact_first r n str =
  let reg = regexp_string r in
  replace_first reg n str
