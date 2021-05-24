(** [chaotic_funct n] is a number calculated from [n] that differs
    greatly when [n] changes. *)
let chaotic_funct n =
  let den = float_of_int (n mod 255) /. (255. *. 10.) in
  let sn = (125. *. sin (1. /. den)) +. 125. in
  int_of_float sn

let array_sum arr = Array.fold_left (fun n acc -> n + acc) 0 arr

(** [xor_enc k t] is [t] encrypted using key [k]. [k] is an array of
    integers and [t] is a list of integers, they do not need to be the
    same size *)
let xor_enc k t =
  let key_ascii_sum = array_sum k in
  let key_len = Array.length k in
  let rec enc_list lst i acc =
    match lst with
    | [] -> acc
    | h :: t ->
        let ind = i mod key_len in
        let ch = chaotic_funct (Array.get k ind + i + key_ascii_sum) in
        enc_list t (i + 1) (Int.logxor ch h :: acc)
  in
  List.rev (enc_list t 0 [])

(** [ascii_of_ind str i] is the ascii code for the character at index
    [i] of string [str]. Raises: Invalid_argument if n not a valid index
    in s. *)
let ascii_of_ind str i = str |> (fun s -> String.get s i) |> Char.code

(** [str_to_ascii_array str] is an array such that index i of the array
    is the ascii representation of the character at index i of [str] *)
let str_to_ascii_array str =
  let str_len = String.length str in
  let arr = Array.make str_len 0 in
  for i = 0 to str_len - 1 do
    Array.set arr i (ascii_of_ind str i)
  done;
  arr

(** [str_to_ascii_array str] is a list such that index i of the list is
    the ascii representation of the character at index i of [str] *)
let str_to_ascii_list str =
  let str_len = String.length str in
  let rec ascii_l ind acc =
    if ind >= str_len then acc
    else ascii_l (ind + 1) (ascii_of_ind str ind :: acc)
  in
  List.rev (ascii_l 0 [])

(** [ceasar s lst e] is the list of integers [lst], except that it is
    encrypted with an ascii ceasar cipher. If [e], then [ceasar s lst e]
    is a ciphertext, otherwise [ceasar s lst e] is a plaintext *)
let ceasar s lst e =
  let shift = if e then s else -s in
  let const = if e then 0 else 255 in
  List.map (fun n -> ((n + shift) mod 255) + const) lst

(** [lst_to_string l] is the list of integers [l] converted into a space
    separated string of integers *)
let rec lst_to_string l =
  let rec l_t_s lst acc =
    match lst with
    | [] -> acc
    | h :: t -> l_t_s t (acc ^ " " ^ string_of_int h)
  in
  let str = l_t_s l "" in
  String.sub str 1 (String.length str - 1)

(** [str_to_list s] is the list of integers [it1;it2;it3;...;itn] if [s]
    is \"it1 it2 it3 ... itn\", where it1-itn are all integers. "*)
let str_to_list s =
  let str_lst = String.split_on_char ' ' s in
  List.map (fun c -> int_of_string c) str_lst

(** [lst_to_ascii_string lst] is the list of ascii integers [lst]
    converted into a string. *)
let lst_to_ascii_string lst =
  let chr_lst = List.rev (List.map (fun n -> Char.chr n) lst) in
  Scanf.unescaped
    (List.fold_left (fun acc c -> Char.escaped c ^ acc) "" chr_lst)

let encrypt k p =
  if k = "" || p = "" then p
  else
    let key_arr = str_to_ascii_array k in
    let p_list = str_to_ascii_list p in
    let key_sum = array_sum key_arr in
    let shifted_p = ceasar key_sum p_list true in
    let xored_p = xor_enc key_arr shifted_p in
    lst_to_string xored_p

let decrypt k c =
  if k = "" || c = "" then c
  else
    let key_arr = str_to_ascii_array k in
    let c_list = str_to_list c in
    let key_sum = array_sum key_arr in
    let xored_c = xor_enc key_arr c_list in
    let shifted_c = ceasar key_sum xored_c false in
    lst_to_ascii_string shifted_c
