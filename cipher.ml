(** [int_of_hex hex] is the base 10 integer representation of [hex].
    Precondition: [hex] is a hexadecimal string *)
let int_of_hex hex = Int64.of_string ("0x" ^ hex)

(** [hex_or_int num] is the hexadecimal string representation of [num]. *)
let hex_of_int num =
  let hex_str n =
    let r = Int64.to_int n in
    if 0 <= r && r <= 9 then string_of_int r
    else if 10 <= r && r <= 15 then String.make 1 (char_of_int (87 + r))
    else failwith "Invalid hexadecimal digit"
  in
  let rec aux n acc =
    let div = Int64.div n 16L in
    let rem = Int64.rem n 16L in
    if div = 0L then hex_str rem ^ acc else aux div (hex_str rem ^ acc)
  in
  aux num ""

(** [ascii_to_hex str] is the string [str] converted into hexadecimal *)
let ascii_to_hex str =
  let char_lst = str |> String.to_seq |> List.of_seq in
  String.concat ""
    (List.map
       (fun c -> int_of_char c |> Int64.of_int |> hex_of_int)
       char_lst)

(** [pad_length s str] is the string [str], except it has length [s],
    padded with zeros at the beginning if needed*)
let pad_length s str =
  let len = String.length str in
  String.sub (String.make s '0' ^ str) len s

let mod32val =
  let rec gen c acc =
    if c = 32L then acc else gen (Int64.add c 1L) (Int64.mul 2L acc)
  in
  gen 1L 2L

(** Addition modulo 32 of two hexadeciaml numbers *)
let add_hex h1 h2 =
  pad_length 8
    (hex_of_int
       (Int64.rem (Int64.add (int_of_hex h1) (int_of_hex h2)) mod32val))

let subkeys = ref (Array.make 18 "0")

let sboxes = ref (Array.make_matrix 4 256 "0")

(** [init_subkeys key] initializes subkeys using [key] *)
let init_subkeys key =
  let subk = File.open_to_string "data/encryption/subkeys" in
  let subk_list = String.split_on_char ' ' subk in
  subkeys := Array.of_list subk_list;
  let hex_key = ascii_to_hex key in
  let len = String.length hex_key in
  let j = ref 0 in
  for i = 0 to 17 do
    if !j + 8 >= len then j := 0;
    print_endline hex_key;
    print_endline (string_of_int !j);
    print_endline !subkeys.(i);
    !subkeys.(i) <-
      hex_of_int
        (Int64.logxor
           (int_of_hex !subkeys.(i))
           (int_of_hex (String.sub hex_key !j 8)));
    j := !j + 8
  done

(** [init_sbox] initializes the [n]th sbox *)
let init_sbox n =
  let lines =
    "data/encryption/sbox" ^ string_of_int (n + 1)
    |> File.open_to_string
    |> String.split_on_char '\n'
  in
  for i = 0 to 31 do
    let line = List.nth lines i |> String.split_on_char ' ' in
    for s = 0 to 7 do
      let str = List.nth line s in
      !sboxes.(n).((i * 8) + s) <- str
    done
  done

(** [init_sboxes] initializes all 4 sboxes *)
let init_sboxes () =
  for i = 0 to 3 do
    init_sbox i
  done

(** [f hex] is an internal function that gets used each round of the
    encryption. The result is a 32 bit hexadecimal string.s *)
let f h =
  let hex = pad_length 8 h in
  let x_a = String.sub hex 0 2 in
  let x_b = String.sub hex 2 2 in
  let x_c = String.sub hex 4 2 in
  let x_d = String.sub hex 6 2 in
  let res1 =
    int_of_hex
      (add_hex
         !sboxes.(0).(Int64.to_int (int_of_hex x_a))
         !sboxes.(1).(Int64.to_int (int_of_hex x_b)))
  in
  let res2 =
    Int64.logxor res1
      (int_of_hex !sboxes.(2).(Int64.to_int (int_of_hex x_c)))
  in
  add_hex (hex_of_int res2) !sboxes.(3).(Int64.to_int (int_of_hex x_d))

(** [round i block] is the plaintext hexadecimal block that occurs when
    [block] goes through the [i]th round of encryption. Output and input
    are 64 bit hexadecimals*)
let round i block =
  let left = String.sub block 0 8 in
  (*print_string block; print_endline ""; print_string (string_of_int
    i); print_endline ""; *)
  let right = String.sub block 8 8 in
  let new_right =
    pad_length 8
      (hex_of_int
         (Int64.logxor (int_of_hex left) (int_of_hex !subkeys.(i))))
  in
  let new_left =
    pad_length 8
      (hex_of_int
         (Int64.logxor (int_of_hex (f new_right)) (int_of_hex right)))
  in
  new_left ^ new_right

(** [post_processing_e block] is a 64 bit hexadecimal block that is the
    result of the final processing on the 64 bit hexadecimal [block].
    Used in encryption*)
let post_processing_e block =
  let left = String.sub block 0 8 in
  let right = String.sub block 8 8 in
  let new_right =
    hex_of_int
      (Int64.logxor (int_of_hex left) (int_of_hex !subkeys.(16)))
  in
  let new_left =
    hex_of_int
      (Int64.logxor (int_of_hex right) (int_of_hex !subkeys.(17)))
  in
  new_left ^ new_right

(** [post_processing_d block] is a 64 bit hexadecimal block that is the
    result of the final processing on the 64 bit hexadecimal [block].
    Used in decryption*)
let post_processing_d block =
  let left = String.sub block 0 8 in
  let right = String.sub block 8 8 in
  let new_right =
    hex_of_int
      (Int64.logxor (int_of_hex left) (int_of_hex !subkeys.(1)))
  in
  let new_left =
    hex_of_int
      (Int64.logxor (int_of_hex right) (int_of_hex !subkeys.(0)))
  in
  new_left ^ new_right

(** [block_list p e] is a list of [p] split up into 64 bit blocks. [p]
    is a string, the blocks that p is split up into are hexadecimal
    blocks of the ascii representation of [p]. [e] is true when the
    function is used for encrytion, false otherwise. *)
let block_list p e =
  let hex_string = if e then ascii_to_hex p else p in
  let rec lst str acc =
    let len = String.length str in
    if len = 0 then acc
    else if len > 16 then
      lst (String.sub str 16 (len - 16)) (String.sub str 0 16 :: acc)
    else
      let s_padded = String.sub ("0000000000000000" ^ str) len 16 in
      lst "" (s_padded :: acc)
  in
  List.rev (lst hex_string [])

(* [hex_to_ascii h] is the hexadecimal representation of ascii encoded
   string [h]. Precondition: h is an even length hexadecimal string *)
let hex_to_ascii h =
  let rec hex_to_ascii_helper h acc =
    let len = String.length h in
    if len = 2 then
      acc
      ^ (String.sub h 0 2 |> int_of_hex |> Int64.to_int |> Char.chr
       |> String.make 1)
    else if len <> 0 then
      hex_to_ascii_helper
        (String.sub h 2 (len - 2))
        (acc
        ^ (String.sub h 0 2 |> int_of_hex |> Int64.to_int |> Char.chr
         |> String.make 1))
    else acc
  in
  hex_to_ascii_helper h ""

(** [encrypt_block b r] encrypts block [b] starting at round [r] and
    ending at round 16. Precondition: 0 <= r <= 16 *)
let rec encrypt_block b r =
  if r <> 16 then encrypt_block (round r b) (r + 1)
  else post_processing_e b

(** [decrypt_block b r] decrypts block [b] starting at round [r] and
    ending at round 1. Precondition: 1 <= r <= 17 *)
let rec decrypt_block b r =
  if r <> 1 then decrypt_block (round r b) (r - 1)
  else post_processing_d b

let encrypt k p =
  init_subkeys k;
  init_sboxes ();
  let bl = block_list p true in
  let rec encrypt_blocks acc = function
    | [] -> acc
    | h :: t -> encrypt_blocks (acc ^ encrypt_block h 0) t
  in
  encrypt_blocks "" bl

let decrypt k c =
  init_subkeys k;
  init_sboxes ();
  let bl = block_list c false in
  let rec decrypt_blocks acc = function
    | [] -> acc
    | h :: t -> decrypt_blocks (acc ^ decrypt_block h 17) t
  in
  hex_to_ascii (decrypt_blocks "" bl)
