let subkeys = ref (Array.make 18 "0")

let sboxes = ref (Array.make_matrix 4 256 "0")

(* Initializes subkeys *)
let init_subkeys =
  let subk = File.open_to_string "data/encryption/subkeys" in
  let subk_list = String.split_on_char ' ' subk in
  subkeys := Array.of_list subk_list

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

(* Initializes all 4 sboxes *)
let init_sboxes =
  for i = 0 to 3 do
    init_sbox i
  done

(* Temporary implementation for testing*)
let encrypt k p = !sboxes.(1).(255)

let decrypt k c = failwith "unimplemented"
