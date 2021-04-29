open OUnit2
open File
open Cipher
open Yojson

let ex_json = Yojson.Basic.from_file "current_state.json"

(*let json_to_file_test json = assert_equal (Yojson.Basic.from_file
  "new_json.json") (File.json_to_file "new_json.json" ex_json)*)

let gui_tests = []

let file_tests = []

let encrypt_test name key plain cipher =
  name >:: fun _ ->
  assert_equal cipher (encrypt key plain) ~printer:(fun a -> a)

let cipher_test name key plain =
  name >:: fun _ ->
  assert_equal plain
    (plain |> encrypt key |> decrypt key)
    ~printer:(fun a -> a)

let block_list_test name str lst =
  name >:: fun _ ->
  assert_equal lst (block_list str true)
    ~printer:(List.fold_left (fun a acc -> a ^ " " ^ acc) "")

let cipher_tests =
  [
    encrypt_test "Encrypt a message" "password"
      "test message this is a test this is a test"
      "3166745973726b2b2f47466d6267704368414e647a636244644a6246703637675738444a6d524d703652696e6f76557333493847364a594d5747704837434635";
    block_list_test "list test"
      "test message this is a test this is a test"
      [
        "74657374206d6573";
        "7361676520746869";
        "7320697320612074";
        "6573742074686973";
        "2069732061207465";
        "0000000000007374";
      ];
    cipher_test "Encrypt and decrypt a message" "password"
      "test message this is a test this is a test";
    cipher_test "Encrypt and decrypt a message 2" "supersafepassword"
      "SUPER SECRET GOVERNMENT ENCRYPED MESSAGE: Who ate the last \
       slice of whitehouse pizza????";
  ]

let suite =
  "test suite for MS1"
  >::: List.flatten [ file_tests; gui_tests; cipher_tests ]

let _ = run_test_tt_main suite
