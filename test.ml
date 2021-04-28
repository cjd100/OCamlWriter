open OUnit2
open File
open Cipher
open Yojson

let ex_json = Yojson.Basic.from_file "current_state.json"

(*let json_to_file_test json = assert_equal (Yojson.Basic.from_file
  "new_json.json") (File.json_to_file "new_json.json" ex_json)*)

let gui_tests = []

let file_tests = []

let temporary_test name key plain cipher =
  name >:: fun _ ->
  assert_equal "" (Cipher.encrypt "" "") ~printer:(fun a ->
      "#" ^ a ^ "#")

let cipher_tests = [ temporary_test "test" "" "" "" ]

let suite =
  "test suite for MS1"
  >::: List.flatten [ file_tests; gui_tests; cipher_tests ]

let _ = run_test_tt_main suite
