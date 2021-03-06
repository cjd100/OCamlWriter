open OUnit2
open QCheck
open File
open Cipher
open Yojson
open Words
open Regex

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

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let ex_json = Yojson.Basic.from_file "current_state.json"

(*let json_to_file_test json = assert_equal (Yojson.Basic.from_file
  "new_json.json") (File.json_to_file "new_json.json" ex_json)*)

let gui_tests = []

(* how to ensure this executes *)
let test_file1 =
  File.create_file "test_file_1";
  File.save_to_file "test file 1!"

let file_open_test name result filename =
  name >:: fun _ -> assert_equal result (File.open_to_string filename)

let file_tests =
  [ (*file_open_test "open test_file_1" "test file 1!" "test_file_1"*) ]

let encrypt_test name key plain cipher =
  name >:: fun _ ->
  assert_equal cipher (encrypt key plain) ~printer:(fun a -> a)

let cipher_test name key plain =
  name >:: fun _ ->
  assert_equal plain
    (plain |> encrypt key |> decrypt key)
    ~printer:(fun a -> a)

let word_count_test name result str =
  name >:: fun _ ->
  assert_equal result (Words.word_count str) ~printer:string_of_int

let char_count_test name result str =
  name >:: fun _ ->
  assert_equal result (Words.char_count str) ~printer:string_of_int

let uniq_count_test name result str =
  name >:: fun _ ->
  assert_equal result (Words.uniq_count str) ~printer:string_of_int

let remove_dups_test name result lst =
  name >:: fun _ ->
  assert_equal result (Words.remove_dups lst) ~cmp:cmp_set_like_lists
    ~printer:(pp_list pp_string)

let replace_test name reg rep str result exact first =
  let replace =
    match (exact, first) with
    | true, true -> Regex.replace_exact_first
    | true, false -> Regex.replace_exact
    | false, true -> Regex.replace_reg_first
    | false, false -> Regex.replace_reg
  in
  name >:: fun _ ->
  assert_equal result (replace reg rep str) ~printer:(fun a -> a)

let find_test name reg str ind result exact =
  let find = if exact then Regex.find_exact else Regex.find_reg in
  name >:: fun _ ->
  assert_equal result (find reg str ind) ~printer:string_of_int

let cipher_tests =
  [
    cipher_test "Encrypt and decrypt a message" "password"
      "test message this is a test this is a test";
    cipher_test "Encrypt and decrypt a message 2" "supersafepassword"
      "SUPER SECRET GOVERNMENT ENCRYPED MESSAGE: Who ate the last \
       slice of whitehouse pizza????";
    cipher_test "Encrypt and decrypt a message 3"
      "verylongandintricatepassword"
      "According to all known laws\n\
      \       of aviation,\n\
      \       \n\
      \         \n\
      \       there is no way a bee\n\
      \       should be able to fly.\n\
      \       \n\
      \         \n\
      \       Its wings are too small to get\n\
      \       its fat little body off the ground.\n\
      \       \n\
      \         \n\
      \       The bee, of course, flies anyway\n\
      \       \n\
      \         \n\
      \       because bees don't care\n\
      \       what humans think is impossible.\n\
      \       \n\
      \         \n\
      \       Yellow, black. Yellow, black.\n\
      \       Yellow, black. Yellow, black.\n\
      \       \n\
      \         \n\
      \       Ooh, black and yellow!\n\
      \       Let's shake it up a little.\n\
      \       \n\
      \         \n\
      \       Barry! Breakfast is ready!\n\
      \       \n\
      \       ";
    cipher_test "lowercase alphanumeric 1" "aabb09182736ccdd"
      "123456abcd132536";
    cipher_test "lowercase alphanumeric 2" "abab12345920"
      "12930bcdfe39120";
    cipher_test "lowercase alphanumeric 3" "abazb12345920"
      "12930bcdfe39120";
    cipher_test "lowercase alphanumeric 4" "abazb12345920"
      "12930bcdfez39120";
    cipher_test "uppercase alphanumeric 1" "ABAZB12345920"
      "12930BCDFEZ39120";
    cipher_test "uppercase punctuation alphanumeric 1" "ABAZB1234,,5920"
      "12930:   BCDF,EZ39120";
    cipher_test "uppercase & lowercase alphanumeric 1" "ABazB12345920"
      "12930BcdfEZ39120";
    cipher_test "Bee movie - simple passwords" "abazb12345920"
      "According to all known laws\n\
      \       of aviation,\n\
      \       \n\
      \         \n\
      \       there is no way a bee\n\
      \       should be able to fly.\n\
      \       \n\
      \         \n\
      \       Its wings are too small to get\n\
      \       its fat little body off the ground.\n\
      \       \n\
      \         \n\
      \       The bee, of course, flies anyway\n\
      \       \n\
      \         \n\
      \       because bees don't care\n\
      \       what humans think is impossible.\n\
      \       \n\
      \         \n\
      \       Yellow, black. Yellow, black.\n\
      \       Yellow, black. Yellow, black.\n\
      \       \n\
      \         \n\
      \       Ooh, black and yellow!\n\
      \       Let's shake it up a little.\n\
      \       \n\
      \         \n\
      \       Barry! Breakfast is ready!\n\
      \       \n\
      \       ";
    cipher_test "Bee movie - simple passwords - unspaced"
      "abazb12345920"
      "According to all known lawsof aviation,there is no way a \
       beeshould be able to fly.Its wings are too small to getits fat \
       little body off the ground.The bee, of course, flies \
       anywaybecause bees don't carewhat humans think is \
       impossible.Yellow, black. Yellow, black.Yellow, black. Yellow, \
       black.Ooh, black and yellow!Let's shake it up a little.Barry! \
       Breakfast is ready!";
    cipher_test "Random password, simple text"
      "someweird random password" "abebasb312313bb3123bbdasd";
    cipher_test "short even length" "abazb12345920" "ah";
    cipher_test "short odd length" "abazb12345920" "aha";
    cipher_test "Empty key" "" "message 1";
    cipher_test "Empty message" "password" "";
    cipher_test "Empty key and message" "" "";
  ]

let cipher_property str pass =
  try
    if String.contains (String.escaped str) '\\' then true
    else
      str |> Scanf.unescaped |> encrypt pass |> decrypt pass
      = Scanf.unescaped str
  with _ -> true

let cipher_randomized_test =
  [
    QCheck.Test.make QCheck.string
      (fun mess -> cipher_property mess "password")
      ~name:"Cipher test: Random messages, set password";
    QCheck.Test.make QCheck.string
      (fun pass ->
        cipher_property
          "Testin test test m,essage this is a test \
           3l1232JSJ!*J*S*J@JJ*DJ*DSJAD**J@J*J*J*SJ*D 3j123jk \
           e1kdl;;d;1j2d j;d1jdjk2jkdjk12jkdkj2 kd d1"
          pass)
      ~name:"Cipher test: Random messages, set password";
  ]

(* tests for Word compilation unit. Tests word counting functionality
   for various types of strings *)
let word_tests =
  [
    word_count_test "empty str" 0 "";
    word_count_test "string with one space" 0 " ";
    word_count_test "string with newline and space" 0 " \n";
    word_count_test "string of 1 character" 1 "a";
    word_count_test "string with word" 1 "hello!!!";
    word_count_test "multiple words" 6 "hi my name is michael clarkson";
    word_count_test "multiple words with a newline in it" 6
      "hi my name \n is michael clarkson";
    char_count_test "empty str" 0 "";
    char_count_test "space str" 0 " ";
    char_count_test "string with newline and space" 0 " \n";
    char_count_test "string with 1 word" 6 "hello!";
    char_count_test "string with a few words" 8 "hi my name";
    char_count_test "string with words and newlines" 8 "hi my \n name";
    char_count_test "string with only whitespace characters" 0
      " \n\n \t \t \x0c";
    (* uniq_count tests *)
    uniq_count_test "empty str" 0 "";
    uniq_count_test "string with a space" 0 " ";
    uniq_count_test "string with random whitespace chars" 0
      " \n\n \t \t \x0c";
    uniq_count_test "string with 1 word" 1 "hello";
    uniq_count_test "string with 2 of the same word" 1 "hello hello";
    uniq_count_test "string with multiple words" 3
      "hi asasasd clarkson \t";
    uniq_count_test "string with multiple words and weird whitespace" 4
      "a bbbbb cc\t\nasd a";
    (* remove_dups tests *)
    remove_dups_test "empty list" [] [];
    remove_dups_test "list with 1 elt" [ "a" ] [ "a" ];
    remove_dups_test "list with 2 of the same elt" [ "a" ] [ "a"; "a" ];
    remove_dups_test "list with different elements" [ "a"; "b" ]
      [ "a"; "b" ];
    remove_dups_test "more complicated list" [ "a"; "b"; "c" ]
      [ "a"; "a"; "b"; "b"; "c" ];
  ]

let regex_tests =
  [
    replace_test "Replace the word Jeff with [Redacted]" "Jeff"
      "[Redacted]"
      "Jeff went to the store, where Jeff purchased many Jeff"
      "[Redacted] went to the store, where [Redacted] purchased many \
       [Redacted]"
      true false;
    replace_test "Delete any string that is enclosed by lowercase a's"
      "[a].*?[a]" "" "a this will be deleted, a but not this!"
      " but not this!" false false;
    replace_test
      "Replaces the word secret with letters in between it with the \
       word secret"
      "s.*e.*c.*r.*e.*t" "secret" "This is the secret message"
      "Thisecret message" false false;
    replace_test "Replaces fizz with buzz, string does not contain fizz"
      "fizz" "buzz" "just some string" "just some string" true false;
    replace_test "Replaces fizz with buzz" "fizz" "buzz"
      "fizzbuzz fizzbuzz fizz the buzz"
      "buzzbuzz buzzbuzz buzz the buzz" true false;
    find_test "Find the first occurence of Waldo" "Waldo"
      "Noah Emma Oliver Ava Elijah Waldo William Sophia James Amelia \
       Benjamin Isabella Lucas Mia Henry Evelyn Alexander Harper"
      0 28 true;
    find_test "Find the first occurence of Waldo starting at index 28"
      "Waldo"
      "Noah Emma Oliver Ava Elijah Waldo William Sophia James Amelia \
       Benjamin Isabella Lucas Mia Henry Evelyn Alexander Harper"
      28 28 true;
    find_test "Find the first occurence of Waldo starting at index 29"
      "Waldo"
      "Noah Emma Oliver Ava Elijah Waldo William Sophia James Amelia \
       Benjamin Isabella Lucas Mia Henry Evelyn Alexander Harper"
      29 ~-1 true;
    replace_test
      "Replace all phone numbers with [Redacted] (formatted as \
       ###-###-####)"
      "<[0-9]>{3}-<[0-9]>{3}-<[0-9]>{4}" "[Redacted]"
      "Call 800-999-1234 for a free autoquote today! For inquiries \
       with our legal team, dial 786-123-4567"
      "Call [Redacted] for a free autoquote today! For inquiries with \
       our legal team, dial [Redacted]"
      false false;
    find_test "Find the first instance of a phone number in a string"
      "<[0-9]>{3}-<[0-9]>{3}-<[0-9]>{4}"
      "Call 800-999-1234 for a free autoquote today! For inquiries \
       with our legal team, dial 786-123-4567"
      0 5 false;
    find_test
      "Password verification regex: password is 8 characters long, and \
       only contains letters and numbers. Valid password"
      "<[A-Za-z0-9]>{8}" "p455w0rd" 0 0 false;
    find_test
      "Password verification regex: password is 8 characters long, and \
       only contains letters and numbers. Invalid password"
      "<[A-Za-z0-9]>{8}" "P&assw0rd" 0 ~-1 false;
    replace_test
      "Replace the first instance of a phone number with \"Nope!\""
      "<[0-9]>{3}-<[0-9]>{3}-<[0-9]>{4}" "Nope!"
      "Call 800-999-1234 for a free autoquote today! For inquiries \
       with our legal team, dial 786-123-4567"
      "Call Nope! for a free autoquote today! For inquiries with our \
       legal team, dial 786-123-4567"
      false true;
  ]

let suite =
  "test suite for MS1"
  >::: List.flatten
         [
           file_tests; gui_tests; cipher_tests; word_tests; regex_tests;
         ]

let _ =
  run_test_tt_main suite;
  QCheck_runner.run_tests_main cipher_randomized_test
