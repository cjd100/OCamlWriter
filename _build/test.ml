open OUnit2
open File
open Gui

let gui_tests = []

let file_tests = []

let suite =
  "test suite for MS1" >::: List.flatten [ file_tests; gui_tests ]

let _ = run_test_tt_main suite
