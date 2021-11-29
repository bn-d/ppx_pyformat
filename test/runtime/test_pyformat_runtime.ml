open Ppx_pyformat_runtime

let test expected actual _ = OUnit2.assert_equal ~printer:Fun.id expected actual

let align_tests =
  let open OUnit2 in
  "align"
  >::: [
         "left"
         >::: [
                "empty_1" >:: test "" (align_left 0 ' ' "");
                "empty_2" >:: test "   " (align_left 3 ' ' "");
                "negative" >:: test "123" (align_left (-1) ' ' "123");
                "zero" >:: test "123" (align_left 0 ' ' "123");
                "less" >:: test "123" (align_left 1 ' ' "123");
                "same" >:: test "123" (align_left 3 ' ' "123");
                "simple_1" >:: test "abc " (align_left 4 ' ' "abc");
                "simple_2" >:: test "123400" (align_left 6 '0' "1234");
              ];
         "right"
         >::: [
                "empty_1" >:: test "" (align_right 0 ' ' "");
                "empty_2" >:: test "   " (align_right 3 ' ' "");
                "negative" >:: test "123" (align_right (-1) ' ' "123");
                "zero" >:: test "123" (align_right 0 ' ' "123");
                "less" >:: test "123" (align_right 1 ' ' "123");
                "same" >:: test "123" (align_right 3 ' ' "123");
                "simple_1" >:: test " abc" (align_right 4 ' ' "abc");
                "simple_2" >:: test "001234" (align_right 6 '0' "1234");
              ];
         "left"
         >::: [
                "empty_1" >:: test "" (align_center 0 ' ' "");
                "empty_2" >:: test "   " (align_center 3 ' ' "");
                "negative" >:: test "123" (align_center (-1) ' ' "123");
                "zero" >:: test "123" (align_center 0 ' ' "123");
                "less" >:: test "123" (align_center 1 ' ' "123");
                "same" >:: test "123" (align_center 3 ' ' "123");
                "simple_1" >:: test "abc " (align_center 4 ' ' "abc");
                "simple_2" >:: test " abc " (align_center 5 ' ' "abc");
                "simple_3" >:: test " abc  " (align_center 6 ' ' "abc");
                "simple_4" >:: test "12340" (align_center 5 '0' "1234");
                "simple_5" >:: test "012340" (align_center 6 '0' "1234");
                "simple_6" >:: test "0123400" (align_center 7 '0' "1234");
              ];
       ]

let suite =
  let open OUnit2 in
  "test_pyformat_runtime" >::: [ align_tests ]

let _ =
  let open OUnit2 in
  run_test_tt_main suite
