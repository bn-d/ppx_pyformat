open OUnit2
open Ppx_pyformat_runtime
open Utils

let align_tests =
  "align"
  >::: [
         "left"
         >::: [
                "empty_1" >:: test "" (align_left ' ' 0 "");
                "empty_2" >:: test "   " (align_left ' ' 3 "");
                "negative" >:: test "123" (align_left ' ' (-1) "123");
                "zero" >:: test "123" (align_left ' ' 0 "123");
                "less" >:: test "123" (align_left ' ' 1 "123");
                "same" >:: test "123" (align_left ' ' 3 "123");
                "simple_1" >:: test "abc " (align_left ' ' 4 "abc");
                "simple_2" >:: test "123400" (align_left '0' 6 "1234");
              ];
         "right"
         >::: [
                "empty_1" >:: test "" (align_right ' ' 0 "");
                "empty_2" >:: test "   " (align_right ' ' 3 "");
                "negative" >:: test "123" (align_right ' ' (-1) "123");
                "zero" >:: test "123" (align_right ' ' 0 "123");
                "less" >:: test "123" (align_right ' ' 1 "123");
                "same" >:: test "123" (align_right ' ' 3 "123");
                "simple_1" >:: test " abc" (align_right ' ' 4 "abc");
                "simple_2" >:: test "001234" (align_right '0' 6 "1234");
              ];
         "left"
         >::: [
                "empty_1" >:: test "" (align_center ' ' 0 "");
                "empty_2" >:: test "   " (align_center ' ' 3 "");
                "negative" >:: test "123" (align_center ' ' (-1) "123");
                "zero" >:: test "123" (align_center ' ' 0 "123");
                "less" >:: test "123" (align_center ' ' 1 "123");
                "same" >:: test "123" (align_center ' ' 3 "123");
                "simple_1" >:: test "abc " (align_center ' ' 4 "abc");
                "simple_2" >:: test " abc " (align_center ' ' 5 "abc");
                "simple_3" >:: test " abc  " (align_center ' ' 6 "abc");
                "simple_4" >:: test "12340" (align_center '0' 5 "1234");
                "simple_5" >:: test "012340" (align_center '0' 6 "1234");
                "simple_6" >:: test "0123400" (align_center '0' 7 "1234");
              ];
       ]

let suite = "test_pyformat_runtime" >::: [ align_tests; Int.suite; Float.suite ]
let _ = run_test_tt_main suite
