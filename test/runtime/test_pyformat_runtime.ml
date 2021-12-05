open Ppx_pyformat_runtime

let test expected actual _ = OUnit2.assert_equal ~printer:Fun.id expected actual

let align_tests =
  let open OUnit2 in
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
         "binary"
         >::: [
                "simple_1" >:: test "0" (format_binary_int 0);
                "simple_2" >:: test "1" (format_binary_int 1);
                "simple_3" >:: test "1111011" (format_binary_int 123);
                "simple_4" >:: test "-1111011" (format_binary_int (-123));
                "alternate_form_1"
                >:: test "0b1" (format_binary_int ~alternate_form:true 1);
                "alternate_form_2"
                >:: test "-0b1" (format_binary_int ~alternate_form:true (-1));
                "sign"
                >::: [
                       "plus_1" >:: test "+1" (format_binary_int ~sign:Plus 1);
                       "plus_2"
                       >:: test "-1" (format_binary_int ~sign:Plus (-1));
                       "plus_3" >:: test "+0" (format_binary_int ~sign:Plus 0);
                       "minus_1" >:: test "1" (format_binary_int ~sign:Minus 1);
                       "minus_2"
                       >:: test "-1" (format_binary_int ~sign:Minus (-1));
                       "minus_3" >:: test "0" (format_binary_int ~sign:Minus 0);
                       "space_1" >:: test " 1" (format_binary_int ~sign:Space 1);
                       "space_2"
                       >:: test "-1" (format_binary_int ~sign:Space (-1));
                       "space_3" >:: test " 0" (format_binary_int ~sign:Space 0);
                     ];
                "grouping_1" >:: test "0" (format_binary_int ~grouping:true 0);
                "grouping_2"
                >:: test "1000" (format_binary_int ~grouping:true 8);
                "grouping_3"
                >:: test "1_0000" (format_binary_int ~grouping:true 16);
                "grouping_4"
                >:: test "111_1011" (format_binary_int ~grouping:true 123);
                "grouping_5"
                >:: test "1000_0000" (format_binary_int ~grouping:true 128);
                "grouping_6"
                >:: test "1_0000_0000" (format_binary_int ~grouping:true 256);
                "align"
                >::: [
                       "left_1"
                       >:: test "1...."
                             (format_binary_int ~fill:(Left, '.', 5) 1);
                       "left_2"
                       >:: test "-1..."
                             (format_binary_int ~fill:(Left, '.', 5) (-1));
                       "right_1"
                       >:: test "....1"
                             (format_binary_int ~fill:(Right, '.', 5) 1);
                       "right_2"
                       >:: test "...-1"
                             (format_binary_int ~fill:(Right, '.', 5) (-1));
                       "center_1"
                       >:: test "..1.."
                             (format_binary_int ~fill:(Center, '.', 5) 1);
                       "center_2"
                       >:: test ".-1.."
                             (format_binary_int ~fill:(Center, '.', 5) (-1));
                       "pad_1"
                       >:: test "....1"
                             (format_binary_int ~fill:(Pad, '.', 5) 1);
                       "pad_1"
                       >:: test "-...1"
                             (format_binary_int ~fill:(Pad, '.', 5) (-1));
                     ];
                "complex_1"
                >:: test "+0b0_0001"
                      (format_binary_int ~fill:(Pad, '0', 9) ~sign:Plus
                         ~grouping:true ~alternate_form:true 1);
                "complex_2"
                >:: test " 0b0_0001"
                      (format_binary_int ~fill:(Pad, '0', 8) ~sign:Space
                         ~grouping:true ~alternate_form:true 1);
                "complex_3"
                >:: test "-0b.....1"
                      (format_binary_int ~fill:(Pad, '.', 9) ~sign:Minus
                         ~grouping:true ~alternate_form:true (-1));
                "complex_4"
                >:: test "0b....1"
                      (format_binary_int ~fill:(Pad, '.', 7) ~sign:Minus
                         ~grouping:true ~alternate_form:true 1);
                "complex_5"
                >:: test "..+0b1..."
                      (format_binary_int ~fill:(Center, '.', 9) ~sign:Plus
                         ~grouping:true ~alternate_form:true 1);
              ];
       ]

let suite =
  let open OUnit2 in
  "test_pyformat_runtime" >::: [ align_tests ]

let _ =
  let open OUnit2 in
  run_test_tt_main suite
