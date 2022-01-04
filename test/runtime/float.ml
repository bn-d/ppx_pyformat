open OUnit2
open Ppx_pyformat_runtime
open Utils

let suite =
  "float"
  >::: [
         "scientific"
         >::: [
                "simple_1" >:: test "0.000000e+00" (float_to_scientific 0.);
                "simple_2" >:: test "1.000000e+00" (float_to_scientific 1.);
                "simple_3"
                >:: test "1.234568e+09" (float_to_scientific 1234567890.);
                "simple_4"
                >:: test "-1.234568e+09" (float_to_scientific (-1234567890.));
                "upper"
                >:: test "0.000000E+00" (float_to_scientific ~upper:true 0.);
                "sign"
                >::: [
                       "plus_1"
                       >:: test "+1.000000e+00"
                             (float_to_scientific ~sign:Plus 1.);
                       "plus_2"
                       >:: test "-1.000000e+00"
                             (float_to_scientific ~sign:Plus (-1.));
                       "plus_3"
                       >:: test "+0.000000e+00"
                             (float_to_scientific ~sign:Plus 0.);
                       "minus_1"
                       >:: test "1.000000e+00"
                             (float_to_scientific ~sign:Minus 1.);
                       "minus_2"
                       >:: test "-1.000000e+00"
                             (float_to_scientific ~sign:Minus (-1.));
                       "minus_3"
                       >:: test "0.000000e+00"
                             (float_to_scientific ~sign:Minus 0.);
                       "space_1"
                       >:: test " 1.000000e+00"
                             (float_to_scientific ~sign:Space 1.);
                       "space_2"
                       >:: test "-1.000000e+00"
                             (float_to_scientific ~sign:Space (-1.));
                       "space_3"
                       >:: test " 0.000000e+00"
                             (float_to_scientific ~sign:Space 0.);
                     ];
                "grouping_1"
                >:: test "0.000000e+00"
                      (float_to_scientific ~fill:(Pad, '0', 12)
                         ~grouping_option:Comma 0.);
                "grouping_2"
                >:: test "000.000000e+00"
                      (float_to_scientific ~fill:(Pad, '0', 14)
                         ~grouping_option:Comma 0.);
                "grouping_3"
                >:: test "0,000.000000e+00"
                      (float_to_scientific ~fill:(Pad, '0', 15)
                         ~grouping_option:Comma 0.);
                "grouping_4"
                >:: test "0,000.000000e+00"
                      (float_to_scientific ~fill:(Pad, '0', 16)
                         ~grouping_option:Comma 0.);
                "grouping_5"
                >:: test "000,000.000000e+00"
                      (float_to_scientific ~fill:(Pad, '0', 18)
                         ~grouping_option:Comma 0.);
                "grouping_6"
                >:: test "0,000,000.000000e+00"
                      (float_to_scientific ~fill:(Pad, '0', 19)
                         ~grouping_option:Comma 0.);
                "grouping_7"
                >:: test "0_000_000.000000e+00"
                      (float_to_scientific ~fill:(Pad, '0', 20)
                         ~grouping_option:Underscore 0.);
                "align"
                >::: [
                       "left_1"
                       >:: test "1.000000e+00...."
                             (float_to_scientific ~fill:(Left, '.', 16) 1.);
                       "left_2"
                       >:: test "-1.000000e+00..."
                             (float_to_scientific ~fill:(Left, '.', 16) (-1.));
                       "right_1"
                       >:: test "....1.000000e+00"
                             (float_to_scientific ~fill:(Right, '.', 16) 1.);
                       "right_2"
                       >:: test "...-1.000000e+00"
                             (float_to_scientific ~fill:(Right, '.', 16) (-1.));
                       "center_1"
                       >:: test "..1.000000e+00.."
                             (float_to_scientific ~fill:(Center, '.', 16) 1.);
                       "center_2"
                       >:: test ".-1.000000e+00.."
                             (float_to_scientific ~fill:(Center, '.', 16) (-1.));
                       "pad_1"
                       >:: test "....1.000000e+00"
                             (float_to_scientific ~fill:(Pad, '.', 16) 1.);
                       "pad_1"
                       >:: test "-...1.000000e+00"
                             (float_to_scientific ~fill:(Pad, '.', 16) (-1.));
                     ];
                "precision_1"
                >:: test "1e+09" (float_to_scientific ~precision:0 1234567890.);
                "precision_2"
                >:: test "1.23e+09"
                      (float_to_scientific ~precision:2 1234567890.);
                "precision_3"
                >:: test "1.234568e+09"
                      (float_to_scientific ~precision:6 1234567890.);
                "precision_4"
                >:: test
                      "1.2345678899999998900938180668163113296031951904296875000e+00"
                      (float_to_scientific ~precision:55 1.234567890);
                "complex_1"
                >:: test "+0_001e+00"
                      (float_to_scientific ~fill:(Pad, '0', 9) ~sign:Plus
                         ~grouping_option:Underscore ~precision:0 1.);
                "complex_2"
                >:: test " 0_001.2E-01"
                      (float_to_scientific ~fill:(Pad, '0', 12) ~sign:Space
                         ~grouping_option:Underscore ~precision:1 ~upper:true
                         0.125);
                "complex_3"
                >:: test "-....1.234568e-02"
                      (float_to_scientific ~fill:(Pad, '.', 17) ~sign:Minus
                         ~grouping_option:Comma (-0.012345678));
                (* make sure the fill is not uppercase *)
                "complex_4"
                >:: test "aaaa1E+00"
                      (float_to_scientific ~fill:(Pad, 'a', 9) ~sign:Minus
                         ~precision:0 ~upper:true 1.);
                "complex_5"
                >:: test "..+1.23e+01..."
                      (float_to_scientific ~fill:(Center, '.', 14) ~sign:Plus
                         ~precision:2 12.34);
              ];
       ]
