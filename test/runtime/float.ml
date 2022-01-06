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
                "special_1" >:: test "nan" (float_to_scientific nan);
                "special_2" >:: test "inf" (float_to_scientific infinity);
                "special_3" >:: test "-inf" (float_to_scientific neg_infinity);
                "special_4" >:: test "-0.000000e+00" (float_to_scientific (-0.));
                "alternate_form"
                >:: test "0.e+00"
                      (float_to_scientific ~alternate_form:true ~precision:0 0.);
                "upper_1"
                >:: test "0.000000E+00" (float_to_scientific ~upper:true 0.);
                "upper_2" >:: test "NAN" (float_to_scientific ~upper:true nan);
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
                "complex_6"
                >:: test "+000000nan"
                      (float_to_scientific ~fill:(Pad, '0', 10) ~sign:Plus
                         ~precision:2 nan);
              ];
         "fixed_point"
         >::: [
                "simple_1" >:: test "0.000000" (float_to_fixed_point 0.);
                "simple_2" >:: test "1.000000" (float_to_fixed_point 1.);
                "simple_3"
                >:: test "1234567890.000000" (float_to_fixed_point 1234567890.);
                "simple_4"
                >:: test "-12345.678900" (float_to_fixed_point (-12345.67890));
                "special_1" >:: test "nan" (float_to_fixed_point nan);
                "special_2" >:: test "inf" (float_to_fixed_point infinity);
                "special_3" >:: test "-inf" (float_to_fixed_point neg_infinity);
                "special_4" >:: test "-0.000000" (float_to_fixed_point (-0.));
                "alternate_form"
                >:: test "0."
                      (float_to_fixed_point ~alternate_form:true ~precision:0 0.);
                "upper" >:: test "NAN" (float_to_fixed_point ~upper:true nan);
                "sign"
                >::: [
                       "plus_1"
                       >:: test "+1.000000" (float_to_fixed_point ~sign:Plus 1.);
                       "plus_2"
                       >:: test "-1.000000"
                             (float_to_fixed_point ~sign:Plus (-1.));
                       "plus_3"
                       >:: test "+0.000000" (float_to_fixed_point ~sign:Plus 0.);
                       "minus_1"
                       >:: test "1.000000" (float_to_fixed_point ~sign:Minus 1.);
                       "minus_2"
                       >:: test "-1.000000"
                             (float_to_fixed_point ~sign:Minus (-1.));
                       "minus_3"
                       >:: test "0.000000" (float_to_fixed_point ~sign:Minus 0.);
                       "space_1"
                       >:: test " 1.000000"
                             (float_to_fixed_point ~sign:Space 1.);
                       "space_2"
                       >:: test "-1.000000"
                             (float_to_fixed_point ~sign:Space (-1.));
                       "space_3"
                       >:: test " 0.000000"
                             (float_to_fixed_point ~sign:Space 0.);
                     ];
                "grouping_1"
                >:: test "1.000000"
                      (float_to_fixed_point ~grouping_option:Comma 1.);
                "grouping_2"
                >:: test "100.000000"
                      (float_to_fixed_point ~grouping_option:Comma 100.);
                "grouping_3"
                >:: test "1,000.000000"
                      (float_to_fixed_point ~grouping_option:Comma 1000.);
                "grouping_4"
                >:: test "10,000.000000"
                      (float_to_fixed_point ~grouping_option:Comma 10000.);
                "grouping_5"
                >:: test "100,000.000000"
                      (float_to_fixed_point ~grouping_option:Comma 100000.);
                "grouping_6"
                >:: test "1,000,000.000000"
                      (float_to_fixed_point ~grouping_option:Comma 1000000.);
                "grouping_7"
                >:: test "1_000_000.000000"
                      (float_to_fixed_point ~grouping_option:Underscore 1000000.);
                "align"
                >::: [
                       "left_1"
                       >:: test "1.000000...."
                             (float_to_fixed_point ~fill:(Left, '.', 12) 1.);
                       "left_2"
                       >:: test "-1.000000..."
                             (float_to_fixed_point ~fill:(Left, '.', 12) (-1.));
                       "right_1"
                       >:: test "....1.000000"
                             (float_to_fixed_point ~fill:(Right, '.', 12) 1.);
                       "right_2"
                       >:: test "...-1.000000"
                             (float_to_fixed_point ~fill:(Right, '.', 12) (-1.));
                       "center_1"
                       >:: test "..1.000000.."
                             (float_to_fixed_point ~fill:(Center, '.', 12) 1.);
                       "center_2"
                       >:: test ".-1.000000.."
                             (float_to_fixed_point ~fill:(Center, '.', 12) (-1.));
                       "pad_1"
                       >:: test "....1.000000"
                             (float_to_fixed_point ~fill:(Pad, '.', 12) 1.);
                       "pad_1"
                       >:: test "-...1.000000"
                             (float_to_fixed_point ~fill:(Pad, '.', 12) (-1.));
                     ];
                "precision_1"
                >:: test "12346" (float_to_fixed_point ~precision:0 12345.67890);
                "precision_2"
                >:: test "12345.68"
                      (float_to_fixed_point ~precision:2 12345.67890);
                "precision_3"
                >:: test "12345.678900"
                      (float_to_fixed_point ~precision:6 12345.67890);
                "precision_4"
                >:: test
                      "1.2345678899999998900938180668163113296031951904296875000"
                      (float_to_fixed_point ~precision:55 1.234567890);
                "complex_1"
                >:: test "+0_001"
                      (float_to_fixed_point ~fill:(Pad, '0', 5) ~sign:Plus
                         ~grouping_option:Underscore ~precision:0 1.);
                "complex_2"
                >:: test " 0_000_012.5"
                      (float_to_fixed_point ~fill:(Pad, '0', 12) ~sign:Space
                         ~grouping_option:Underscore ~precision:1 ~upper:true
                         12.5);
                "complex_3"
                >:: test "-...0.012346"
                      (float_to_fixed_point ~fill:(Pad, '.', 12) ~sign:Minus
                         ~grouping_option:Comma (-0.012345678));
                (* make sure the fill is not uppercase *)
                "complex_4"
                >:: test "aaaa1"
                      (float_to_fixed_point ~fill:(Pad, 'a', 5) ~sign:Minus
                         ~precision:0 ~upper:true 1.);
                "complex_5"
                >:: test "..+12.34..."
                      (float_to_fixed_point ~fill:(Center, '.', 11) ~sign:Plus
                         ~precision:2 12.34);
                "complex_6"
                >:: test "+000000nan"
                      (float_to_fixed_point ~fill:(Pad, '0', 10) ~sign:Plus
                         ~precision:2 nan);
              ];
       ]
