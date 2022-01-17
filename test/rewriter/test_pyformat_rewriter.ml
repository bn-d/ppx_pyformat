open OUnit2

let test exp act _ = assert_equal ~printer:Fun.id exp act

let text_tests =
  "text"
  >::: [
         "simple" >:: test "123abcABC,.;' \t\n" [%pyformat "123abcABC,.;' \t\n"];
         "curl_escape" >:: test "{}}{" [%pyformat "{{}}}}{{"];
         "consolidation" >:: test "123{}321" [%pyformat "123{{}}321"];
       ]

let index_tests =
  let l1 = [ "123" ] and l2 = [ ""; "456" ] in
  "index"
  >::: [
         "simple_1" >:: test "123" [%pyformat "{l1[0]}"];
         "simple_2" >:: test "456" [%pyformat "{l2[1]}"];
       ]

let conversion_tests =
  let f = 1.0 in
  "conversion"
  >::: [
         "simple_1" >:: test "1." [%pyformat "{f!string_of_float}"];
         "simple_2" >:: test "1." [%pyformat "{f!Float.to_string}"];
       ]

let fill_tests =
  let b1 = 1 and b2 = -1 in
  "fill"
  >::: [
         "left" >:: test "1____" [%pyformat "{b1:_<5b}"];
         "right" >:: test "____1" [%pyformat "{b1:_>5b}"];
         "center" >:: test "__1__" [%pyformat "{b1:_^5b}"];
         "padding" >:: test "-___1" [%pyformat "{b2:_=5b}"];
       ]

let sign_tests =
  let b = 1 in
  "sign"
  >::: [
         "plus" >:: test "+1" [%pyformat "{b:+b}"];
         "minus" >:: test "1" [%pyformat "{b:-b}"];
         "space" >:: test " 1" [%pyformat "{b: b}"];
       ]

let grouping_option_tests =
  let d = 1000 in
  "grouping_option"
  >::: [
         "comma" >:: test "1,000" [%pyformat "{d:,d}"];
         "underscore" >:: test "1_000" [%pyformat "{d:_d}"];
       ]

let format_spec_tests =
  let b1 = 1 and b2 = 16 and x1 = 10 in
  "format_spec"
  >::: [
         fill_tests;
         sign_tests;
         "alternate_form" >:: test "0b1" [%pyformat "{b1:#b}"];
         grouping_option_tests;
         "underscore_grouping" >:: test "1_0000" [%pyformat "{b2:_b}"];
         "upper" >:: test "A" [%pyformat "{x1:X}"];
         (* precision_tests;*)
       ]

let string_tests =
  let s = "str" and l = [ "str" ] in
  "string"
  >::: [
         "simple" >:: test "str" [%pyformat "{s}"];
         "left" >:: test "str__" [%pyformat "{s:_<5}"];
         "right" >:: test "__str" [%pyformat "{s:_>5}"];
         "center" >:: test "_str_" [%pyformat "{s:_^5}"];
         "complex" >:: test "_str_" [%pyformat "{l[0]!Fun.id:_^5}"];
         "default_align" >:: test "str  " [%pyformat "{s:5}"];
       ]

let int_tests =
  let b1 = 123
  and b2 = -3
  and c1 = 97
  and d1 = 1024
  and d2 = -1024
  and o1 = 12345
  and x1 = 123456 in
  "int"
  >::: [
         "left" >:: test "1024_" [%pyformat "{d1:_<5d}"];
         "right" >:: test "_1024" [%pyformat "{d1:_>5d}"];
         "center" >:: test "1024_" [%pyformat "{d1:_^5d}"];
         "default_align" >:: test " 1024" [%pyformat "{d1:5d}"];
         "binary_simple" >:: test "1111011" [%pyformat "{b1:b}"];
         "binary_complex_1" >:: test "_+111_1011" [%pyformat "{b1:_>+10_b}"];
         "binary_complex_2" >:: test "-0b....11" [%pyformat "{b2:.=-#9_b}"];
         "binary_complex_3" >:: test "..-0b11.." [%pyformat "{b2:.^+#9b}"];
         "char_simple" >:: test "a" [%pyformat "{c1:c}"];
         "char_complex_1" >:: test "__a" [%pyformat "{c1:_>3c}"];
         "char_complex_2" >:: test "_{__" [%pyformat "{b1:_^4c}"];
         "decimal_simple" >:: test "1024" [%pyformat "{d1:d}"];
         "decimal_complex_1" >:: test " 0,001,024" [%pyformat "{d1: 09,d}"];
         "decimal_complex_2" >:: test "+1024....." [%pyformat "{d1:.<+10d}"];
         "decimal_complex_3" >:: test ".-1_024.." [%pyformat "{d2:.^-9_d}"];
         "octal_simple" >:: test "30071" [%pyformat "{o1:o}"];
         "octal_complex_1" >:: test "_+0o3_0071" [%pyformat "{o1:_>+#10_o}"];
         "octal_complex_2" >:: test " 0o030071" [%pyformat "{o1: #09o}"];
         "octal_complex_3" >:: test ".3_0071." [%pyformat "{o1:.^8_o}"];
         "hex_simple" >:: test "1e240" [%pyformat "{x1:x}"];
         "hex_complex_1" >:: test "_ 0X1_E240" [%pyformat "{x1:_> #10_X}"];
         "hex_complex_2" >:: test "+00001e240" [%pyformat "{x1:+010x}"];
         "hex_complex_3" >:: test "0x1_e240__" [%pyformat "{x1:_<-#10_x}"];
       ]

let float_tests =
  let e1 = 123456.
  and e2 = -123456.
  and f1 = 123.456
  and f2 = -123.456
  and p1 = 0.123456
  and p2 = -0.123456 in
  "float"
  >::: [
         "scientific_simple" >:: test "1.234560e+05" [%pyformat "{e1:e}"];
         "scientific_special" >:: test "nan" [%pyformat "{nan:e}"];
         "scientific_simple" >:: test "1.234560e+05" [%pyformat "{e1:e}"];
         "scientific_complex_1"
         >:: test "+000_001e+05" [%pyformat "{e1:+012_.0e}"];
         "scientific_complex_2"
         >:: test "__-1.235E+05" [%pyformat "{e2:_>12.3E}"];
         "scientific_complex_3"
         >:: test " 00,001.e+05" [%pyformat "{e1:0= #12,.0e}"];
         "fixed_point_simple" >:: test "3.141593" [%pyformat "{Float.pi:f}"];
         "fixed_point_special" >:: test "nan" [%pyformat "{nan:f}"];
         "fixed_point_complex_1"
         >:: test "+0_000_123" [%pyformat "{f1:+010_.0f}"];
         "fixed_point_complex_2"
         >:: test "__-123.456" [%pyformat "{f2:_>10.3F}"];
         "fixed_point_complex_3"
         >:: test "__ 123.___" [%pyformat "{f1:_^ #10,.0f}"];
         "general_simple" >:: test "123.456" [%pyformat "{f1:g}"];
         "general_special" >:: test "nan" [%pyformat "{nan:g}"];
         "general_complex_1" >:: test "+0_000_123" [%pyformat "{f1:+010_.3g}"];
         "general_complex_2" >:: test "_-1.23E+05" [%pyformat "{e2:_>10.3G}"];
         "general_complex_3" >:: test "_ 1.e+02__" [%pyformat "{f1:_^ #10,.0g}"];
         "percentage_simple" >:: test "12.345600%" [%pyformat "{p1:%}"];
         "percentage_special" >:: test "nan%" [%pyformat "{nan:%}"];
         "percentage_complex_1"
         >:: test "+0_000_012%" [%pyformat "{p1:+010_.0%}"];
         "percentage_complex_2" >:: test "__-12.346%" [%pyformat "{p2:_>10.3%}"];
         "percentage_complex_3"
         >:: test "__ 12.%___" [%pyformat "{p1:_^ #10,.0%}"];
       ]

let arg_tests =
  let s1 = "hello" and s2 = "world" in
  "arg"
  >::: [
         "simple"
         >:: test "hello"
               [%pyformat
                 "{0}";
                 s1];
         "multiple"
         >:: test "hello world!"
               [%pyformat
                 "{0} {1}{2}";
                 s1;
                 s2;
                 "!"];
         "mixed"
         >:: test "hello world!"
               [%pyformat
                 "{s1} {s2}{0}";
                 "!"];
         "auto"
         >:: test "hello world!"
               [%pyformat
                 "{} {}{}";
                 s1;
                 s2;
                 "!"];
         "duplicated"
         >:: test "hello hello"
               [%pyformat
                 "{0} {0}";
                 s1];
         "unused"
         >:: test "world"
               [%pyformat
                 "{1}";
                 s1;
                 s2];
       ]

let suite =
  "test_pyformat_rewriter"
  >::: [
         text_tests;
         index_tests;
         conversion_tests;
         format_spec_tests;
         string_tests;
         int_tests;
         float_tests;
         arg_tests;
       ]

let _ = run_test_tt_main suite
