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
  "index"
  >::: [
         "simple_1"
         >:: test "123"
               (let l = [ "123" ] in
                [%pyformat "{l[0]}"]);
         "simple_2"
         >:: test "456"
               (let l = [ ""; "456" ] in
                [%pyformat "{l[1]}"]);
       ]

let conversion_tests =
  "conversion"
  >::: [
         "simple_1"
         >:: test "1."
               (let f = 1.0 in
                [%pyformat "{f!string_of_float}"]);
         "simple_2"
         >:: test "1."
               (let f = 1.0 in
                [%pyformat "{f!Float.to_string}"]);
       ]

let fill_tests =
  "fill"
  >::: [
         "left"
         >:: test "1____"
               (let b = 1 in
                [%pyformat "{b:_<5b}"]);
         "right"
         >:: test "____1"
               (let b = 1 in
                [%pyformat "{b:_>5b}"]);
         "left"
         >:: test "__1__"
               (let b = 1 in
                [%pyformat "{b:_^5b}"]);
         "left"
         >:: test "-___1"
               (let b = -1 in
                [%pyformat "{b:_=5b}"]);
       ]

let sign_tests =
  "sign"
  >::: [
         "plus"
         >:: test "+1"
               (let b = 1 in
                [%pyformat "{b:+b}"]);
         "minus"
         >:: test "1"
               (let b = 1 in
                [%pyformat "{b:-b}"]);
         "space"
         >:: test " 1"
               (let b = 1 in
                [%pyformat "{b: b}"]);
       ]

let grouping_option_tests =
  "grouping_option"
  >::: [
         "comma"
         >:: test "1,000"
               (let d = 1000 in
                [%pyformat "{d:,d}"]);
         "underscore"
         >:: test "1_000"
               (let d = 1000 in
                [%pyformat "{d:_d}"]);
       ]

let format_spec_tests =
  "format_spec"
  >::: [
         fill_tests;
         sign_tests;
         "alternate_form"
         >:: test "0b1"
               (let b = 1 in
                [%pyformat "{b:#b}"]);
         grouping_option_tests;
         "underscore_grouping"
         >:: test "1_0000"
               (let b = 16 in
                [%pyformat "{b:_b}"]);
         "upper"
         >:: test "A"
               (let x = 10 in
                [%pyformat "{x:X}"]);
         (* precision_tests;*)
       ]

let string_tests =
  "string"
  >::: [
         "simple"
         >:: test "str"
               (let s = "str" in
                [%pyformat "{s}"]);
         "left"
         >:: test "str__"
               (let s = "str" in
                [%pyformat "{s:_<5}"]);
         "right"
         >:: test "__str"
               (let s = "str" in
                [%pyformat "{s:_>5}"]);
         "center"
         >:: test "_str_"
               (let s = "str" in
                [%pyformat "{s:_^5}"]);
         "complex"
         >:: test "_str_"
               (let l = [ "str" ] in
                [%pyformat "{l[0]!Fun.id:_^5}"]);
       ]

let int_tests =
  "int"
  >::: [
         "left"
         >:: test "123__"
               (let d = 123 in
                [%pyformat "{d:_<5d}"]);
         "right"
         >:: test "__123"
               (let d = 123 in
                [%pyformat "{d:_>5d}"]);
         "center"
         >:: test "_123_"
               (let d = 123 in
                [%pyformat "{d:_^5d}"]);
         "binary_simple"
         >:: test "1111011"
               (let b = 123 in
                [%pyformat "{b:b}"]);
         "binary_complex_1"
         >:: test "_+111_1011"
               (let b = 123 in
                [%pyformat "{b:_>+10_b}"]);
         "binary_complex_2"
         >:: test "-0b....11"
               (let b = -3 in
                [%pyformat "{b:.=-#9_b}"]);
         "binary_complex_3"
         >:: test "..+0b1..."
               (let b = 1 in
                [%pyformat "{b:.^+#9b}"]);
         "char_simple"
         >:: test "a"
               (let c = 97 in
                [%pyformat "{c:c}"]);
         "char_complex_1"
         >:: test "__a"
               (let c = 97 in
                [%pyformat "{c:_>3c}"]);
         "char_complex_2"
         >:: test "_{__"
               (let c = 123 in
                [%pyformat "{c:_^4c}"]);
         "decimal_simple"
         >:: test "1024"
               (let d = 1024 in
                [%pyformat "{d:d}"]);
         "decimal_complex_1"
         >:: test " 0,001,024"
               (let d = 1024 in
                [%pyformat "{d: 09,d}"]);
         "decimal_complex_2"
         >:: test "+1024....."
               (let d = 1024 in
                [%pyformat "{d:.<+10d}"]);
         "decimal_complex_3"
         >:: test ".-1_024.."
               (let d = -1024 in
                [%pyformat "{d:.^-9_d}"]);
         "octal_simple"
         >:: test "30071"
               (let o = 12345 in
                [%pyformat "{o:o}"]);
         "octal_complex_1"
         >:: test "_+0o3_0071"
               (let o = 12345 in
                [%pyformat "{o:_>+#10_o}"]);
         "octal_complex_2"
         >:: test " 0o030071"
               (let o = 12345 in
                [%pyformat "{o: #09o}"]);
         "octal_complex_3"
         >:: test ".3_0071."
               (let o = 12345 in
                [%pyformat "{o:.^8_o}"]);
         "hex_simple"
         >:: test "1e240"
               (let x = 123456 in
                [%pyformat "{x:x}"]);
         "hex_complex_1"
         >:: test "_ 0X1_E240"
               (let x = 123456 in
                [%pyformat "{x:_> #10_X}"]);
         "hex_complex_2"
         >:: test "+00001e240"
               (let x = 123456 in
                [%pyformat "{x:+010x}"]);
         "hex_complex_3"
         >:: test "0x1_e240__"
               (let x = 123456 in
                [%pyformat "{x:_<-#10_x}"]);
       ]

let float_tests =
  "float"
  >::: [
         "scientific_simple"
         >:: test "1.234560e+05"
               (let e = 123456. in
                [%pyformat "{e:e}"]);
         "scientific_complex_1"
         >:: test "+0_001e+05"
               (let e = 123456. in
                [%pyformat "{e:+010_.0e}"]);
         "scientific_complex_2"
         >:: test "__-1.235E+05"
               (let e = -123456. in
                [%pyformat "{e:_>12.3E}"]);
         "scientific_complex_3"
         >:: test " 00,001.e+05"
               (let e = 123456. in
                [%pyformat "{e:0= #12,.0e}"]);
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
       ]

let _ = run_test_tt_main suite
