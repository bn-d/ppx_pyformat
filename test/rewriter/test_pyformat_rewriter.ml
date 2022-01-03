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
         "binary_simple"
         >:: test "1111011"
               (let b = 123 in
                [%pyformat "{b:b}"]);
         "binary_complex"
         >:: test "_+0b111_1011"
               (let b = 123 in
                [%pyformat "{b:_>+#12_b}"]);
         "char_simple"
         >:: test "a"
               (let c = 97 in
                [%pyformat "{c:c}"]);
         "char_complex"
         >:: test "__a"
               (let c = 97 in
                [%pyformat "{c:_>3c}"]);
         "decimal_simple"
         >:: test "1024"
               (let d = 1024 in
                [%pyformat "{d:d}"]);
         "decimal_complex"
         >:: test "+001,024"
               (let d = 1024 in
                [%pyformat "{d:+08,d}"]);
         "octal_simple"
         >:: test "30071"
               (let o = 12345 in
                [%pyformat "{o:o}"]);
         "octal_complex"
         >:: test "_+0o3_0071"
               (let o = 12345 in
                [%pyformat "{o:_>+#10_o}"]);
         "hex_simple"
         >:: test "1e240"
               (let x = 123456 in
                [%pyformat "{x:x}"]);
         "hex_complex"
         >:: test "_ 0X1_E240"
               (let x = 123456 in
                [%pyformat "{x:_> #10_X}"]);
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
       ]

let _ = run_test_tt_main suite
