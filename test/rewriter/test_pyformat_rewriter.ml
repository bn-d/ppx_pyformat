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

let suite =
  "test_pyformat_rewriter"
  >::: [ text_tests; index_tests; conversion_tests; string_tests ]

let _ = run_test_tt_main suite
