open OUnit2

let test exp act _ = assert_equal ~printer:Fun.id exp act

let text_tests =
  "text"
  >::: [
         "simple" >:: test [%pyformat "123abcABC,.;' \t\n"] "123abcABC,.;' \t\n";
         "curl_escape" >:: test [%pyformat "{{}}}}{{"] "{}}{";
         "consolidation" >:: test [%pyformat "123{{}}321"] "123{}321";
       ]

let suite = "test_pyformat_rewriter" >::: [ text_tests ]

let _ = run_test_tt_main suite
