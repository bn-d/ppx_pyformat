open Ppx_pyformat.Types
open Test_pyformat_parser_utils

let text_tests =
  let open OUnit2 in
  [
    "simple" >:: test "123abcABC,.;' \t\n" [ Text "123abcABC,.;' \t\n" ];
    "curl_escape" >:: test "{{}}}}{{" [ Text "{}}{" ];
    "consolidation" >:: test "123{{}}321" [ Text "123{}321" ];
    "single_left"
    >:: test_exc "{" (ValueError "Single '{' encountered in format string");
    "single_right"
    >:: test_exc "}" (ValueError "Single '}' encountered in format string");
  ]

let arg_tests =
  let open OUnit2 in
  [
    "digit_1" >:: test "{0}" [ make_field (Digit 0) ];
    "digit_2" >:: test "{1}" [ make_field (Digit 1) ];
    "digit_3" >:: test "{123}" [ make_field (Digit 123) ];
    "id_1" >:: test "{pi}" [ make_field (Identifier [ "pi" ]) ];
    "id_2" >:: test "{Float.pi}" [ make_field (Identifier [ "pi"; "Float" ]) ];
    "id_3"
    >:: test "{Stdlib.Float.pi}"
          [ make_field (Identifier [ "pi"; "Float"; "Stdlib" ]) ];
    "id_4" >:: test "{my_arg}" [ make_field (Identifier [ "my_arg" ]) ];
    "auto_1" >:: test "{}" [ make_field (Digit 0) ];
    "auto_2"
    >:: test "{}{}{}"
          [ make_field (Digit 0); make_field (Digit 1); make_field (Digit 2) ];
    "auto_3"
    >:: test " {} {} "
          [
            Text " ";
            make_field (Digit 0);
            Text " ";
            make_field (Digit 1);
            Text " ";
          ];
    "auto_to_manual"
    >:: test_exc "{}{0}"
          (ValueError
             "Cannot switch from automatic field numbering to manual field \
              specification");
    "manual_to_auto"
    >:: test_exc "{0}{}"
          (ValueError
             "Cannot switch from manual field specification to automatic field \
              numbering");
  ]

let index_tests =
  let open OUnit2 in
  [
    "simple_1" >:: test "{[0]}" [ make_field ~index:(List_index 0) (Digit 0) ];
    "simple_2" >:: test "{[1]}" [ make_field ~index:(List_index 1) (Digit 0) ];
    "simple_3"
    >:: test "{[123]}" [ make_field ~index:(List_index 123) (Digit 0) ];
    "invalid_1"
    >:: test_exc "{[-1]}" (TypeError "List indices must be integers");
    "invalid_2"
    >:: test_exc "{[abc]}" (TypeError "List indices must be integers");
  ]

let conversion_tests =
  let open OUnit2 in
  [
    "simple_1"
    >:: test "{!float_to_string}"
          [ make_field ~conversion:[ "float_to_string" ] (Digit 0) ];
    "simple_2"
    >:: test "{!Float.to_string}"
          [ make_field ~conversion:[ "to_string"; "Float" ] (Digit 0) ];
    "invalid_1" >:: test_exc "{!1}" (ValueError "");
    "invalid_1" >:: test_exc "{!Float.N}" (ValueError "");
  ]

let general_tests =
  let open OUnit2 in
  [
    "unmatched_1"
    >:: test_exc "{123" (ValueError "Expected '}' before end of string");
    "unmatched_2"
    >:: test_exc "{123:s123" (ValueError "Unmatched '{' in format spec");
    "order" >:: test "abc{}123" [ Text "abc"; make_field (Digit 0); Text "123" ];
  ]

let suite =
  let open OUnit2 in
  "parser"
  >::: [
         "text" >::: text_tests;
         "arg" >::: arg_tests;
         "index" >::: index_tests;
         "conversion" >::: conversion_tests;
         "format_spec" >::: Test_pyformat_parser_format_spec.suite;
         "general" >::: general_tests;
         (* TODO test for invalid ids *)
         (* TODO test for conversion *)
         (* TODO test for multi id *)
         (* TODO test for complex id *)
       ]
