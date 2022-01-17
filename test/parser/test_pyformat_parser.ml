open OUnit2
open Ppx_pyformat.Types
open Utils

let text_tests =
  "text"
  >::: [
         "simple" >:: test "123abcABC,.;' \t\n" [ Text "123abcABC,.;' \t\n" ];
         "curl_escape" >:: test "{{}}}}{{" [ Text "{}}{" ];
         "consolidation" >:: test "123{{}}321" [ Text "123{}321" ];
         "single_left"
         >:: test_exc "{" (ValueError "Single '{' encountered in format string");
         "single_right"
         >:: test_exc "}" (ValueError "Single '}' encountered in format string");
       ]

let arg_tests =
  "arg"
  >::: [
         "digit_1" >:: test "{0}" [ make_field (Digit 0) ];
         "digit_2" >:: test "{1}" [ make_field (Digit 1) ];
         "digit_3" >:: test "{123}" [ make_field (Digit 123) ];
         "id_1" >:: test "{pi}" [ make_field (Identifier [ "pi" ]) ];
         "id_2"
         >:: test "{Float.pi}" [ make_field (Identifier [ "Float"; "pi" ]) ];
         "id_3"
         >:: test "{Stdlib.Float.pi}"
               [ make_field (Identifier [ "Stdlib"; "Float"; "pi" ]) ];
         "id_4" >:: test "{my_arg}" [ make_field (Identifier [ "my_arg" ]) ];
         "auto_1" >:: test "{}" [ make_field (Digit 0) ];
         "auto_2"
         >:: test "{}{}{}"
               [
                 make_field (Digit 0);
                 make_field (Digit 1);
                 make_field (Digit 2);
               ];
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
                  "Cannot switch from automatic field numbering to manual \
                   field specification");
         "manual_to_auto"
         >:: test_exc "{0}{}"
               (ValueError
                  "Cannot switch from manual field specification to automatic \
                   field numbering");
         "invalid_1" >:: test_exc "{123asasd}" (ValueError "Invalid specifier");
         "invalid_2" >:: test_exc "{Stdlib:}" (ValueError "Invalid specifier");
         "invalid_3" >:: test_exc "{Stdlib.:}" (KeyError "Invalid identifier");
         "invalid_4" >:: test_exc "{-1:}" (ValueError "Invalid specifier");
       ]

let index_tests =
  "index"
  >::: [
         "simple_1"
         >:: test "{[0]}" [ make_field ~index:(List_index 0) (Digit 0) ];
         "simple_2"
         >:: test "{[1]}" [ make_field ~index:(List_index 1) (Digit 0) ];
         "simple_3"
         >:: test "{[123]}" [ make_field ~index:(List_index 123) (Digit 0) ];
         "invalid_1"
         >:: test_exc "{[-1]}" (TypeError "List indices must be integers");
         "invalid_2"
         >:: test_exc "{[abc]}" (TypeError "List indices must be integers");
       ]

let conversion_tests =
  "conversion"
  >::: [
         "simple_1"
         >:: test "{!float_to_string}"
               [ make_field ~conversion:[ "float_to_string" ] (Digit 0) ];
         "simple_2"
         >:: test "{!Float.to_string}"
               [ make_field ~conversion:[ "Float"; "to_string" ] (Digit 0) ];
         "invalid_1"
         >:: test_exc "{!1}"
               (KeyError "Invalid identifier for conversion function");
         "invalid_2"
         >:: test_exc "{!Float.N}"
               (KeyError "Invalid identifier for conversion function");
       ]

let general_tests =
  "general"
  >::: [
         "unmatched_1"
         >:: test_exc "{123" (ValueError "Expected '}' before end of string");
         "unmatched_2"
         >:: test_exc "{123:s123" (ValueError "Unmatched '{' in format spec");
         "order"
         >:: test "abc{}123" [ Text "abc"; make_field (Digit 0); Text "123" ];
         "complex_1"
         >:: test "my_date: {my_date!Date.to_string:->9s}\n"
               [
                 Text "my_date: ";
                 make_field ~conversion:[ "Date"; "to_string" ]
                   ~format_spec:
                     (make_string
                        ~fill:(make_fill ~char_:'-' ~width:9 Right)
                        ())
                   (Identifier [ "my_date" ]);
                 Text "\n";
               ];
         "complex_2"
         >:: test "1st: {[0]!int_of_float:d} 2nd: {[1]:#_.10f}"
               [
                 Text "1st: ";
                 make_field ~index:(List_index 0) ~conversion:[ "int_of_float" ]
                   ~format_spec:(make_int ()) (Digit 0);
                 Text " 2nd: ";
                 make_field ~index:(List_index 1)
                   ~format_spec:
                     (make_float ~alternate_form:true
                        ~grouping_option:Underscore ~precision:10 ~type_:Fixed
                        ())
                   (Digit 1);
               ];
       ]

let suite =
  "test_pyformat_parser"
  >::: [
         text_tests;
         arg_tests;
         index_tests;
         conversion_tests;
         Format_spec.suite;
         general_tests;
       ]

let _ = run_test_tt_main suite
