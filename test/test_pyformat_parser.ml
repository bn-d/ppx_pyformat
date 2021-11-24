open Ppx_pyformat.Types

let format_spec = String_format { fill = None }

let make_field ?index ?conversion ?(format_spec = format_spec) arg =
  Field (make_replacement_field ~arg ?index ?conversion ~format_spec ())

let make_string = make_string_format_of_format_spec

let make_int = make_int_format_of_format_spec

let make_float = make_float_format_of_format_spec

let test str expected _ =
  Ppx_pyformat.Utils.parse str
  |> OUnit2.assert_equal ~printer:Test_utils.string_of_elements expected

let test_exc str exc _ =
  let f _ = Ppx_pyformat.Utils.parse str in
  OUnit2.assert_raises exc f

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

let fill_tests =
  let open OUnit2 in
  let make_tests align =
    let name, c =
      match align with
      | Left -> ("left", '<')
      | Right -> ("right", '>')
      | Center -> ("center", '^')
      | Pad -> ("pad", '=')
    in
    let s = String.make 1 c in
    let fill = make_fill ~char_:' ' align in
    let x_fill = make_fill ~char_:'x' align in
    let common =
      [
        "no_width_1"
        >:: test
              ("{:" ^ s ^ "d}")
              [ make_field ~format_spec:(make_int ()) (Digit 0) ];
        "no_width_2"
        >:: test
              ("{: " ^ s ^ "d}")
              [ make_field ~format_spec:(make_int ()) (Digit 0) ];
        "width_1"
        >:: test
              ("{:x" ^ s ^ "1d}")
              [
                make_field
                  ~format_spec:(make_int ~fill:(x_fill, 1) ())
                  (Digit 0);
              ];
        "width_2"
        >:: test
              ("{:x" ^ s ^ "16d}")
              [
                make_field
                  ~format_spec:(make_int ~fill:(x_fill, 16) ())
                  (Digit 0);
              ];
        "width_3"
        >:: test
              ("{:x" ^ s ^ "256d}")
              [
                make_field
                  ~format_spec:(make_int ~fill:(x_fill, 256) ())
                  (Digit 0);
              ];
        "default_char"
        >:: test
              ("{:" ^ s ^ "1d}")
              [
                make_field ~format_spec:(make_int ~fill:(fill, 1) ()) (Digit 0);
              ];
        "with_zero"
        >:: test
              ("{:x" ^ s ^ "010d}")
              [
                make_field
                  ~format_spec:(make_int ~fill:(x_fill, 10) ())
                  (Digit 0);
              ];
        "left_curl"
        >:: test
              ("{:{" ^ s ^ "1d}")
              [
                make_field
                  ~format_spec:
                    (make_int ~fill:(make_fill ~char_:'{' align, 1) ())
                  (Digit 0);
              ];
        "right_curl"
        >:: test
              ("{:}" ^ s ^ "1d}")
              [
                make_field
                  ~format_spec:
                    (make_int ~fill:(make_fill ~char_:'}' align, 1) ())
                  (Digit 0);
              ];
        "tab"
        >:: test
              ("{:\n" ^ s ^ "1d}")
              [
                make_field
                  ~format_spec:
                    (make_int ~fill:(make_fill ~char_:'\n' align, 1) ())
                  (Digit 0);
              ];
        "newline"
        >:: test
              ("{:\t" ^ s ^ "1d}")
              [
                make_field
                  ~format_spec:
                    (make_int ~fill:(make_fill ~char_:'\t' align, 1) ())
                  (Digit 0);
              ];
        "space"
        >:: test
              ("{: " ^ s ^ "1d}")
              [
                make_field ~format_spec:(make_int ~fill:(fill, 1) ()) (Digit 0);
              ];
      ]
    in
    let special =
      match align with
      | Pad ->
          [
            "simple_zero_int"
            >:: test "{:0d}" [ make_field ~format_spec:(make_int ()) (Digit 0) ];
            "simple_zero_float"
            >:: test "{:0f}"
                  [ make_field ~format_spec:(make_float ()) (Digit 0) ];
            "invalid_invalid_1"
            >:: test_exc "{:0}"
                  (ValueError
                     "'=' alignment not allowed in string format specifier");
            "invalid_invalid_2"
            >:: test_exc "{:x=10}"
                  (ValueError
                     "'=' alignment not allowed in string format specifier");
            "invalid_invalid_3"
            >:: test_exc "{:010}"
                  (ValueError
                     "'=' alignment not allowed in string format specifier");
          ]
      | Right ->
          [
            "default_align"
            >:: test "{:10}"
                  [
                    make_field
                      ~format_spec:(make_string ~fill:(fill, 10) ())
                      (Digit 0);
                  ];
          ]
      | _ -> []
    in
    name >::: List.concat [ common; special ]
  in
  [ make_tests Left; make_tests Right; make_tests Center; make_tests Pad ]

let format_spec_tests =
  let open OUnit2 in
  [ "fill" >::: fill_tests ]

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
         "format_spec" >::: format_spec_tests;
         "general" >::: general_tests;
         (* TODO test for invalid ids *)
         (* TODO test for conversion *)
         (* TODO test for multi id *)
         (* TODO test for complex id *)
       ]
