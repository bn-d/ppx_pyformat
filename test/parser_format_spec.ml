open Ppx_pyformat.Types
open Parser_utils

let arg = Digit 0

let make_fill_tests align =
  let open OUnit2 in
  let name, s =
    match align with
    | Left -> ("left", "<")
    | Right -> ("right", ">")
    | Center -> ("center", "^")
    | Pad -> ("pad", "=")
  in
  let fill = make_fill ~char_:' ' align in
  let x_fill = make_fill ~char_:'x' align in
  let common =
    [
      "no_width_1"
      >:: test ("{:" ^ s ^ "d}") [ make_field ~format_spec:(make_int ()) arg ];
      "no_width_2"
      >:: test ("{: " ^ s ^ "d}") [ make_field ~format_spec:(make_int ()) arg ];
      "width_1"
      >:: test
            ("{:x" ^ s ^ "1d}")
            [ make_field ~format_spec:(make_int ~fill:(x_fill, 1) ()) arg ];
      "width_2"
      >:: test
            ("{:x" ^ s ^ "16d}")
            [ make_field ~format_spec:(make_int ~fill:(x_fill, 16) ()) arg ];
      "width_3"
      >:: test
            ("{:x" ^ s ^ "256d}")
            [ make_field ~format_spec:(make_int ~fill:(x_fill, 256) ()) arg ];
      "default_char"
      >:: test
            ("{:" ^ s ^ "1d}")
            [ make_field ~format_spec:(make_int ~fill:(fill, 1) ()) arg ];
      "with_zero"
      >:: test
            ("{:x" ^ s ^ "010d}")
            [ make_field ~format_spec:(make_int ~fill:(x_fill, 10) ()) arg ];
      "left_curl"
      >:: test
            ("{:{" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:(make_int ~fill:(make_fill ~char_:'{' align, 1) ())
                arg;
            ];
      "right_curl"
      >:: test
            ("{:}" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:(make_int ~fill:(make_fill ~char_:'}' align, 1) ())
                arg;
            ];
      "newline"
      >:: test
            ("{:\n" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:
                  (make_int ~fill:(make_fill ~char_:'\n' align, 1) ())
                arg;
            ];
      "tab"
      >:: test
            ("{:\t" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:
                  (make_int ~fill:(make_fill ~char_:'\t' align, 1) ())
                arg;
            ];
      "space"
      >:: test
            ("{: " ^ s ^ "1d}")
            [ make_field ~format_spec:(make_int ~fill:(fill, 1) ()) arg ];
    ]
  in
  let special =
    match align with
    | Pad ->
        [
          "simple_zero_int"
          >:: test "{:0d}" [ make_field ~format_spec:(make_int ()) arg ];
          "simple_zero_float"
          >:: test "{:0g}" [ make_field ~format_spec:(make_float ()) arg ];
          "invalid_1"
          >:: test_exc "{:0}"
                (ValueError
                   "'=' alignment not allowed in string format specifier");
          "invalid_2"
          >:: test_exc "{:x=10}"
                (ValueError
                   "'=' alignment not allowed in string format specifier");
          "invalid_3"
          >:: test_exc "{:010}"
                (ValueError
                   "'=' alignment not allowed in string format specifier");
        ]
    | Right ->
        [
          "default_align"
          >:: test "{:10}"
                [
                  make_field ~format_spec:(make_string ~fill:(fill, 10) ()) arg;
                ];
        ]
    | _ -> []
  in
  name >::: List.concat [ common; special ]

let fill_tests =
  let open OUnit2 in
  "fill"
  >::: [
         make_fill_tests Left;
         make_fill_tests Right;
         make_fill_tests Center;
         make_fill_tests Pad;
       ]

let make_sign_tests sign =
  let open OUnit2 in
  let name, s =
    match sign with
    | Plus -> ("plus", "+")
    | Minus -> ("minus", "-")
    | Space -> ("space", " ")
  in
  let tests =
    [
      "int"
      >:: test
            ("{:" ^ s ^ "d}")
            [ make_field ~format_spec:(make_int ~sign ()) arg ];
      "float"
      >:: test
            ("{:" ^ s ^ "g}")
            [ make_field ~format_spec:(make_float ~sign ()) arg ];
      "string"
      >:: test_exc
            ("{:" ^ s ^ "}")
            (ValueError "Sign not allowed in string format specifier");
    ]
  in
  name >::: tests

let sign_tests =
  let open OUnit2 in
  "sign"
  >::: [ make_sign_tests Plus; make_sign_tests Minus; make_sign_tests Space ]

let suite =
  let open OUnit2 in
  "format_spec" >::: [ fill_tests; sign_tests ]
