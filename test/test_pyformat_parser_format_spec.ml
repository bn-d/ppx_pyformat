open Ppx_pyformat.Types
open Test_pyformat_parser_utils

let make_fill_tests align =
  let open OUnit2 in
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
              make_field ~format_spec:(make_int ~fill:(x_fill, 1) ()) (Digit 0);
            ];
      "width_2"
      >:: test
            ("{:x" ^ s ^ "16d}")
            [
              make_field ~format_spec:(make_int ~fill:(x_fill, 16) ()) (Digit 0);
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
            [ make_field ~format_spec:(make_int ~fill:(fill, 1) ()) (Digit 0) ];
      "with_zero"
      >:: test
            ("{:x" ^ s ^ "010d}")
            [
              make_field ~format_spec:(make_int ~fill:(x_fill, 10) ()) (Digit 0);
            ];
      "left_curl"
      >:: test
            ("{:{" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:(make_int ~fill:(make_fill ~char_:'{' align, 1) ())
                (Digit 0);
            ];
      "right_curl"
      >:: test
            ("{:}" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:(make_int ~fill:(make_fill ~char_:'}' align, 1) ())
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
            [ make_field ~format_spec:(make_int ~fill:(fill, 1) ()) (Digit 0) ];
    ]
  in
  let special =
    match align with
    | Pad ->
        [
          "simple_zero_int"
          >:: test "{:0d}" [ make_field ~format_spec:(make_int ()) (Digit 0) ];
          "simple_zero_float"
          >:: test "{:0g}" [ make_field ~format_spec:(make_float ()) (Digit 0) ];
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
                  make_field
                    ~format_spec:(make_string ~fill:(fill, 10) ())
                    (Digit 0);
                ];
        ]
    | _ -> []
  in
  name >::: List.concat [ common; special ]

let fill_tests =
  [
    make_fill_tests Left;
    make_fill_tests Right;
    make_fill_tests Center;
    make_fill_tests Pad;
  ]

let suite =
  let open OUnit2 in
  [ "fill" >::: fill_tests ]
