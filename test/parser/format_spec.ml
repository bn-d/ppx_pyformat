open OUnit2
open Ppx_pyformat.Types
open Utils

let arg = Digit 0

let make_fill_tests align s =
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
            [
              make_field ~format_spec:(make_int ~fill:(x_fill ~width:1) ()) arg;
            ];
      "width_2"
      >:: test
            ("{:x" ^ s ^ "16d}")
            [
              make_field ~format_spec:(make_int ~fill:(x_fill ~width:16) ()) arg;
            ];
      "width_3"
      >:: test
            ("{:x" ^ s ^ "256d}")
            [
              make_field
                ~format_spec:(make_int ~fill:(x_fill ~width:256) ())
                arg;
            ];
      "default_char"
      >:: test
            ("{:" ^ s ^ "1d}")
            [ make_field ~format_spec:(make_int ~fill:(fill ~width:1) ()) arg ];
      "with_zero"
      >:: test
            ("{:x" ^ s ^ "010d}")
            [
              make_field ~format_spec:(make_int ~fill:(x_fill ~width:10) ()) arg;
            ];
      "left_curl"
      >:: test
            ("{:{" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:
                  (make_int ~fill:(make_fill ~char_:'{' ~width:1 align) ())
                arg;
            ];
      "right_curl"
      >:: test
            ("{:}" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:
                  (make_int ~fill:(make_fill ~char_:'}' ~width:1 align) ())
                arg;
            ];
      "newline"
      >:: test
            ("{:\n" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:
                  (make_int ~fill:(make_fill ~char_:'\n' ~width:1 align) ())
                arg;
            ];
      "tab"
      >:: test
            ("{:\t" ^ s ^ "1d}")
            [
              make_field
                ~format_spec:
                  (make_int ~fill:(make_fill ~char_:'\t' ~width:1 align) ())
                arg;
            ];
      "space"
      >:: test
            ("{: " ^ s ^ "1d}")
            [ make_field ~format_spec:(make_int ~fill:(fill ~width:1) ()) arg ];
    ]
  in
  let special =
    match align with
    | Pad ->
        [
          "zero_int"
          >:: test "{:0d}" [ make_field ~format_spec:(make_int ()) arg ];
          "zero_float"
          >:: test "{:0g}" [ make_field ~format_spec:(make_float ()) arg ];
          "string_1"
          >:: test_exc "{:0}"
                (ValueError
                   "'=' alignment not allowed in string format specifier");
          "string_2"
          >:: test_exc "{:x=10}"
                (ValueError
                   "'=' alignment not allowed in string format specifier");
          "string_3"
          >:: test_exc "{:010}"
                (ValueError
                   "'=' alignment not allowed in string format specifier");
        ]
    | Left ->
        [
          "string_default"
          >:: test "{:10s}"
                [
                  make_field
                    ~format_spec:(make_string ~fill:(fill ~width:10) ())
                    arg;
                ];
        ]
    | Right ->
        [
          "int_default"
          >:: test "{:10d}"
                [
                  make_field
                    ~format_spec:(make_int ~fill:(fill ~width:10) ())
                    arg;
                ];
          "float_default"
          >:: test "{:10g}"
                [
                  make_field
                    ~format_spec:(make_float ~fill:(fill ~width:10) ())
                    arg;
                ];
        ]
    | _ -> []
  in
  List.concat [ common; special ]

let fill_tests =
  "fill"
  >::: [
         "left" >::: make_fill_tests Left "<";
         "right" >::: make_fill_tests Right ">";
         "center" >::: make_fill_tests Center "^";
         "pad" >::: make_fill_tests Pad "=";
       ]

let make_sign_tests sign s =
  let string_error =
    match sign with
    | Space -> "Space not allowed in string format specifier"
    | _ -> "Sign not allowed in string format specifier"
  in
  [
    "int"
    >:: test
          ("{:" ^ s ^ "d}")
          [ make_field ~format_spec:(make_int ~sign ()) arg ];
    "float"
    >:: test
          ("{:" ^ s ^ "g}")
          [ make_field ~format_spec:(make_float ~sign ()) arg ];
    "string" >:: test_exc ("{:" ^ s ^ "}") (ValueError string_error);
  ]

let sign_tests =
  "sign"
  >::: [
         "plus" >::: make_sign_tests Plus "+";
         "minus" >::: make_sign_tests Minus "-";
         "space" >::: make_sign_tests Space " ";
       ]

let alternate_form_tests =
  let alternate_form = true in
  "alternate_form"
  >::: [
         "int"
         >:: test "{:#d}"
               [ make_field ~format_spec:(make_int ~alternate_form ()) arg ];
         "float"
         >:: test "{:#g}"
               [ make_field ~format_spec:(make_float ~alternate_form ()) arg ];
         "string"
         >:: test_exc "{:#}"
               (ValueError
                  "Alternate form (#) not allowed in string format specifier");
       ]

let make_grouping_option_tests grouping_option s =
  [
    "int"
    >:: test
          ("{:" ^ s ^ "d}")
          [ make_field ~format_spec:(make_int ~grouping_option ()) arg ];
    "float"
    >:: test
          ("{:" ^ s ^ "g}")
          [ make_field ~format_spec:(make_float ~grouping_option ()) arg ];
    "string"
    >:: test_exc
          ("{:" ^ s ^ "}")
          (ValueError "Grouping option not allowed in string format specifier");
  ]

let grouping_option_tests =
  "grouping_option"
  >::: [
         "comma" >::: make_grouping_option_tests Comma ",";
         "underscore" >::: make_grouping_option_tests Underscore "_";
         "mixed"
         >:: test_exc "{:,_}" (ValueError "Cannot specify both ',' and '_'");
         "binary"
         >:: test_exc "{:,b}"
               (ValueError "Cannot specify ',' with 'b', 'o', 'x' or 'X'");
         "octal"
         >:: test_exc "{:,o}"
               (ValueError "Cannot specify ',' with 'b', 'o', 'x' or 'X'");
         "hex"
         >:: test_exc "{:,x}"
               (ValueError "Cannot specify ',' with 'b', 'o', 'x' or 'X'");
       ]

let precision_tests =
  "precision"
  >::: [
         "float_1"
         >:: test "{:.0g}"
               [ make_field ~format_spec:(make_float ~precision:0 ()) arg ];
         "float_2"
         >:: test "{:.1g}"
               [ make_field ~format_spec:(make_float ~precision:1 ()) arg ];
         "float_3"
         >:: test "{:.16g}"
               [ make_field ~format_spec:(make_float ~precision:16 ()) arg ];
         "float_4"
         >:: test "{:.256g}"
               [ make_field ~format_spec:(make_float ~precision:256 ()) arg ];
         "no_precision"
         >:: test_exc "{:.g}" (ValueError "Format specifier missing precision");
         "int"
         >:: test_exc "{:.1d}"
               (ValueError "Precision not allowed in integer format specifier");
         "string"
         >:: test_exc "{:.1}"
               (ValueError "Precision not allowed in string format specifier");
       ]

let type_tests =
  "type"
  >::: [
         "default_1"
         >:: test "{}" [ make_field ~format_spec:(make_string ()) arg ];
         "default_2"
         >:: test "{:}" [ make_field ~format_spec:(make_string ()) arg ];
         "string"
         >:: test "{:s}" [ make_field ~format_spec:(make_string ()) arg ];
         "binary_1"
         >:: test "{:b}"
               [ make_field ~format_spec:(make_int ~type_:Binary ()) arg ];
         "char"
         >:: test "{:c}"
               [ make_field ~format_spec:(make_int ~type_:Char ()) arg ];
         "decimal"
         >:: test "{:d}"
               [ make_field ~format_spec:(make_int ~type_:Decimal ()) arg ];
         "octal"
         >:: test "{:o}"
               [ make_field ~format_spec:(make_int ~type_:Octal ()) arg ];
         "hex"
         >:: test "{:x}"
               [ make_field ~format_spec:(make_int ~type_:Hex ()) arg ];
         "hex_upper"
         >:: test "{:X}"
               [
                 make_field
                   ~format_spec:(make_int ~type_:Hex ~upper:true ())
                   arg;
               ];
         "scientific"
         >:: test "{:e}"
               [ make_field ~format_spec:(make_float ~type_:Scientific ()) arg ];
         "scientific_upper"
         >:: test "{:E}"
               [
                 make_field
                   ~format_spec:(make_float ~type_:Scientific ~upper:true ())
                   arg;
               ];
         "fixed"
         >:: test "{:f}"
               [ make_field ~format_spec:(make_float ~type_:Fixed ()) arg ];
         "fixed_upper"
         >:: test "{:F}"
               [
                 make_field
                   ~format_spec:(make_float ~type_:Fixed ~upper:true ())
                   arg;
               ];
         "general"
         >:: test "{:g}"
               [ make_field ~format_spec:(make_float ~type_:General ()) arg ];
         "general_upper"
         >:: test "{:G}"
               [
                 make_field
                   ~format_spec:(make_float ~type_:General ~upper:true ())
                   arg;
               ];
         "percentage"
         >:: test "{:%}"
               [ make_field ~format_spec:(make_float ~type_:Percentage ()) arg ];
       ]

let complex_tests =
  "complex"
  >::: [
         "string"
         >:: test "{:-<010s}"
               [
                 make_field
                   ~format_spec:
                     (make_string
                        ~fill:(make_fill ~char_:'-' ~width:10 Left)
                        ())
                   arg;
               ];
         "int_1"
         >:: test "{: #010_o}"
               [
                 make_field
                   ~format_spec:
                     (make_int
                        ~fill:(make_fill ~char_:'0' ~width:10 Pad)
                        ~sign:Space ~alternate_form:true
                        ~grouping_option:Underscore ~type_:Octal ())
                   arg;
               ];
         "int_2"
         >:: test "{:>>+21b}"
               [
                 make_field
                   ~format_spec:
                     (make_int
                        ~fill:(make_fill ~char_:'>' ~width:21 Right)
                        ~sign:Plus ~type_:Binary ())
                   arg;
               ];
         "int_3"
         >:: test "{:.<-32_X}"
               [
                 make_field
                   ~format_spec:
                     (make_int
                        ~fill:(make_fill ~char_:'.' ~width:32 Left)
                        ~sign:Minus ~grouping_option:Underscore ~type_:Hex
                        ~upper:true ())
                   arg;
               ];
         "float_1"
         >:: test "{: #010_.12f}"
               [
                 make_field
                   ~format_spec:
                     (make_float
                        ~fill:(make_fill ~char_:'0' ~width:10 Pad)
                        ~sign:Space ~alternate_form:true
                        ~grouping_option:Underscore ~precision:12 ~type_:Fixed
                        ())
                   arg;
               ];
         "float_2"
         >:: test "{:>>+21%}"
               [
                 make_field
                   ~format_spec:
                     (make_float
                        ~fill:(make_fill ~char_:'>' ~width:21 Right)
                        ~sign:Plus ~type_:Percentage ())
                   arg;
               ];
         "float_3"
         >:: test "{:.<-32,.1G}"
               [
                 make_field
                   ~format_spec:
                     (make_float
                        ~fill:(make_fill ~char_:'.' ~width:32 Left)
                        ~sign:Minus ~grouping_option:Comma ~precision:1
                        ~type_:General ~upper:true ())
                   arg;
               ];
       ]

let suite =
  "format_spec"
  >::: [
         fill_tests;
         sign_tests;
         alternate_form_tests;
         grouping_option_tests;
         precision_tests;
         type_tests;
         complex_tests;
       ]
