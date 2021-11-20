open Ppx_pyformat.Types

let format_spec = String_format { fill = None }

let make_rf = make_replacement_field

let test str expected _ =
  Ppx_pyformat.Utils.parse str |> OUnit2.assert_equal expected

let test_with_error str exc _ =
  let f _ = Ppx_pyformat.Utils.parse str in
  OUnit2.assert_raises exc f

let suite =
  let open OUnit2 in
  "parser"
  >::: [
         "simple_string"
         >:: test "123abcABC,.;' \t\n" [ Text "123abcABC,.;' \t\n" ];
         "curl_escape" >:: test "{{}}" [ Text "{}" ];
         "consolidation" >:: test "123{{}}321" [ Text "123{}321" ];
         "digit_id_1"
         >:: test "{0}" [ Field (make_rf ~arg:(Digit 0) ~format_spec ()) ];
         "digit_id_2"
         >:: test "{1}" [ Field (make_rf ~arg:(Digit 1) ~format_spec ()) ];
         "digit_id_3"
         >:: test "{123}" [ Field (make_rf ~arg:(Digit 123) ~format_spec ()) ];
         "id_1"
         >:: test "{pi}"
               [ Field (make_rf ~arg:(Identifier [ "pi" ]) ~format_spec ()) ];
         "id_2"
         >:: test "{Float.pi}"
               [
                 Field
                   (make_rf ~arg:(Identifier [ "pi"; "Float" ]) ~format_spec ());
               ];
         "id_3"
         >:: test "{Stdlib.Float.pi}"
               [
                 Field
                   (make_rf
                      ~arg:(Identifier [ "pi"; "Float"; "Stdlib" ])
                      ~format_spec ());
               ];
         "id_4"
         >:: test "{my_arg}"
               [
                 Field (make_rf ~arg:(Identifier [ "my_arg" ]) ~format_spec ());
               ];
         "auto_field_1"
         >:: test "{}" [ Field (make_rf ~arg:(Digit 0) ~format_spec ()) ];
         "auto_field_2"
         >:: test "{}{}{}"
               [
                 Field (make_rf ~arg:(Digit 0) ~format_spec ());
                 Field (make_rf ~arg:(Digit 1) ~format_spec ());
                 Field (make_rf ~arg:(Digit 2) ~format_spec ());
               ];
         "auto_field_3"
         >:: test " {} {} "
               [
                 Text " ";
                 Field (make_rf ~arg:(Digit 0) ~format_spec ());
                 Text " ";
                 Field (make_rf ~arg:(Digit 0) ~format_spec ());
                 Text " ";
               ];
         (* TODO test for invalid ids *)
         (* TODO test for index *)
         (* TODO test for conversion *)
         (* TODO test for multi id *)
         (* TODO test for complex id *)
       ]
