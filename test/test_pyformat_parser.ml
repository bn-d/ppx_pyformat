open Ppx_pyformat.Types

let field = make_field ~arg:(Digit 0) ()

let format_spec = String_format { fill = None }

let replacement_field = make_replacement_field ~field ~format_spec ()

let make_f = make_field

let make_rf = make_replacement_field

let test str expected _ =
  Ppx_pyformat.Utils.parse str |> OUnit2.assert_equal expected

let suite =
  let open OUnit2 in
  "parser"
  >::: [
         "simple_string"
         >:: test "123abcABC,.;' \t\n" [ Text "123abcABC,.;' \t\n" ];
         "curl_escape" >:: test "{{}}" [ Text "{}" ];
         "consolidation" >:: test "123{{}}321" [ Text "123{}321" ];
         "simple_field" >:: test "{}" [ Field replacement_field ];
         "digit_id_0" >:: test "{0}" [ Field replacement_field ];
         "digit_id_1"
         >:: test "{1}"
               [
                 Field
                   (make_rf ~field:(make_f ~arg:(Digit 1) ()) ~format_spec ());
               ];
         "digit_id_2"
         >:: test "{123}"
               [
                 Field
                   (make_rf ~field:(make_f ~arg:(Digit 123) ()) ~format_spec ());
               ];
         "id_1"
         >:: test "{pi}"
               [
                 Field
                   (make_rf
                      ~field:(make_f ~arg:(Identifier [ "pi" ]) ())
                      ~format_spec ());
               ];
         "id_2"
         >:: test "{Math.pi}"
               [
                 Field
                   (make_rf
                      ~field:(make_f ~arg:(Identifier [ "pi"; "Math" ]) ())
                      ~format_spec ());
               ];
         "id_3"
         >:: test "{Stdlib.Math.pi}"
               [
                 Field
                   (make_rf
                      ~field:
                        (make_f ~arg:(Identifier [ "pi"; "Math"; "Stdlib" ]) ())
                      ~format_spec ());
               ];
         "id_4"
         >:: test "{my_arg}"
               [
                 Field
                   (make_rf
                      ~field:(make_f ~arg:(Identifier [ "my_arg" ]) ())
                      ~format_spec ());
               ];
         (* TODO test for invalid ids *)
         (* TODO test for index *)
         (* TODO test for conversion *)
         (* TODO test for multi id *)
         (* TODO test for complex id *)
       ]
