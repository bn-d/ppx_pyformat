open Ppx_pyformat.Types

let test_parse str expected _ =
  Ppx_pyformat.Utils.parse str |> OUnit2.assert_equal expected

let default_field = make_field ~arg:(Digit 0) ()

let default_format_spec = String_format { fill = None }

let default_replacement_field =
  make_replacement_field ~field:default_field ~format_spec:default_format_spec
    ()

let suite =
  let open OUnit2 in
  "parser"
  >::: [
         "pure_string"
         >:: test_parse "123abcABC,.;' \t\n" [ Text "123abcABC,.;' \t\n" ];
         "curl_escape" >:: test_parse "{{}}" [ Text "{}" ];
         "consolidation" >:: test_parse "123{{}}321" [ Text "123{}321" ];
         "pure_field" >:: test_parse "{}" [ Field default_replacement_field ];
       ]
