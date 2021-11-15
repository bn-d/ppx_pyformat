let test_pure_string _ =
  Ppx_pyformat.Utils.parse "123ABCabc,./;'\t\n" |> ignore

let suite =
  let open OUnit2 in
  ("parser" >::: [
    "pure_string" >:: test_pure_string;
  ])

