open Ppx_pyformat_runtime

let _ = ignore format_string

let suite =
  let open OUnit2 in
  "test_pyformat_runtime"
  >::: [

  ]

let _ =
  let open OUnit2 in
  run_test_tt_main suite
