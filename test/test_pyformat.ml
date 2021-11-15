let _ =
  let open OUnit2 in
  run_test_tt_main ("test_pyformat" >::: [
    Test_pyformat_parser.suite;
  ])
