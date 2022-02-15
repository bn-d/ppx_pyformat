open Ppx_pyformat.Types

let format_spec = String_format { fill = None }

let make_field ?index ?conversion ?(format_spec = format_spec) arg =
  Field (make_replacement_field ~arg ?index ?conversion ~format_spec ())

let make_string = make_string_format_of_format_spec
let make_int = make_int_format_of_format_spec
let make_float = make_float_format_of_format_spec

let test str expected _ =
  Ppx_pyformat.Utils.parse str
  |> OUnit2.assert_equal ~printer:Printer_utils.string_of_elements expected

let test_exc str exc _ =
  let f _ = Ppx_pyformat.Utils.parse str in
  OUnit2.assert_raises exc f
