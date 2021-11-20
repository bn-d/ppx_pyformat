open Ppx_pyformat.Types

let string_of_element = function
  | Text str -> Printf.sprintf "{text=\"%s\"}" str
  | Field { arg; _ } ->
      let arg_str =
        match arg with
        | Digit n -> "{digit=" ^ string_of_int n ^ "}"
        | Identifier _l -> "{id=}"
      in
      Printf.sprintf "{field={%s}}" arg_str

let string_of_elements elements =
  elements
  |> List.map string_of_element
  |> String.concat ";\n  "
  |> fun s -> "[\n  " ^ s ^ "\n]"
