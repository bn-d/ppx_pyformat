open Ppx_pyformat.Types

let string_of_align = function
  | Left -> "Left"
  | Right -> "Right"
  | Center -> "Center"
  | Pad -> "Pad"

let string_of_fill = function
  | None -> ""
  | Some ({ char_; align }, width) ->
      let char_str =
        if Option.is_some char_ then
          char_ |> Option.get |> String.make 1
        else
          ""
      in
      let align_str = string_of_align align in
      let width_str = string_of_int width in
      Printf.sprintf "({char_='%s'; align=%s}, %s)" char_str align_str width_str

let string_of_int_type = function
  | Binary -> "b"
  | Char -> "c"
  | Decimal -> "d"
  | Octal -> "o"
  | Hex -> "x"

let string_of_float_type = function
  | Scientific -> "e"
  | Fixed -> "f"
  | General -> "g"
  | Percentage -> "%"

let string_of_format_spec = function
  | String_format { fill } ->
      let fill_str = string_of_fill fill in
      Printf.sprintf "String_format {fill=%s}" fill_str
  | Int_format { type_; fill; upper; _ } ->
      let type_str =
        if upper then
          String.uppercase_ascii (string_of_int_type type_)
        else
          string_of_int_type type_
      in
      let fill_str = string_of_fill fill in
      Printf.sprintf "Int_format {type=%s; fill=%s}" type_str fill_str
  | Float_format { type_; fill; upper; _ } ->
      let type_str =
        if upper then
          String.uppercase_ascii (string_of_float_type type_)
        else
          string_of_float_type type_
      in
      let fill_str = string_of_fill fill in
      Printf.sprintf "Float_format {type=%s; fill=%s}" type_str fill_str

let string_of_element = function
  | Text str -> Printf.sprintf "{text=\"%s\"}" str
  | Field { arg; format_spec; _ } ->
      let arg_str =
        match arg with
        | Digit n -> "{digit=" ^ string_of_int n ^ "}"
        | Identifier _l -> "{id=}"
      in
      let format_spec_str = string_of_format_spec format_spec in
      Printf.sprintf "{arg=%s; format_spec=%s}" arg_str format_spec_str

let string_of_elements elements =
  elements
  |> List.map string_of_element
  |> String.concat ";\n  "
  |> fun s -> "[\n  " ^ s ^ "\n]"
