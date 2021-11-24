open Types

let parse_module s = String.sub s 0 (String.length s - 1)

let parse_list_index s = String.sub s 1 (String.length s - 2) |> int_of_string

let match_align = function
  | "<" -> Left
  | ">" -> Right
  | "^" -> Center
  | "=" -> Pad
  | _ -> raise (ValueError "invalid alignment option")

let parse_fill s =
  let char_ = s.[0] in
  let align = s.[1] |> String.make 1 |> match_align in
  make_fill ~char_ align

let parse_width s =
  let zero = if String.get s 0 = '0' then Some () else None in
  let width = Some (int_of_string s) in
  (zero, width)

let parse_type s =
  let type_ =
    match s with
    | "s" -> String
    | "b" -> Int Binary
    | "c" -> Int Char
    | "d" -> Int Decimal
    | "o" -> Int Octal
    | "x" | "X" -> Int Hex
    | "e" | "E" -> Float Scientific
    | "f" | "F" -> Float Fixed
    | "g" | "G" -> Float General
    | "%" -> Float Percentage
    | u -> raise (ValueError ("Unknown format code '" ^ u ^ "'"))
  in
  let upper = if String.uppercase_ascii s = s then Some () else None in
  (Some type_, upper)
