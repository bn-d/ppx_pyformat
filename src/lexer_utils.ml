open Types

let parse_module s = String.sub s 0 (String.length s - 1)

let parse_list_index s =
  String.sub s 1 (String.length s - 2)
  |> int_of_string
  |> (fun i -> List_index i)
  |> Option.some

let parse_align = function
  | "<" -> Left
  | ">" -> Right
  | "^" -> Center
  | "=" -> Pad
  | u -> raise (ValueError ("Unknown alignment '" ^ u ^ "'"))

let parse_fill s =
  match String.length s with
  | 1 -> (Some (parse_align s), None)
  | 2 -> (Some (parse_align (String.sub s 1 1)), Some s.[0])
  | _ -> raise (ValueError "Unexpected alignment string")

let parse_sign = function
  | "+" -> Some Plus
  | "-" -> Some Minus
  | " " -> Some Space
  | u -> raise (ValueError ("Unknown sign '" ^ u ^ "'"))

let parse_width s =
  let zero = if String.get s 0 = '0' then Some () else None in
  let width = Some (int_of_string s) in
  (zero, width)

let parse_grouping_option = function
  | "," -> Some Comma
  | "_" -> Some Underscore
  | u -> raise (ValueError ("Unknown grouping option '" ^ u ^ "'"))

let pasrse_precision s =
  String.sub s 1 (String.length s - 1) |> int_of_string |> Option.some

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
  let upper =
    if String.uppercase_ascii s = s && type_ <> Float Percentage then
      Some ()
    else
      None
  in
  (Some type_, upper)
