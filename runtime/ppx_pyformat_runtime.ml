let align_left c w s =
  let len = String.length s in
  if len >= w then
    s
  else
    s ^ String.make (w - len) c

let align_right c w s =
  let len = String.length s in
  if len >= w then
    s
  else
    String.make (w - len) c ^ s

let align_center c w s =
  let len = String.length s in
  if len >= w then
    s
  else
    let left_l = (w - len) / 2 in
    let right_l = w - len - left_l in
    String.make left_l c ^ s ^ String.make right_l c

type padding_config = char * int

type sign = Plus | Minus | Space

type grouping_option = Comma | Underscore

(** get sign string of number *)
let sign_str_of_num is_positive sign num =
  match sign with
  | Plus when is_positive num -> "+"
  | Minus when is_positive num -> ""
  | Space when is_positive num -> " "
  | _ -> "-"

let sign_str_of_int = sign_str_of_num (fun num -> num >= 0)

let sign_str_of_float = sign_str_of_num (fun num -> not (Float.sign_bit num))

let grouping_config_of_grouping_option grouping_option =
  match grouping_option with
  | Some Underscore -> Some ("_", 3)
  | Some Comma -> Some (",", 3)
  | None -> None

(** insert grouping separator into string *)
let insert_grouping separator width str =
  let l = String.length str in
  let rec impl acc index =
    if index - width <= 0 then
      String.sub str 0 index ^ separator ^ acc
    else
      let acc = String.sub str (index - width) width ^ separator ^ acc in
      impl acc (index - width)
  in
  if l <= width then str else impl (String.sub str (l - width) width) (l - width)

(** handle grouping and padding option *)
let handle_padding_grouping padding grouping prefix num_str suffix =
  let formated =
    match (padding, grouping) with
    | Some (c, w), Some (gc, gw) ->
        let num_w = w - String.length prefix - String.length suffix in
        (* grouping separator only applied to {0} fill *)
        if c = '0' then
          (* max 1 for avoiding _0000 situation, so filling extra 0 *)
          let act_w = (num_w / (gw + 1) * gw) + max 1 (num_w mod (gw + 1)) in
          align_right c act_w num_str |> insert_grouping gc gw
        else
          num_str |> insert_grouping gc gw |> align_right c num_w
    | Some (c, w), None ->
        let num_w = w - String.length prefix - String.length suffix in
        align_right c num_w num_str
    | None, Some (gc, gw) -> insert_grouping gc gw num_str
    | None, None -> num_str
  in
  prefix ^ formated ^ suffix

(** handle grouping and padding option for int string *)
let handle_int_padding_grouping pad grouping prefix num_str =
  handle_padding_grouping pad grouping prefix num_str ""

(** handle upper option *)
let handle_upper upper str = if upper then String.uppercase_ascii str else str

(** convert int to binary string. only take non-negative number *)
let string_of_binary_int num =
  let rec impl acc cur =
    if cur = 0 then
      acc
    else if cur mod 2 = 0 then
      impl ("0" ^ acc) (Int.shift_right cur 1)
    else
      impl ("1" ^ acc) (Int.shift_right cur 1)
  in
  if num = 0 then
    "0"
  else
    impl "" num

let int_to_binary
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?(grouping = false)
    num =
  let prefix = sign_str_of_int sign num ^ if alternate_form then "0b" else "" in
  let grouping = if grouping then Some ("_", 4) else None in
  let num_str = string_of_binary_int (abs num) in
  handle_int_padding_grouping padding grouping prefix num_str

(* since char does not take {Pad}, will dispatch align in rewriter *)
let int_to_char num = Char.chr num |> String.make 1

let int_to_decimal ?padding ?(sign = Minus) ?grouping_option num =
  let prefix = sign_str_of_int sign num in
  let grouping = grouping_config_of_grouping_option grouping_option in
  let num_str = string_of_int (abs num) in
  handle_int_padding_grouping padding grouping prefix num_str

let string_of_octal_int num = Printf.sprintf "%o" num

let int_to_octal
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?(grouping = false)
    num =
  let prefix = sign_str_of_int sign num ^ if alternate_form then "0o" else "" in
  let grouping = if grouping then Some ("_", 4) else None in
  let num_str = string_of_octal_int (abs num) in
  handle_int_padding_grouping padding grouping prefix num_str

let string_of_hexadecimal_int num = Printf.sprintf "%x" num

let int_to_hexadecimal
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?(grouping = false)
    ?(upper = false)
    num =
  let prefix =
    sign_str_of_int sign num
    ^ if not alternate_form then "" else if upper then "0X" else "0x"
  in
  let grouping = if grouping then Some ("_", 4) else None in
  let num_str = abs num |> string_of_hexadecimal_int |> handle_upper upper in
  handle_int_padding_grouping padding grouping prefix num_str

let is_special_float num = not (Float.is_finite num)

let handle_special_float ?padding ~sign ~upper ?(suffix = "") num =
  let prefix = sign_str_of_float sign num in
  let num_str = Float.abs num |> string_of_float |> handle_upper upper in
  handle_padding_grouping padding None prefix num_str suffix

let string_of_scientific_float ?(precision = 6) num =
  Printf.sprintf "%.*e" precision num

let float_to_scientific
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?grouping_option
    ?(precision = 6)
    ?(upper = false)
    num =
  if is_special_float num then
    handle_special_float ?padding ~sign ~upper num
  else
    let prefix = sign_str_of_float sign num in
    let grouping = grouping_config_of_grouping_option grouping_option in
    let num_str =
      Float.abs num
      |> string_of_scientific_float ~precision
      |> handle_upper upper
    in
    let int_str = String.sub num_str 0 1 in
    let fac_str = String.sub num_str 1 (String.length num_str - 1) in
    let suffix =
      if precision = 0 && alternate_form then
        "." ^ fac_str
      else
        fac_str
    in
    handle_padding_grouping padding grouping prefix int_str suffix

let string_of_fixed_point_float ?(precision = 6) num =
  Printf.sprintf "%.*f" precision num

let float_to_fixed_point_impl
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?grouping_option
    ?(precision = 6)
    ?(upper = false)
    ?(suffix = "")
    num =
  if is_special_float num then
    handle_special_float ?padding ~sign ~upper ~suffix num
  else
    let prefix = sign_str_of_float sign num in
    let grouping = grouping_config_of_grouping_option grouping_option in
    let num_str =
      Float.abs num
      |> string_of_fixed_point_float ~precision
      |> handle_upper upper
    in
    let int_str, fac_str =
      match String.split_on_char '.' num_str with
      | [ int_str ] -> (int_str, "")
      | [ int_str; fac_str ] -> (int_str, fac_str)
      | _ -> failwith "unexpected number string during format"
    in
    let suffix =
      if String.length fac_str > 0 || alternate_form then
        "." ^ fac_str ^ suffix
      else
        fac_str ^ suffix
    in
    handle_padding_grouping padding grouping prefix int_str suffix

let float_to_fixed_point
    ?padding
    ?sign
    ?alternate_form
    ?grouping_option
    ?precision
    ?upper
    num =
  float_to_fixed_point_impl ?padding ?sign ?alternate_form ?grouping_option
    ?precision ?upper num

let float_to_general
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?grouping_option
    ?(precision = 6)
    ?(upper = false)
    num =
  (* TODO speical handle for nan etc *)
  (* TODO no trailing zero *)
  let _ =
    ignore
      (padding, sign, alternate_form, grouping_option, precision, upper, num)
  in
  failwith ""

let float_to_percentage
    ?padding
    ?sign
    ?alternate_form
    ?grouping_option
    ?precision
    ?upper
    num =
  float_to_fixed_point_impl ?padding ?sign ?alternate_form ?grouping_option
    ?precision ?upper ~suffix:"%" (num *. 100.)
