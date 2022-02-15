module B = Bytes

external format_int : string -> int -> string = "caml_format_int"
external bytes_unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"

external bytes_unsafe_fill :
  bytes -> int -> int -> char -> unit
  = "caml_fill_bytes"
  [@@noalloc]

external bytes_unsafe_blit_string :
  string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string"
  [@@noalloc]

external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

let align_left c w s =
  let len = String.length s in
  if len >= w then
    s
  else
    let b = B.create w in
    bytes_unsafe_blit_string s 0 b 0 len;
    bytes_unsafe_fill b len (w - len) c;
    bytes_unsafe_to_string b

let align_right c w s =
  let len = String.length s in
  if len >= w then
    s
  else
    let b = B.create w and fill_len = w - len in
    bytes_unsafe_fill b 0 fill_len c;
    bytes_unsafe_blit_string s 0 b fill_len len;
    bytes_unsafe_to_string b

let align_center c w s =
  let len = String.length s in
  if len >= w then
    s
  else
    let b = B.create w in
    let left_len = (w - len) / 2 in
    let right_len = w - len - left_len in
    bytes_unsafe_fill b 0 left_len c;
    bytes_unsafe_blit_string s 0 b left_len len;
    bytes_unsafe_fill b (left_len + len) right_len c;
    bytes_unsafe_to_string b

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

let rec string_of_binary_int_impl (b, l) cur =
  if cur = 0 then
    l
  else (
    if cur mod 2 = 0 then
      bytes_unsafe_set b (63 - l) '0'
    else
      bytes_unsafe_set b (63 - l) '1';
    string_of_binary_int_impl (b, l + 1) (Int.shift_right cur 1))

(** convert int to binary string. only take non-negative number *)
let string_of_binary_int num =
  if num = 0 then
    "0"
  else
    let b = B.create 64 in
    let l = string_of_binary_int_impl (b, 0) num in
    B.sub_string b (64 - l) l

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

let int_to_octal
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?(grouping = false)
    num =
  let prefix = sign_str_of_int sign num ^ if alternate_form then "0o" else "" in
  let grouping = if grouping then Some ("_", 4) else None in
  let num_str = format_int "%o" (abs num) in
  handle_int_padding_grouping padding grouping prefix num_str

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
  let num_str = abs num |> format_int "%x" |> handle_upper upper in
  handle_int_padding_grouping padding grouping prefix num_str

let is_special_float num = not (Float.is_finite num)

let handle_special_float ?padding ~sign ~upper ?(suffix = "") num =
  let prefix = sign_str_of_float sign num in
  let num_str = Float.abs num |> string_of_float |> handle_upper upper in
  handle_padding_grouping padding None prefix num_str suffix

(** turn string into char list  *)
let char_list_of_string s =
  let rec impl i l = if i < 0 then l else impl (i - 1) (s.[i] :: l) in
  impl (String.length s - 1) []

let remove_trailing_zero ~is_scientific remove_zero str =
  if (not remove_zero) || not (String.contains str '.') then
    str
  else
    let num_str, suffix =
      if is_scientific then
        let len = String.length str in
        (String.sub str 0 (len - 4), String.sub str (len - 4) 4)
      else
        (str, "")
    in
    let _, l =
      List.fold_left
        (fun (check, l) cur ->
          if not check then
            (false, l)
          else if cur = '0' then
            (true, l + 1)
          else if cur = '.' then
            (false, l + 1)
          else
            (false, l))
        (true, 0)
        (char_list_of_string num_str |> List.rev)
    in
    String.sub num_str 0 (String.length num_str - l) ^ suffix

let string_of_scientific_float ?(precision = 6) num =
  Printf.sprintf "%.*e" precision num

let float_to_scientific_impl
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?grouping_option
    ?(precision = 6)
    ?(upper = false)
    ~remove_zero
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
      |> remove_trailing_zero ~is_scientific:true
           (remove_zero && not alternate_form)
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

let float_to_scientific = float_to_scientific_impl ~remove_zero:false

let string_of_fixed_point_float ?(precision = 6) num =
  Printf.sprintf "%.*f" precision num

let float_to_fixed_point_impl
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?grouping_option
    ?(precision = 6)
    ?(upper = false)
    ~suffix
    ~remove_zero
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
      |> remove_trailing_zero ~is_scientific:false
           (remove_zero && not alternate_form)
    in
    let int_str, fac_str =
      match String.split_on_char '.' num_str with
      | [ int_str ] -> (int_str, "")
      | [ int_str; fac_str ] -> (int_str, fac_str)
      | _ -> ("", num_str)
    in
    let suffix =
      if String.length fac_str > 0 || alternate_form then
        "." ^ fac_str ^ suffix
      else
        fac_str ^ suffix
    in
    handle_padding_grouping padding grouping prefix int_str suffix

let float_to_fixed_point =
  float_to_fixed_point_impl ~suffix:"" ~remove_zero:false

let float_to_general
    ?padding
    ?(sign = Minus)
    ?(alternate_form = false)
    ?grouping_option
    ?(precision = 6)
    ?(upper = false)
    num =
  let precision = max precision 1 in
  let exp = Float.abs num |> Float.log10 |> Float.floor |> int_of_float in
  let format_func =
    if -4 <= exp && exp < precision then
      float_to_fixed_point_impl ~precision:(precision - 1 - exp) ~suffix:""
    else
      float_to_scientific_impl ~precision:(precision - 1)
  in
  format_func ?padding ~sign ~alternate_form ?grouping_option ~upper
    ~remove_zero:true num

let float_to_percentage
    ?padding
    ?sign
    ?alternate_form
    ?grouping_option
    ?precision
    ?upper
    num =
  float_to_fixed_point_impl ?padding ?sign ?alternate_form ?grouping_option
    ?precision ?upper ~suffix:"%" ~remove_zero:false (num *. 100.)
