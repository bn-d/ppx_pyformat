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

(** fake align function for none *)
let align_none _ _ s = s

type align = Left | Right | Center | Pad

type fill = align * char * int

type sign = Plus | Minus | Space

type grouping_option = Comma | Underscore

(** get sign string of number *)
let sign_str_of_num zero sign num =
  match sign with
  | Plus when num >= zero -> "+"
  | Minus when num >= zero -> ""
  | Space when num >= zero -> " "
  | _ -> "-"

let sign_str_of_int = sign_str_of_num 0

let sign_str_of_float = sign_str_of_num 0.

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

(** handle grouping and fill option *)
let handle_fill_grouping fill grouping prefix num_str suffix =
  let handle_fill align_fun c w =
    let grouped_str =
      match grouping with
      | Some (gc, gw) -> insert_grouping gc gw num_str
      | None -> num_str
    in
    align_fun c w (prefix ^ grouped_str ^ suffix)
  in
  match fill with
  | Some (Left, c, w) -> handle_fill align_left c w
  | Some (Right, c, w) -> handle_fill align_right c w
  | Some (Center, c, w) -> handle_fill align_center c w
  | Some (Pad, c, w) -> (
      let num_w = w - String.length prefix - String.length suffix in
      match grouping with
      | Some (gc, gw) ->
          let filled_num_str =
            (* grouping separator only applied to {0} fill *)
            if c = '0' then
              (* max 1 for avoiding _0000 situation, so filling extra 0 *)
              let act_w =
                (num_w / (gw + 1) * gw) + max 1 (num_w mod (gw + 1))
              in
              align_right c act_w num_str |> insert_grouping gc gw
            else
              num_str |> insert_grouping gc gw |> align_right c num_w
          in
          prefix ^ filled_num_str ^ suffix
      | None -> prefix ^ align_right c num_w num_str ^ suffix)
  | None -> handle_fill align_none () ()

(** handle grouping and fill option for int string *)
let handle_int_fill_grouping fill grouping prefix num_str =
  handle_fill_grouping fill grouping prefix num_str ""

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
    ?fill
    ?(sign = Minus)
    ?(alternate_form = false)
    ?(grouping = false)
    num =
  let prefix = sign_str_of_int sign num ^ if alternate_form then "0b" else "" in
  let grouping = if grouping then Some ("_", 4) else None in
  let num_str = string_of_binary_int (abs num) in
  handle_int_fill_grouping fill grouping prefix num_str

(* since char does not take {Pad}, will dispatch align in rewriter *)
let int_to_char num = Char.chr num |> String.make 1

let int_to_decimal ?fill ?(sign = Minus) ?grouping_option num =
  let prefix = sign_str_of_int sign num in
  let grouping = grouping_config_of_grouping_option grouping_option in
  let num_str = string_of_int (abs num) in
  handle_int_fill_grouping fill grouping prefix num_str

let string_of_octal_int num = Printf.sprintf "%o" num

let int_to_octal
    ?fill
    ?(sign = Minus)
    ?(alternate_form = false)
    ?(grouping = false)
    num =
  let prefix = sign_str_of_int sign num ^ if alternate_form then "0o" else "" in
  let grouping = if grouping then Some ("_", 4) else None in
  let num_str = string_of_octal_int (abs num) in
  handle_int_fill_grouping fill grouping prefix num_str

let string_of_hexadecimal_int num = Printf.sprintf "%x" num

let int_to_hexadecimal
    ?fill
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
  handle_int_fill_grouping fill grouping prefix num_str

let string_of_scientific_float ?(precision = 6) num =
  Printf.sprintf "%.*e" precision num

let float_to_scientific
    ?fill
    ?(sign = Minus)
    ?grouping_option
    ?(precision = 6)
    ?(upper = false)
    num =
  let prefix = sign_str_of_float sign num in
  let grouping = grouping_config_of_grouping_option grouping_option in
  let num_str =
    Float.abs num |> string_of_scientific_float ~precision |> handle_upper upper
  in
  let int_str = String.sub num_str 0 1 in
  let suffix = String.sub num_str 1 (String.length num_str - 1) in
  handle_fill_grouping fill grouping prefix int_str suffix

let float_to_fixed_point _num = failwith "not impl"

let float_to_general _num = failwith "not impl"

let float_to_percentage _num = failwith "not impl"
