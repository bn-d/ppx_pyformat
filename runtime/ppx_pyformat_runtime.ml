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

let _sign_str_of_float = sign_str_of_num 0.

(** insert grouping separator into string *)
let insert_grouping separator width str =
  let l = String.length str in
  let rec impl acc index =
    if index - width <= 0 then
      String.sub str 0 index ^ separator ^ acc
    else
      let acc = String.sub str (index - width) width ^ separator ^ acc in
      impl acc (index - 4)
  in
  if l <= width then str else impl (String.sub str (l - 4) 4) (l - 4)

let insert_undersore = insert_grouping "_" 4

let _insert_comma = insert_grouping "," 3

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

let format_binary_int
    ?fill
    ?(sign = Minus)
    ?(alternate_form = false)
    ?(grouping = false)
    num =
  let prefix =
    let sign_str = sign_str_of_int sign num in
    let af_str = if alternate_form then "0b" else "" in
    sign_str ^ af_str
  in
  let num_str = string_of_binary_int (abs num) in
  match fill with
  | Some (Left, c, w) ->
      let num_str = if grouping then insert_undersore num_str else num_str in
      align_left c w (prefix ^ num_str)
  | Some (Right, c, w) ->
      let num_str = if grouping then insert_undersore num_str else num_str in
      align_right c w (prefix ^ num_str)
  | Some (Center, c, w) ->
      let num_str = if grouping then insert_undersore num_str else num_str in
      align_center c w (prefix ^ num_str)
  | Some (Pad, c, w) ->
      let num_w = w - String.length prefix in
      if grouping then
        let filled_num_str =
          (* grouping separator only applied to '0' *)
          if c = '0' then
            (* max 1 for avoiding _0000 situation, so filling extra 0 *)
            let act_w = (num_w / 5 * 4) + max 1 (num_w mod 5) in
            align_right c act_w num_str |> insert_undersore
          else
            insert_undersore num_str |> align_right c num_w
        in
        prefix ^ filled_num_str
      else
        prefix ^ align_right c num_w num_str
  | None ->
      let num_str = if grouping then insert_undersore num_str else num_str in
      prefix ^ num_str
