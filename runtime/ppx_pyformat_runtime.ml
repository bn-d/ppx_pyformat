let align_left w c s =
  let len = String.length s in
  if len >= w then
    s
  else
    s ^ String.make (w - len) c

let align_right w c s =
  let len = String.length s in
  if len >= w then
    s
  else
    String.make (w - len) c ^ s

let align_center w c s =
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

let binary_of_int ?fill:_ ?sign:_ ?alternate_form:_ ?grouping:_ _ = ""
