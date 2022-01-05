val align_left : char -> int -> string -> string

val align_right : char -> int -> string -> string

val align_center : char -> int -> string -> string

type align = Left | Right | Center | Pad

type fill = align * char * int

type sign = Plus | Minus | Space

type grouping_option = Comma | Underscore

val int_to_binary :
  ?fill:fill ->
  ?sign:sign ->
  ?alternate_form:bool ->
  ?grouping:bool ->
  int ->
  string

val int_to_char : int -> string
(** Only support ASCII at the moment *)

val int_to_decimal :
  ?fill:fill -> ?sign:sign -> ?grouping_option:grouping_option -> int -> string

val int_to_octal :
  ?fill:fill ->
  ?sign:sign ->
  ?alternate_form:bool ->
  ?grouping:bool ->
  int ->
  string

val int_to_hexadecimal :
  ?fill:fill ->
  ?sign:sign ->
  ?alternate_form:bool ->
  ?grouping:bool ->
  ?upper:bool ->
  int ->
  string

val float_to_scientific :
  ?fill:fill ->
  ?sign:sign ->
  ?alternate_form:bool ->
  ?grouping_option:grouping_option ->
  ?precision:int ->
  ?upper:bool ->
  float ->
  string

val float_to_fixed_point :
  ?fill:fill ->
  ?sign:sign ->
  ?alternate_form:bool ->
  ?grouping_option:grouping_option ->
  ?precision:int ->
  ?upper:bool ->
  float -> string

val float_to_general :
  ?fill:fill ->
  ?sign:sign ->
  ?alternate_form:bool ->
  ?grouping_option:grouping_option ->
  ?precision:int ->
  ?upper:bool ->
  float -> string

val float_to_percentage :
  ?fill:fill ->
  ?sign:sign ->
  ?alternate_form:bool ->
  ?grouping_option:grouping_option ->
  ?precision:int ->
  ?upper:bool ->
  float -> string

