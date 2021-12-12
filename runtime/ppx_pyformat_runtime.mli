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

(*
val octal_of_int : ?fill:fill -> ?sign:sign -> ?alternate_form:bool -> ?grouping:bool -> int -> string

val hex_of_int : ?fill:fill -> ?sign:sign -> ?alternate_form:bool -> ?grouping:bool -> ?upper:bool -> int -> string

val scientific_of_float

val fixed_point_of_float

val general_of_float

val percentage_of_float*)
