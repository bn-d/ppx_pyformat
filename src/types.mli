exception ValueError of string

type arg = Digit of int | Identifier of string list

type index = List_index of int

type align = Left | Right | Pad | Center

type sign = Plus | Minus | Space

type width = int

type fill = { char_ : char option; align : align }

type grouping_option = Comma | Underscore

type int_type = Binary | Char | Decimal | Oxtal | Hex

type float_type = Scientific | Fixed | General | Percentage

type type_ = String | Int of int_type | Float of float_type

type raw_format_spec = {
  fill : fill option;
  sign : sign option;
  alternate_form : unit option;
  zero : unit option;
  width : width option;
  grouping_option : grouping_option option;
  precision : int option;
  type_ : type_ option;
  upper : unit option;
}
[@@deriving make]

type raw_replacement_field = {
  arg : arg option;
  index : index option;
  conversion : string list option;
  format_spec : raw_format_spec option;
}
[@@deriving make]

type raw_element = Raw_text of string | Raw_field of raw_replacement_field

type raw_elements = raw_element list

type string_format_spec = { fill : (fill * width) option }

type int_format_spec = {
  type_ : int_type;
  fill : (fill * width) option;
  sign : sign option;
  alternate_form : bool;
  grouping_option : grouping_option option;
  upper : bool;
}

type float_format_spec = {
  type_ : float_type;
  fill : (fill * width) option;
  sign : sign option;
  grouping_option : grouping_option option;
  precision : int;
  upper : bool;
}

type format_spec =
  | String_format of string_format_spec
  | Int_format of int_format_spec
  | Float_format of float_format_spec

type replacement_field = {
  arg : arg;
  index : index option;
  conversion : string list option;
  format_spec : format_spec;
}
[@@deriving make]

type element = Text of string | Field of replacement_field

type elements = element list
