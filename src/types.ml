exception KeyError of string

exception TypeError of string

exception ValueError of string

type arg = Digit of int | Identifier of string list

type index = List_index of int

type align = Left | Right | Pad | Center

type sign = Plus | Minus | Space

type width = int

type fill = { char_ : char option; align : align [@main] } [@@deriving make]

type grouping_option = Comma | Underscore

type int_type = Binary | Char | Decimal | Octal | Hex

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
  arg : arg;
  index : index option;
  conversion : string list option;
  format_spec : raw_format_spec option;
}
[@@deriving make]

type raw_element = Raw_text of string | Raw_field of raw_replacement_field

type raw_elements = raw_element list

type format_spec =
  | String_format of { fill : (fill * width) option }
  | Int_format of {
      type_ : int_type; [@default Decimal]
      fill : (fill * width) option;
      sign : sign; [@default Minus]
      alternate_form : bool; [@default false]
      grouping_option : grouping_option option;
      upper : bool; [@default false]
    }
  | Float_format of {
      type_ : float_type; [@default General]
      fill : (fill * width) option;
      sign : sign; [@default Minus]
      alternate_form : bool; [@default false]
      grouping_option : grouping_option option;
      (* TODO not right *)
      precision : int; [@default 6]
      upper : bool; [@default false]
    }
[@@deriving make]

type replacement_field = {
  arg : arg;
  index : index option;
  conversion : string list option;
  format_spec : format_spec;
}
[@@deriving make]

type element = Text of string | Field of replacement_field

type elements = element list
