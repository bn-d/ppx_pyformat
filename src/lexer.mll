{
open Lexing

open Parser
open Types

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0' - '9']
let upper = ['A' - 'Z']
let lower = ['a' - 'z']
let word_char = digit | upper | lower | '_'

let not_curl = [^ '{' '}']*
let module_ = upper word_char* '.'
let id_ = lower word_char*

rule read = parse
  | "{" eof { raise (ValueError "Single '{' encountered in format string") }
  | "}" eof { raise (ValueError "Single '}' encountered in format string") }
  | "{{" { STR "{" }
  | "}}" { STR "}" }
  | '{' {
    read_arg_name lexbuf
  }
  | not_curl { STR (lexeme lexbuf) }
  | _ { raise (ValueError ("Unexpected char: " ^ lexeme lexbuf)) }
  | eof { EOF }

and read_arg_name = parse
  | digit+ {
    let _ = Type_utils.set_arg_manual () in
    let arg = Digit (int_of_string (lexeme lexbuf)) in
    make_replacement_field arg lexbuf
  }
  | "" { read_identifier [] lexbuf }

and read_identifier acc = parse
  | module_ {
    let acc = (Lexer_utils.parse_module (lexeme lexbuf))::acc in
    read_identifier acc lexbuf
  }
  | id_ {
    let _ = Type_utils.set_arg_manual () in
    let arg = Identifier ((lexeme lexbuf)::acc) in
    make_replacement_field arg lexbuf
  }
  | "" {
    if List.length acc > 0 then
      raise (KeyError "Invalid identifier")
    else (* if no arg provided, use auto mode *)
      let arg = Type_utils.get_auto_arg () in
      make_replacement_field arg  lexbuf
  }

and make_replacement_field arg = parse
  | "" {
    let field = make_raw_replacement_field ~arg () in
    read_index field lexbuf
  }

and read_index field = parse
  | '[' digit+ ']' {
    let index = Lexer_utils.parse_list_index (lexeme lexbuf) in
    read_conversion { field with index } lexbuf
  }
  | '[' not_curl ']' { raise (TypeError "List indices must be integers") }
  | "" { read_conversion field lexbuf }

and read_conversion field = parse
  | "!" { read_conversion_id [] field lexbuf }
  | "" { read_format_spec field lexbuf }

and read_conversion_id acc field = parse
  | module_ {
    let acc = (Lexer_utils.parse_module (lexeme lexbuf))::acc in
    read_conversion_id acc field lexbuf
  }
  | id_ {
    let conversion = ((lexeme lexbuf)::acc) |> Option.some in
    read_format_spec { field with conversion } lexbuf
  }
  | "" {
    raise (KeyError "Invalid identifier for conversion function")
  }

and read_format_spec field = parse
  | ":" {
    let format_spec = make_raw_format_spec () in
    read_fill format_spec field lexbuf
  }
  | "" { read_field_end field lexbuf }

and read_fill format_spec field = parse
  | _ ['<' '>' '=' '^'] {
    let fill = Lexer_utils.parse_fill (lexeme lexbuf) |> Option.some in
    read_sign { format_spec with fill } field lexbuf
  }
  (* with no fill char *)
  | ['<' '>' '=' '^'] {
    let fill =
      make_fill (Lexer_utils.parse_align (lexeme lexbuf)) |> Option.some
    in
    read_sign { format_spec with fill } field lexbuf
  }
  | "" { read_sign format_spec field lexbuf }

and read_sign format_spec field = parse
  | ['+' '-' ' '] {
    let sign = Lexer_utils.parse_sign (lexeme lexbuf) in
    read_alternate_form { format_spec with sign } field lexbuf
  }
  | "" { read_alternate_form format_spec field lexbuf }

and read_alternate_form format_spec field = parse
  | '#' {
    let alternate_form = Some () in
    read_width { format_spec with alternate_form } field lexbuf
  }
  | "" { read_width format_spec field lexbuf }

and read_width format_spec field = parse
  | digit+ {
    let zero, width = Lexer_utils.parse_width (lexeme lexbuf) in
    read_grouping_option { format_spec with zero; width } field lexbuf
  }
  | "" { read_grouping_option format_spec field lexbuf }

and read_grouping_option format_spec field = parse
  | [',' '_'] {
    let grouping_option =
      Lexer_utils.parse_grouping_option (lexeme lexbuf)
    in
    read_precision { format_spec with grouping_option } field lexbuf
  }
  | "" { read_precision format_spec field lexbuf }

and read_precision format_spec field = parse
  | '.' digit+ {
    let precision = Lexer_utils.pasrse_precision (lexeme lexbuf) in
    read_format_type { format_spec with precision } field lexbuf
  }
  | '.' {
    raise (ValueError "Format specifier missing precision")
  }
  | "" { read_format_type format_spec field lexbuf }

and read_format_type format_spec field = parse
  | ['s' 'b' 'c' 'd' 'o' 'x' 'X' 'e' 'E' 'f' 'F' 'g' 'G' '%'] {
    let type_, upper = Lexer_utils.parse_type (lexeme lexbuf) in
    end_format_spec { format_spec with type_; upper } field lexbuf
  }
  | "" { end_format_spec format_spec field lexbuf }

and end_format_spec format_spec field = parse
  | "" {
    let format_spec = Some format_spec in
    read_field_end { field with format_spec } lexbuf
  }

and read_field_end field = parse
  | "}" { FIELD field }
  | eof { raise (ValueError "Expected '}' before end of string") }
  | _ {
    let exc =
    if Option.is_some field.index || Option.is_some field.conversion || Option.is_some field.format_spec then
      ValueError "Unmatched '{' in format spec"
    else ValueError "Invalid specifier"
    in
    raise exc
  }

