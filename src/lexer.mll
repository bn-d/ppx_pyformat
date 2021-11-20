{
open Lexing
open Parser

module Utils = Lexer_utils

exception SyntaxError of string
exception TypeError = Types.TypeError
exception ValueError = Types.ValueError

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
  | "{" eof { raise (ValueError "single '{' encountered in format string") }
  | "}" eof { raise (ValueError "single '}' encountered in format string") }
  | "{{" { STR "{" }
  | "}}" { STR "}" }
  | '{' {
    read_arg_name lexbuf
  }
  | not_curl { STR (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_arg_name = parse
  | digit+ {
    let _ = Type_utils.set_arg_manual () in
    let arg = Types.Digit (int_of_string (Lexing.lexeme lexbuf)) in
    make_replacement_field arg lexbuf
  }
  | "" { read_identifier [] lexbuf }

and read_identifier acc = parse
  | module_ {
    let acc = (Lexer_utils.parse_module (Lexing.lexeme lexbuf))::acc in
    read_identifier acc lexbuf
  }
  | id_ {
    let _ = Type_utils.set_arg_manual () in
    let arg = Types.Identifier ((Lexing.lexeme lexbuf)::acc) in
    make_replacement_field arg lexbuf
  }
  | "" {
    if List.length acc > 0 then
      raise (SyntaxError "Invalid identifier")
    else (* if no arg provided, use auto mode *)
      let arg = Type_utils.get_auto_arg () in
      make_replacement_field arg  lexbuf
  }

and make_replacement_field arg = parse
  | "" {
    let field = Types.make_raw_replacement_field ~arg () in
    read_index field lexbuf
  }

and read_index field = parse
  | '[' digit+ ']' {
    let index =
      Utils.parse_list_index (Lexing.lexeme lexbuf) |> Option.some
    in
    read_conversion { field with index } lexbuf
  }
  | '[' _ ']' { raise (TypeError "list indices must be integers")}
  | "" { read_conversion field lexbuf }

and read_conversion field = parse
  | "!" { read_conversion_id [] field lexbuf }
  | "" { read_format_spec field lexbuf }

and read_conversion_id acc field = parse
  | module_ { read_conversion_id ((Lexing.lexeme lexbuf)::acc) field lexbuf }
  | id_ {
    let conversion = ((Lexing.lexeme lexbuf)::acc) |> Option.some in
    read_format_spec { field with conversion } lexbuf
  }
  | "" {
    raise (SyntaxError "Invalid identifier for conversion function")
  }

and read_format_spec field = parse
  | ":" {
    let format_spec = Types.make_raw_format_spec () in
    read_align format_spec field lexbuf
  }
  | "" { read_field_end field lexbuf }

and read_align format_spec field = parse
  | "}" { FIELD { field with format_spec = Some format_spec} }
  | _ { raise (SyntaxError "Unmatched curly brace for replacement field") }

and read_field_end field = parse
  | "}" { FIELD field }
  | eof { raise (ValueError "expected '}' before end of string") }
  | _ { raise (ValueError "unmatched '{' in format spec") }

