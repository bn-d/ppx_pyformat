{
open Lexing
open Parser

module Utils = Lexer_utils

exception SyntaxError of string

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
  | "{{" { STR "{" }
  | "}}" { STR "}" }
  | '{' {
    let field = Types.make_raw_replacement_field () in
    read_arg_name field lexbuf
  }
  | not_curl { STR (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_arg_name field = parse
  | digit+ {
    let arg =
      Types.Digit (int_of_string (Lexing.lexeme lexbuf)) |> Option.some
    in
    read_index { field with arg } lexbuf
  }
  | "" { read_identifier [] field lexbuf }

and read_identifier acc field = parse
  | module_ {
    let acc = (Lexer_utils.parse_module (Lexing.lexeme lexbuf))::acc in
    read_identifier acc field lexbuf
  }
  | id_ {
    let arg =
      Types.Identifier ((Lexing.lexeme lexbuf)::acc) |> Option.some
    in
    read_index { field with arg } lexbuf
  }
  | "" {
    if List.length acc > 0 then
      raise (SyntaxError "Invalid identifier")
    else (* skip read index if no field is provided *)
      read_index field lexbuf
  }

and read_index field = parse
  (* TODO implement hashtable index *)
  | '[' digit+ ']' {
    let index =
      Utils.parse_list_index (Lexing.lexeme lexbuf) |> Option.some
    in
    read_conversion { field with index } lexbuf
  }
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
  | _ { raise (Types.ValueError "unmatched '{' in format spec") }

