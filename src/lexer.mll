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
let module_ = upper word_char*
let id_ = lower word_char*

rule read = parse
  | "{{" { STR "{" }
  | "}}" { STR "}" }
  | '{' { read_arg_name lexbuf }
  | not_curl { STR (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_arg_name = parse
  | digit+ {
    let arg = Types.Digit (int_of_string (Lexing.lexeme lexbuf)) in
    make_field arg lexbuf
  }
  | "" { read_identifier [] lexbuf }

and read_identifier acc = parse
  | module_ {
    let acc = (Lexing.lexeme lexbuf)::acc in
    read_identifier acc lexbuf
  }
  | id_ {
    let arg = Types.Identifier ((Lexing.lexeme lexbuf)::acc) in
    make_field arg lexbuf
  }
  | "" {
    if List.length acc > 0 then
      raise (SyntaxError "Invalid identifier")
    else (* set default value for field*)
      make_field (Types.Digit 0) lexbuf
  }

and make_field arg = parse
  | "" {
    let field = Types.make_field ~arg () in
    read_index field lexbuf
  }

and read_index field = parse
  (* TODO implement hashtable index *)
  | '[' digit+ ']' {
    let index = Utils.parser_list_index (Lexing.lexeme lexbuf) in
    make_rp_field { field with index } lexbuf
  }
  | "" { make_rp_field field lexbuf }

and make_rp_field field = parse
  | "" {
    let rp_field = Types.make_raw_replacement_field ~field () in
    read_conversion rp_field lexbuf
  }

and read_conversion rp_field = parse
  | "!" { read_conversion_id rp_field [] lexbuf }
  | "" { make_format_spec rp_field lexbuf }

and read_conversion_id rp_field acc = parse
  | module_ { read_conversion_id rp_field ((Lexing.lexeme lexbuf)::acc) lexbuf }
  | id_ {
    let conversion = Some ((Lexing.lexeme lexbuf)::acc) in
    make_format_spec { rp_field with conversion } lexbuf
  }
  | "" {
    raise (SyntaxError "Invalid identifier for conversion function")
  }

and make_format_spec rp_field = parse
  | "" {
    let format_spec = Types.make_raw_format_spec () in
    read_align rp_field format_spec lexbuf
  }

and read_align rp_field format_spec = parse
  | "}" { FIELD { rp_field with format_spec = Some format_spec} }
  | _ { raise (SyntaxError "Unmatched curly brace for replacement field")}


