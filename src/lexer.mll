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
  | digit+ { read_index (Types.Digit (int_of_string (Lexing.lexeme lexbuf))) lexbuf }
  | "" { read_identifier [] lexbuf }

and read_identifier acc = parse
  | module_ {  read_identifier ((Lexing.lexeme lexbuf)::acc) lexbuf }
  | id_ { read_index (Types.Identifier ((Lexing.lexeme lexbuf)::acc)) lexbuf }
  | "" {
    if List.length acc > 0 then
      raise (SyntaxError "Invalid identifier")
    else (* set default value for field*)
      read_index (Types.Digit 0) lexbuf
  }

and read_index arg = parse
  (* TODO implement hashtable index *)
  | '[' digit+ ']' {
    let index = Utils.parser_list_index (Lexing.lexeme lexbuf) in
    let field = Types.make_field ~arg ~index () in
    read_conversion field [] lexbuf
  }
  | "" { let field = Types.make_field ~arg () in read_conversion field [] lexbuf }

and read_conversion field acc = parse
  | module_ {  read_conversion field ((Lexing.lexeme lexbuf)::acc) lexbuf }
  | id_ {
    let rp_field = Types.make_raw_replacement_field
                     ~field ~conversion:((Lexing.lexeme lexbuf)::acc) ()
    in
    read_align rp_field lexbuf
  }
  | "" {
    if List.length acc > 0 then
      raise (SyntaxError "Invalid identifier for conversion function")
    else
      let rp_field = Types.make_raw_replacement_field ~field () in
      read_align rp_field lexbuf
  }

and read_align rp_field = parse
  | "}" { FIELD rp_field }
  | _ { raise (SyntaxError "Unmatched curly brace for replacement field")}


