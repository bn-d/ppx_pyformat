{
open Lexing
open Parser

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
  | digit+ { DIGIT_ARG_NAME (int_of_string (Lexing.lexeme lexbuf)); read_index lexbuf }
  | _ { read_identifier false (Lexing.lexme lexbuf) }

and read_identifier has_id = parse
  | module_ { IDENTIFIER_MODULE_NAME (Lexing.lexme lexbuf); read_identifier true lexbuf }
  | id_ { IDENTIFIER_ARG_NAME (Lexing.lexeme lexbuf); read_index lexbuf }
  | _ { if has_id then raise (SyntaxError "Invalid identifier") else read_index (Lexing.lexme lexbuf) }

  (* TODO *)
and read_index = parse
  | '}' { RIGHT }
  | _ { raise (SyntaxError "Unmatched curly brace")}


