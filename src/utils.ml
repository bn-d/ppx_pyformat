let parse str =
  (* reset parse state *)
  let _ = Type_utils.reset_arg_mode () in
  let lexbuf = Lexing.from_string str in

  Parser.prog Lexer.read lexbuf
