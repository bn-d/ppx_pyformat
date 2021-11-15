let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.prog Lexer.read lexbuf
