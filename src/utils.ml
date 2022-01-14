let get_arg_name idx = Printf.sprintf "_ppx_pyformat_arg_%d" idx

let parse str =
  (* reset parse state *)
  let _ = Type_utils.reset_arg_mode () in
  let lexbuf = Lexing.from_string str in

  Parser.prog Lexer.read lexbuf
