module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper

let get_arg_name idx = Printf.sprintf "_ppx_pyformat_arg_%d" idx

let expr_of_true ~loc : P.expression =
  let open P in
  [%expr true]

let expr_of_false ~loc : P.expression =
  let open P in
  [%expr false]

let expr_of_runtime_fun ~loc fun_name =
  let lid = Longident.Ldot (Lident "Ppx_pyformat_runtime", fun_name) in
  Ast_helper.Exp.ident ~loc P.{ txt = lid; loc }

let expr_of_ids ~loc ids : P.expression =
  match ids with
  | [] -> P.Location.raise_errorf ~loc "the identifier list cannot be empty"
  | hd :: tl ->
      List.fold_left
        (fun acc cur -> Longident.Ldot (acc, cur))
        (Longident.Lident hd) tl
      |> fun lid -> P.{ txt = lid; loc } |> Ast_helper.Exp.ident ~loc
