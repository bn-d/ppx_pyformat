module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper

let concat_lid : Longident.t = Lident "^"

let value_binding_of_arg ~loc:_ _index (_expr : P.expression) : P.value_binding
    =
  failwith "args input is not implemented"

let generate_format_expr ~loc ?(args = []) (str : string) : P.expression =
  let str_expr =
    str
    |> Utils.parse
    |> List.map (Element_gen.string_expr_of_element ~loc)
    |> fun exprs ->
    match exprs with
    | [] ->
        P.Location.raise_errorf ~loc
          "the string expression list cannot be empty"
    | hd :: tl ->
        List.fold_left
          (fun acc cur ->
            let concat_expr =
              Ast_helper.Exp.ident P.{ txt = concat_lid; loc }
            in
            Ast_helper.Exp.apply ~loc concat_expr
              [ (Nolabel, acc); (Nolabel, cur) ])
          hd tl
  in
  if List.length args > 0 then
    let bindings = List.mapi (value_binding_of_arg ~loc) args in
    Ast_helper.Exp.let_ Nonrecursive bindings str_expr
  else
    str_expr

let expand_string ~ctxt (expr : P.expression) : P.expression =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (str, loc, _)) -> generate_format_expr ~loc str
  | _ ->
      let loc = P.Expansion_context.Extension.extension_point_loc ctxt in
      P.Location.raise_errorf ~loc
        "the first argument must be a string constant"

let extensions =
  [
    P.Extension.V3.declare "pyformat" P.Extension.Context.Expression
      P.Ast_pattern.(single_expr_payload __)
      expand_string;
  ]

let rules : P.Context_free.Rule.t list =
  extensions |> List.map P.Context_free.Rule.extension

let () = P.Driver.register_transformation ~rules "pyformat"
