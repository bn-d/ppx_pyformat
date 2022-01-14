module RUtils = Rewriter_utils
module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper

let concat_lid : Longident.t = Lident "^"

let rec unwrap_args (expr : P.expression) : P.expression list =
  match expr.pexp_desc with
  | Pexp_sequence (hd, tl) -> hd :: unwrap_args tl
  | _ -> [ expr ]

let value_binding_of_arg index (expr : P.expression) : P.value_binding =
  let loc = expr.pexp_loc in
  let pat =
    index
    |> RUtils.get_arg_name
    |> (fun txt -> P.{ txt; loc })
    |> Ast_helper.Pat.var ~loc
  in
  Ast_helper.Vb.mk ~loc pat expr

let generate_format_expr ~loc ?(args = []) (str : string) : P.expression =
  let str_expr =
    str
    |> Utils.parse
    |> List.map (Element_gen.string_expr_of_element ~loc)
    |> fun exprs ->
    match exprs with
    | [] ->
        let open P in
        [%expr ""]
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
    let bindings = List.mapi value_binding_of_arg args in
    Ast_helper.Exp.let_ ~loc Nonrecursive bindings str_expr
  else
    str_expr

let expand_format ~ctxt (expr : P.expression) : P.expression =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (str, loc, _)) -> generate_format_expr ~loc str
  | Pexp_sequence
      ({ pexp_desc = Pexp_constant (Pconst_string (str, loc, _)); _ }, args) ->
      let args = args |> unwrap_args in
      generate_format_expr ~loc ~args str
  | Pexp_sequence _ ->
      let loc = P.Expansion_context.Extension.extension_point_loc ctxt in
      P.Location.raise_errorf ~loc
        "the first expression of sequence must be a string constant"
  | _ ->
      let loc = P.Expansion_context.Extension.extension_point_loc ctxt in
      P.Location.raise_errorf ~loc
        "the expression must be a string constant or a sequence that starts \
         with string constant"

let extensions =
  [
    P.Extension.V3.declare "pyformat" P.Extension.Context.Expression
      P.Ast_pattern.(single_expr_payload __)
      expand_format;
  ]

let rules : P.Context_free.Rule.t list =
  extensions |> List.map P.Context_free.Rule.extension

let () = P.Driver.register_transformation ~rules "pyformat"
