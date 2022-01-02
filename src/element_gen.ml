open Types
module Utils = Rewriter_utils
module P = Ppxlib
module Ast_builder = Ppxlib.Ast_builder.Default
module Ast_helper = Ppxlib.Ast_helper

let apply_index ~loc index (expr : P.expression) : P.expression =
  match index with
  | None -> expr
  | Some (List_index idx) ->
      let idx_expr = Ast_builder.eint ~loc idx in
      let open P in
      [%expr List.nth [%e expr] [%e idx_expr]]

let apply_conversion ~loc conversion (expr : P.expression) : P.expression =
  match conversion with
  | None -> expr
  | Some ids ->
      let fun_expr = Utils.ident_expr_of_ids ~loc ids in
      let open P in
      [%expr [%e fun_expr] [%e expr]]

let apply_align ~loc func_lid c w (expr : P.expression) : P.expression =
  let func_expr = Ast_helper.Exp.ident ~loc P.{ txt = func_lid; loc } in
  let c_expr = Ast_builder.echar ~loc (Option.value ~default:' ' c) in
  let w_expr = Ast_builder.eint ~loc w in
  let open P in
  [%expr [%e func_expr] [%e c_expr] [%e w_expr] [%e expr]]

let apply_left_align =
  apply_align (Longident.Ldot (Lident "Ppx_pyformat_runtime", "align_left"))

let apply_right_align =
  apply_align (Longident.Ldot (Lident "Ppx_pyformat_runtime", "align_right"))

let apply_center_align =
  apply_align (Longident.Ldot (Lident "Ppx_pyformat_runtime", "align_center"))

let apply_string_format ~loc ~fill (expr : P.expression) : P.expression =
  match fill with
  | None | Some ({ align = Pad; _ }, _) -> expr
  | Some ({ align = Left; char_ }, w) -> apply_left_align ~loc char_ w expr
  | Some ({ align = Right; char_ }, w) -> apply_right_align ~loc char_ w expr
  | Some ({ align = Center; char_ }, w) -> apply_center_align ~loc char_ w expr

let apply_format_spec ~loc format_spec (expr : P.expression) : P.expression =
  (* all the validation should be done in type_utils, so do not check
   * unused fields here *)
  match format_spec with
  | String_format { fill } -> apply_string_format ~loc ~fill expr
  | _ -> failwith "not impl"
(*| Int_format spec -> apply_int_format ~loc spec expr
  | Float_format spec -> apply_float_format ~loc spec expr*)

let string_expr_of_rfield ~loc (rfield : replacement_field) : P.expression =
  (match rfield.arg with
  | Digit idx -> [ Utils.get_arg_name idx ]
  | Identifier ids -> ids)
  |> Utils.ident_expr_of_ids ~loc
  |> apply_index ~loc rfield.index
  |> apply_conversion ~loc rfield.conversion
  |> apply_format_spec ~loc rfield.format_spec

let string_expr_of_element ~loc (element : element) : P.expression =
  match element with
  | Text str -> Ast_builder.estring ~loc str
  | Field rfield -> string_expr_of_rfield ~loc rfield
