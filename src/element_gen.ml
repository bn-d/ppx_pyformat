open Types
module Utils = Rewriter_utils
module P = Ppxlib
module Ast_builder = Ppxlib.Ast_builder.Default
module Ast_helper = Ppxlib.Ast_helper

let expr_of_char_opt ~loc char_ : P.expression =
  char_ |> Option.value ~default:' ' |> Ast_builder.echar ~loc

let arg_opt_of_fill ~loc fill : (P.arg_label * P.expression) option =
  match fill with
  | None -> None
  | Some ({ align; char_ }, width) ->
      let align_expr =
        let open P in
        match align with
        | Left -> [%expr Ppx_pyformat_runtime.Left]
        | Right -> [%expr Ppx_pyformat_runtime.Right]
        | Center -> [%expr Ppx_pyformat_runtime.Center]
        | Pad -> [%expr Ppx_pyformat_runtime.Pad]
      in
      let char_expr = expr_of_char_opt ~loc char_ in
      let width_expr = Ast_builder.eint ~loc width in
      let open P in
      Some
        ( Labelled "fill",
          [%expr [%e align_expr], [%e char_expr], [%e width_expr]] )

let arg_opt_of_sign ~loc sign : (P.arg_label * P.expression) option =
  let sign_expr =
    let open P in
    match sign with
    | Plus -> [%expr Ppx_pyformat_runtime.Plus]
    | Minus -> [%expr Ppx_pyformat_runtime.Minus]
    | Space -> [%expr Ppx_pyformat_runtime.Space]
  in
  Some (Labelled "sign", sign_expr)

let arg_opt_of_alternate_form ~loc alternate_form :
    (P.arg_label * P.expression) option =
  let alternate_form_expr =
    match alternate_form with
    | true -> Utils.expr_of_true ~loc
    | false -> Utils.expr_of_false ~loc
  in
  Some (Labelled "alternate_form", alternate_form_expr)

let arg_opt_of_underscore_grouping ~loc grouping_option =
  match grouping_option with
  | None | Some Comma -> None
  | Some Underscore -> Some (P.Labelled "grouping", Utils.expr_of_true ~loc)

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
      let fun_expr = Utils.expr_of_ids ~loc ids in
      let open P in
      [%expr [%e fun_expr] [%e expr]]

let apply_align ~loc fun_name char_ width (expr : P.expression) : P.expression =
  let func_expr = Utils.expr_of_runtime_fun ~loc fun_name in
  let char_expr = expr_of_char_opt ~loc char_ in
  let width_expr = Ast_builder.eint ~loc width in
  let open P in
  [%expr [%e func_expr] [%e char_expr] [%e width_expr] [%e expr]]

let apply_left_align = apply_align "align_left"

let apply_right_align = apply_align "align_right"

let apply_center_align = apply_align "align_center"

let apply_string_format ~loc ~fill (expr : P.expression) : P.expression =
  match fill with
  | None | Some ({ align = Pad; _ }, _) -> expr
  | Some ({ align = Left; char_ }, w) -> apply_left_align ~loc char_ w expr
  | Some ({ align = Right; char_ }, w) -> apply_right_align ~loc char_ w expr
  | Some ({ align = Center; char_ }, w) -> apply_center_align ~loc char_ w expr

let apply_binary_format
    ~loc
    ~fill
    ~sign
    ~alternate_form
    ~grouping_option
    (expr : P.expression) : P.expression =
  let fun_expr = Utils.expr_of_runtime_fun ~loc "int_to_binary" in
  let fill_arg = arg_opt_of_fill ~loc fill in
  let sign_arg = arg_opt_of_sign ~loc sign in
  let alternate_form_arg = arg_opt_of_alternate_form ~loc alternate_form in
  let grouping_arg = arg_opt_of_underscore_grouping ~loc grouping_option in
  [
    fill_arg; sign_arg; alternate_form_arg; grouping_arg; Some (P.Nolabel, expr);
  ]
  |> List.filter Option.is_some
  |> List.map Option.get
  |> fun args -> Ast_helper.Exp.apply fun_expr args

let apply_format_spec ~loc format_spec (expr : P.expression) : P.expression =
  (* all the validation should be done in type_utils, so do not check
   * unused fields here *)
  match format_spec with
  | String_format { fill } -> apply_string_format ~loc ~fill expr
  | Int_format
      { type_ = Binary; fill; sign; alternate_form; grouping_option; _ } ->
      apply_binary_format ~loc ~fill ~sign ~alternate_form ~grouping_option expr
  (*| Int_format {type_=Char;fill;_ } ->
      apply_char_format ~loc ~fill expr
    | Int_format {type_=Decimal;fill; sign; grouping_option;_} ->
      apply_decimal_format ~loc ~fill ~sign ~grouping_option expr
    | Int_format {type_=Octal;fill; sign; alternate_form; grouping_option;_} ->
      apply_octal_format ~loc ~sign ~alternate_form ~grouping_option expr
    | Int_format {type_=Hex;fill; sign; alternate_form; grouping_option; upper} ->
      apply_hexadecimal_format ~fill ~sign ~alternate_form ~grouping_option ~upper expr
  *)
  | _ -> failwith "not impl"

let string_expr_of_rfield ~loc (rfield : replacement_field) : P.expression =
  (match rfield.arg with
  | Digit idx -> [ Utils.get_arg_name idx ]
  | Identifier ids -> ids)
  |> Utils.expr_of_ids ~loc
  |> apply_index ~loc rfield.index
  |> apply_conversion ~loc rfield.conversion
  |> apply_format_spec ~loc rfield.format_spec

let string_expr_of_element ~loc (element : element) : P.expression =
  match element with
  | Text str -> Ast_builder.estring ~loc str
  | Field rfield -> string_expr_of_rfield ~loc rfield
