open Types
module Utils = Rewriter_utils
module P = Ppxlib
module Ast_builder = Ppxlib.Ast_builder.Default
module Ast_helper = Ppxlib.Ast_helper

let expr_of_char_opt ~loc char_ : P.expression =
  char_ |> Option.value ~default:' ' |> Ast_builder.echar ~loc

let padding_of_fill = function
  | Some ({ align = Pad; char_ }, width) -> Some (char_, width)
  | _ -> None

let arg_opt_of_padding ~loc padding : (P.arg_label * P.expression) option =
  padding
  |> Option.map (fun (char_, width) ->
         let char_expr = expr_of_char_opt ~loc char_ in
         let width_expr = Ast_builder.eint ~loc width in
         let open P in
         (Labelled "padding", [%expr [%e char_expr], [%e width_expr]]))

let arg_opt_of_sign ~loc sign : (P.arg_label * P.expression) option =
  sign
  |> Option.map (fun sign ->
         let sign_expr =
           let open P in
           match sign with
           | Plus -> [%expr Ppx_pyformat_runtime.Plus]
           | Minus -> [%expr Ppx_pyformat_runtime.Minus]
           | Space -> [%expr Ppx_pyformat_runtime.Space]
         in
         (P.Labelled "sign", sign_expr))

let arg_opt_of_alternate_form ~loc alternate_form :
    (P.arg_label * P.expression) option =
  alternate_form
  |> Option.map (fun alternate_form ->
         (P.Labelled "alternate_form", Utils.expr_of_bool ~loc alternate_form))

let arg_opt_of_grouping_option ~loc grouping_option :
    (P.arg_label * P.expression) option =
  grouping_option
  |> Option.map (fun grouping_option ->
         let open P in
         let label = Labelled "grouping_option" in
         match grouping_option with
         | Comma -> (label, [%expr Ppx_pyformat_runtime.Comma])
         | Underscore -> (label, [%expr Ppx_pyformat_runtime.Underscore]))

let grouping_of_grouping_option = function
  | Some Underscore -> true
  | _ -> false

let arg_opt_of_grouping ~loc grouping : (P.arg_label * P.expression) option =
  grouping
  |> Option.map (fun grouping ->
         (P.Labelled "grouping", Utils.expr_of_bool ~loc grouping))

let arg_opt_of_upper ~loc upper : (P.arg_label * P.expression) option =
  upper
  |> Option.map (fun upper ->
         (P.Labelled "upper", Utils.expr_of_bool ~loc upper))

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
  | Some ({ align = Left; char_ }, w) -> apply_left_align ~loc char_ w expr
  | Some ({ align = Right; char_ }, w) -> apply_right_align ~loc char_ w expr
  | Some ({ align = Center; char_ }, w) -> apply_center_align ~loc char_ w expr
  | _ -> expr

(** apply functions for binary format *)
let apply_format_function
    ~loc
    ?fill
    ?padding
    ?sign
    ?alternate_form
    ?grouping_option
    ?grouping
    ?upper
    func_name
    (expr : P.expression) : P.expression =
  let fun_expr = Utils.expr_of_runtime_fun ~loc func_name in
  let padding_arg = arg_opt_of_padding ~loc padding in
  let sign_arg = arg_opt_of_sign ~loc sign in
  let alternate_form_arg = arg_opt_of_alternate_form ~loc alternate_form in
  let grouping_option_arg = arg_opt_of_grouping_option ~loc grouping_option in
  let grouping_arg = arg_opt_of_grouping ~loc grouping in
  let upper_arg = arg_opt_of_upper ~loc upper in
  [
    padding_arg;
    sign_arg;
    alternate_form_arg;
    grouping_option_arg;
    grouping_arg;
    upper_arg;
    Some (P.Nolabel, expr);
  ]
  |> List.filter Option.is_some
  |> List.map Option.get
  |> Ast_helper.Exp.apply ~loc fun_expr
  |> apply_string_format ~loc ~fill

(** apply functions for format *)
let apply_format_spec ~loc format_spec (expr : P.expression) : P.expression =
  (* all the validation should be done in type_utils, so minimal validation done here. only check related field  *)
  match format_spec with
  | String_format { fill } -> apply_string_format ~loc ~fill expr
  | Int_format
      { type_ = Binary; fill; sign; alternate_form; grouping_option; _ } ->
      let padding = padding_of_fill fill in
      let grouping = grouping_of_grouping_option grouping_option in
      apply_format_function ~loc ?fill ?padding ~sign ~alternate_form ~grouping
        "int_to_binary" expr
  | Int_format { type_ = Char; fill; _ } ->
      apply_format_function ~loc ?fill "int_to_char" expr
  | Int_format { type_ = Decimal; fill; sign; grouping_option; _ } ->
      let padding = padding_of_fill fill in
      apply_format_function ~loc ?fill ?padding ~sign ?grouping_option
        "int_to_decimal" expr
  | Int_format { type_ = Octal; fill; sign; alternate_form; grouping_option; _ }
    ->
      let padding = padding_of_fill fill in
      let grouping = grouping_of_grouping_option grouping_option in
      apply_format_function ~loc ?fill ?padding ~sign ~alternate_form ~grouping
        "int_to_octal" expr
  | Int_format
      { type_ = Hex; fill; sign; alternate_form; grouping_option; upper } ->
      let padding = padding_of_fill fill in
      let grouping = grouping_of_grouping_option grouping_option in
      apply_format_function ~loc ?fill ?padding ~sign ~alternate_form ~grouping
        ~upper "int_to_hexadecimal" expr
  | _ -> failwith "not impl"

(** generate string expression according to replacement field *)
let string_expr_of_rfield ~loc (rfield : replacement_field) : P.expression =
  (match rfield.arg with
  | Digit idx -> [ Utils.get_arg_name idx ]
  | Identifier ids -> ids)
  |> Utils.expr_of_ids ~loc
  |> apply_index ~loc rfield.index
  |> apply_conversion ~loc rfield.conversion
  |> apply_format_spec ~loc rfield.format_spec

(** generate string expression according to element *)
let string_expr_of_element ~loc (element : element) : P.expression =
  match element with
  | Text str -> Ast_builder.estring ~loc str
  | Field rfield -> string_expr_of_rfield ~loc rfield
