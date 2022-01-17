open Types
module P = Ppxlib
module Ast_builder = Ppxlib.Ast_builder.Default
module Ast_helper = Ppxlib.Ast_helper

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

let padding_of_fill (fill : fill option) =
  match fill with
  | Some { align = Pad; char_; width } -> Some (char_, width)
  | _ -> None

let arg_opt_of_padding ~loc padding : (P.arg_label * P.expression) option =
  padding
  |> Option.map (fun (char_, width) ->
         let char_expr = Ast_builder.echar ~loc char_ in
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
         (P.Labelled "alternate_form", Ast_builder.ebool ~loc alternate_form))

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
         (P.Labelled "grouping", Ast_builder.ebool ~loc grouping))

let arg_opt_of_precision ~loc precision : (P.arg_label * P.expression) option =
  precision
  |> Option.map (fun precision ->
         (P.Labelled "precision", Ast_builder.eint ~loc precision))

let arg_opt_of_upper ~loc upper : (P.arg_label * P.expression) option =
  upper
  |> Option.map (fun upper ->
         (P.Labelled "upper", Ast_builder.ebool ~loc upper))

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
      let fun_expr = expr_of_ids ~loc ids in
      let open P in
      [%expr [%e fun_expr] [%e expr]]

let apply_fill ~loc fun_name char_ width (expr : P.expression) : P.expression =
  let func_expr = expr_of_runtime_fun ~loc fun_name in
  let char_expr = Ast_builder.echar ~loc char_ in
  let width_expr = Ast_builder.eint ~loc width in
  let open P in
  [%expr [%e func_expr] [%e char_expr] [%e width_expr] [%e expr]]

let apply_string_format ~loc ~(fill : fill option) (expr : P.expression) :
    P.expression =
  match fill with
  | Some { align = Left; char_; width } ->
      apply_fill ~loc "align_left" char_ width expr
  | Some { align = Right; char_; width } ->
      apply_fill ~loc "align_right" char_ width expr
  | Some { align = Center; char_; width } ->
      apply_fill ~loc "align_center" char_ width expr
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
    ?precision
    ?upper
    func_name
    (expr : P.expression) : P.expression =
  let fun_expr = expr_of_runtime_fun ~loc func_name in
  let padding_arg = arg_opt_of_padding ~loc padding in
  let sign_arg = arg_opt_of_sign ~loc sign in
  let alternate_form_arg = arg_opt_of_alternate_form ~loc alternate_form in
  let grouping_option_arg = arg_opt_of_grouping_option ~loc grouping_option in
  let grouping_arg = arg_opt_of_grouping ~loc grouping in
  let precision_arg = arg_opt_of_precision ~loc precision in
  let upper_arg = arg_opt_of_upper ~loc upper in
  [
    padding_arg;
    sign_arg;
    alternate_form_arg;
    grouping_option_arg;
    grouping_arg;
    precision_arg;
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
  | Float_format
      { type_; fill; sign; alternate_form; grouping_option; precision; upper }
    ->
      let func_name =
        match type_ with
        | Scientific -> "float_to_scientific"
        | Fixed -> "float_to_fixed_point"
        | General -> "float_to_general"
        | Percentage -> "float_to_percentage"
      in
      let padding = padding_of_fill fill in
      apply_format_function ~loc ?fill ?padding ~sign ~alternate_form
        ?grouping_option ~precision ~upper func_name expr

(** generate string expression according to replacement field *)
let string_expr_of_rfield ~loc (rfield : replacement_field) : P.expression =
  (match rfield.arg with
  | Digit idx -> [ Utils.get_arg_name idx ]
  | Identifier ids -> ids)
  |> expr_of_ids ~loc
  |> apply_index ~loc rfield.index
  |> apply_conversion ~loc rfield.conversion
  |> apply_format_spec ~loc rfield.format_spec

(** generate string expression according to element *)
let string_expr_of_element ~loc (element : element) : P.expression =
  match element with
  | Text str -> Ast_builder.estring ~loc str
  | Field rfield -> string_expr_of_rfield ~loc rfield
