open Types

type mode = Auto of int | Manual

let arg_mode : mode option ref = ref None

let reset_arg_mode _ = arg_mode := None

let set_arg_manual _ =
  match !arg_mode with
  | None -> arg_mode := Some Manual
  | Some Manual -> ()
  | Some (Auto _) ->
      raise
        (ValueError
           "Cannot switch from automatic field numbering to manual field \
            specification")

let get_auto_arg _ =
  match !arg_mode with
  | None ->
      let _ = arg_mode := Some (Auto 1) in
      Digit 0
  | Some (Auto n) ->
      let _ = arg_mode := Some (Auto (n + 1)) in
      Digit n
  | Some Manual ->
      raise
        (ValueError
           "Cannot switch from manual field specification to automatic field \
            numbering")

(* by default, use string *)
let default_format_spec = String_format { fill = None }

let handle_fill ~default align char_ zero width =
  let char_ = Option.value ~default:' ' char_ in
  match (width, align, zero) with
  (* no width no fill *)
  | None, _, _ | Some 0, _, _ -> None
  (* fill will overwrite zero setting *)
  | Some width, Some align, _ -> Some (make_fill ~char_ ~width align)
  | Some width, None, Some () -> Some (make_fill ~char_:'0' ~width Pad)
  | Some width, None, None -> Some (make_fill ~char_ ~width default)

let sanitize_int_format_spec raw type_ =
  (match (raw.grouping_option, type_) with
  | Some Comma, Binary | Some Comma, Octal | Some Comma, Hex ->
      raise (ValueError "Cannot specify ',' with 'b', 'o', 'x' or 'X'")
  | _, _ -> ());
  if Option.is_some raw.precision then
    raise (ValueError "Precision not allowed in integer format specifier");
  (* actual mapping *)
  let fill =
    handle_fill ~default:Right raw.align raw.char_ raw.zero raw.width
  in
  let sign = Option.value ~default:Minus raw.sign in
  let alternate_form = Option.is_some raw.alternate_form in
  let grouping_option = raw.grouping_option in
  let upper = Option.is_some raw.upper in
  Int_format { type_; fill; sign; alternate_form; grouping_option; upper }

let sanitize_float_format_spec raw type_ =
  let fill =
    handle_fill ~default:Right raw.align raw.char_ raw.zero raw.width
  in
  let sign = Option.value ~default:Minus raw.sign in
  let alternate_form = Option.is_some raw.alternate_form in
  let grouping_option = raw.grouping_option in
  let precision = Option.value ~default:6 raw.precision |> max 0 in
  let upper = Option.is_some raw.upper in
  Float_format
    { type_; fill; sign; alternate_form; grouping_option; precision; upper }

let sanitize_string_format_spec raw =
  if raw.sign = Some Space then
    raise (ValueError "Space not allowed in string format specifier");
  if Option.is_some raw.sign then
    raise (ValueError "Sign not allowed in string format specifier");
  if Option.is_some raw.alternate_form then
    raise
      (ValueError "Alternate form (#) not allowed in string format specifier");
  if
    (Option.is_some raw.align && Option.get raw.align = Pad)
    || (Option.is_none raw.align && Option.is_some raw.zero)
  then
    raise (ValueError "'=' alignment not allowed in string format specifier");
  if Option.is_some raw.grouping_option then
    raise (ValueError "Grouping option not allowed in string format specifier");
  if Option.is_some raw.precision then
    raise (ValueError "Precision not allowed in string format specifier");
  if Option.is_some raw.upper then
    raise (ValueError "Upper not allowed in string format specifier");
  (* actual mapping *)
  let fill = handle_fill ~default:Left raw.align raw.char_ None raw.width in
  String_format { fill }

let sanitize_field (raw : raw_replacement_field) : replacement_field =
  let arg = raw.arg in
  let index = raw.index in
  let conversion = raw.conversion in
  (* set default format spec if none else map according to type *)
  let format_spec =
    raw.format_spec
    |> Option.map (fun (fs : raw_format_spec) ->
           match fs.type_ with
           | Some (Int t) -> sanitize_int_format_spec fs t
           | Some (Float t) -> sanitize_float_format_spec fs t
           | Some String | None -> sanitize_string_format_spec fs)
    |> Option.value ~default:default_format_spec
  in
  { arg; index; conversion; format_spec }
