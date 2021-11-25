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

let default_char = Some ' '

let handle_fill fs =
  match (fs.width, fs.fill, fs.zero) with
  (* no width no fill *)
  | None, _, _ | Some 0, _, _ -> None
  (* fill will overwrite zero setting *)
  | Some w, Some f, _ ->
      let char_ = match f.char_ with None -> default_char | c -> c in
      Some ({ f with char_ }, w)
  | Some w, None, Some () ->
      let f = { char_ = Some '0'; align = Pad } in
      Some (f, w)
  (* by default, align right fill with space *)
  | Some w, None, None ->
      let f = { char_ = default_char; align = Right } in
      Some (f, w)

let sanitize_int_format_spec fs type_ =
  let fill = handle_fill fs in
  let sign = fs.sign |> Option.value ~default:Minus in
  let alternate_form = fs.alternate_form |> Option.is_some in
  let grouping_option = fs.grouping_option in
  let _ =
    if Option.is_some fs.precision then
      raise (ValueError "Precision not allowed in integer format specifier")
  in
  let upper = fs.upper |> Option.is_some in
  Int_format { type_; fill; sign; alternate_form; grouping_option; upper }

let sanitize_float_format_spec fs type_ =
  let fill = handle_fill fs in
  let sign = fs.sign |> Option.value ~default:Minus in
  let alternate_form = fs.alternate_form |> Option.is_some in
  let grouping_option = fs.grouping_option in
  (* set default precision as 4 *)
  (* TODO this is wrong *)
  let precision = fs.precision |> Option.value ~default:6 |> max 1 in
  let upper = fs.upper |> Option.is_some in
  Float_format
    { type_; fill; sign; alternate_form; grouping_option; precision; upper }

let sanitize_string_format_spec fs =
  let fill = handle_fill fs in
  let _ =
    if Option.is_some fs.sign then
      raise (ValueError "Sign not allowed in string format specifier")
  in
  let _ =
    if Option.is_some fs.alternate_form then
      raise
        (ValueError "Alternate form (#) not allowed in string format specifier")
  in
  let _ =
    match (fill, fs.zero) with
    | Some ({ align = Pad; _ }, _), _ | None, Some () ->
        raise
          (ValueError "'=' alignment not allowed in string format specifier")
    | _ -> ()
  in
  let _ =
    if Option.is_some fs.grouping_option then
      raise
        (ValueError "Grouping option not allowed in string format specifier")
  in
  let _ =
    if Option.is_some fs.precision then
      raise (ValueError "Precision not allowed in string format specifier")
  in
  let _ =
    if Option.is_some fs.upper then
      raise (ValueError "Upper not allowed in string format specifier")
  in
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
