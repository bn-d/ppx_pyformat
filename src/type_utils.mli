val reset_arg_mode : unit -> unit
(** Reset the current arg mode. *)

val set_arg_manual : unit -> unit
(** Set the current arg mode to manual. If the current mode has been already set to auto, will raise exception. *)

val get_auto_arg : unit -> Types.arg
(** Set the current arg mode to auto and generate appropriate {Types.arg}. If the current mode has been already set to manual, will raise exception.  *)

val sanitize_field : Types.raw_replacement_field -> Types.replacement_field
(** Validate and convert raw_replacement_field to replacement_field. *)
