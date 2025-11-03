(** Code generator for the [default] functions (i.e builders) *)

include Pb_codegen_plugin.S

type default_info = {
  fname: string;
  ftype: string;  (** Type of the field *)
  ftype_underlying: string;
      (** Type of the field without [option] around for optionals *)
  default_value: string;  (** Default value if not provided *)
  optional: bool;  (** Are we passing an option? *)
  rfp: Pb_codegen_ocaml_type.record_field_presence;
  bitfield_idx: int;
}

val record_field_default_info :
  Pb_codegen_ocaml_type.record_field -> default_info
(** This function returns [(field_name, field_default_value, field_type)] for a
    record field. *)
