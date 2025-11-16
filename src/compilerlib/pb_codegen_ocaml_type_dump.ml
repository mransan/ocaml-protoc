(*
  The MIT License (MIT)

  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)
module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

(** OCaml type representation dumping plugin *)

module PP = struct
  open Ot

  (* Helper function to convert payload_kind to string *)
  let string_of_payload_kind pk =
    match pk with
    | Pk_varint zigzag -> Printf.sprintf "Pk_varint (zigzag: %b)" zigzag
    | Pk_bits32 -> "Pk_bits32"
    | Pk_bits64 -> "Pk_bits64"
    | Pk_bytes -> "Pk_bytes"

  (* Helper function to convert basic_type to string *)
  let string_of_basic_type bt =
    match bt with
    | Bt_string -> "Bt_string"
    | Bt_float -> "Bt_float"
    | Bt_int -> "Bt_int"
    | Bt_int32 -> "Bt_int32"
    | Bt_uint32 -> "Bt_uint32"
    | Bt_int64 -> "Bt_int64"
    | Bt_uint64 -> "Bt_uint64"
    | Bt_bytes -> "Bt_bytes"
    | Bt_bool -> "Bt_bool"

  (* Helper function to convert repeated_type to string *)
  let string_of_repeated_type rt =
    match rt with
    | Rt_list -> "Rt_list"
    | Rt_repeated_field -> "Rt_repeated_field"

  (* Helper function to convert associative_type to string *)
  let string_of_associative_type at =
    match at with
    | At_list -> "At_list"
    | At_hashtable -> "At_hashtable"

  (* Helper function to convert constant to string *)
  let string_of_constant constant =
    match constant with
    | Pb_option.Constant_string s ->
      Printf.sprintf "Constant_string %S" (String.escaped s)
    | Constant_bool b -> Printf.sprintf "Constant_bool %b" b
    | Constant_int i -> Printf.sprintf "Constant_int %d" i
    | Constant_float f -> Printf.sprintf "Constant_float %f" f
    | Constant_literal s ->
      Printf.sprintf "Constant_literal %S" (String.escaped s)

  (* Helper function to convert default_value to string *)
  let string_of_default_value dv =
    match dv with
    | None -> "None"
    | Some value -> string_of_constant value

  (* Recursive function to print a record field type *)
  let rec print_record_field_type sc rf_type =
    match rf_type with
    | Rft_nolabel (ftype, enc_num, pk) ->
      F.linep sc
        "  Rft_nolabel (Field Type: %s, Encoding: %d, Payload Kind: %s)"
        (string_of_field_type ftype)
        enc_num
        (string_of_payload_kind pk)
    | Rft_required (ftype, enc_num, pk, dv) ->
      F.linep sc
        "  Rft_required (Field Type: %s, Encoding: %d, Payload Kind: %s, \
         Default Value: %s)"
        (string_of_field_type ftype)
        enc_num
        (string_of_payload_kind pk)
        (string_of_default_value dv)
    | Rft_optional (ftype, enc_num, pk, dv) ->
      F.linep sc
        "  Rft_optional (Field Type: %s, Encoding: %d, Payload Kind: %s, \
         Default Value: %s)"
        (string_of_field_type ftype)
        enc_num
        (string_of_payload_kind pk)
        (string_of_default_value dv)
    | Rft_repeated (rt, ftype, enc_num, pk, packed) ->
      F.linep sc
        "  Rft_repeated (Repeated Type: %s, Field Type: %s, Encoding: %d, \
         Payload Kind: %s, Packed: %b)"
        (string_of_repeated_type rt)
        (string_of_field_type ftype)
        enc_num
        (string_of_payload_kind pk)
        packed
    | Rft_associative (at, enc_num, (bt, pk1), (ftype, pk2)) ->
      F.linep sc
        "  Rft_associative (Associative Type: %s, Encoding: %d, Basic Type: \
         %s, Payload Kind1: %s, Field Type: %s, Payload Kind2: %s)"
        (string_of_associative_type at)
        enc_num (string_of_basic_type bt)
        (string_of_payload_kind pk1)
        (string_of_field_type ftype)
        (string_of_payload_kind pk2)
    | Rft_variant v -> F.linep sc "  Rft_variant: %s" v.v_name

  (* Helper function to convert field_type to string *)
  and string_of_field_type ft =
    match ft with
    | Ft_unit -> "Ft_unit"
    | Ft_basic_type bt -> "Ft_basic_type: " ^ string_of_basic_type bt
    | Ft_user_defined_type udt -> "Ft_user_defined_type: " ^ udt.udt_type_name
    | Ft_wrapper_type wt ->
      Printf.sprintf "Ft_wrapper_type: Basic Type: %s, Payload Kind: %s"
        (string_of_basic_type wt.wt_type)
        (string_of_payload_kind wt.wt_pk)

  (* Recursive function to print a variant *)
  let rec print_variant sc variant =
    F.linep sc "Variant: %s" variant.v_name;
    List.iter (print_variant_constructor sc) variant.v_constructors

  (* Recursive function to print a variant constructor *)
  and print_variant_constructor sc vc =
    F.linep sc "  Constructor: %s" vc.vc_constructor;
    F.linep sc "    Field Type: %s\n"
      (string_of_variant_constructor_type vc.vc_field_type);
    F.linep sc "    Encoding Number: %d, Payload Kind: %s" vc.vc_encoding_number
      (string_of_payload_kind vc.vc_payload_kind);
    F.linep sc "    Options: %s"
      (Format.asprintf "%a" Pb_option.pp_set vc.vc_options)

  (* Helper function to convert variant_constructor_type to string *)
  and string_of_variant_constructor_type vct =
    match vct with
    | Vct_nullary -> "Vct_nullary"
    | Vct_non_nullary_constructor ft ->
      "Vct_non_nullary_constructor: " ^ string_of_field_type ft

  (* Recursive function to print a record *)
  let rec print_record sc record =
    F.linep sc "Record: %s" record.r_name;
    List.iter (print_record_field sc) record.r_fields

  (* Recursive function to print a record field *)
  and print_record_field sc record_field =
    F.linep sc "- Field: %s" record_field.rf_label;
    print_record_field_type sc record_field.rf_field_type;
    F.linep sc "  Field options: %s"
      (Format.asprintf "%a" Pb_option.pp_set record_field.rf_options)

  (* Recursive function to print a const_variant *)
  let rec print_const_variant sc const_variant =
    F.linep sc "Const Variant: %s" const_variant.cv_name;
    List.iter (print_const_variant_constructor sc) const_variant.cv_constructors

  (* Recursive function to print a const_variant constructor *)
  and print_const_variant_constructor sc cvc =
    F.linep sc "  Constructor: %s" cvc.cvc_name;
    F.linep sc "    Binary Value: %d, String Value: %s" cvc.cvc_binary_value
      cvc.cvc_string_value;
    F.linep sc "    Options: %s"
      (Format.asprintf "%a" Pb_option.pp_set cvc.cvc_options)

  (* Recursive function to print the type_spec *)
  let print_type_spec sc type_spec =
    match type_spec with
    | Record record -> print_record sc record
    | Variant variant -> print_variant sc variant
    | Const_variant const_variant -> print_const_variant sc const_variant
    | Unit empty_record -> F.linep sc "Empty Record: %s" empty_record.er_name

  (* Entry point to start printing *)
  let print_type sc type_ =
    F.linep sc "Module Prefix: %s" type_.module_prefix;
    print_type_spec sc type_.spec;
    F.linep sc "Options: %s"
      (Format.asprintf "%a" Pb_option.pp_set type_.type_options);
    match type_.type_level_ppx_extension with
    | Some ext -> F.linep sc "PPX Extension: %s" ext
    | None -> ()
end

let gen_struct ?and_ ~mode:_ t sc =
  (match and_ with
  | Some _ -> ()
  | None ->
    F.line sc "(* ----------------------------------------------------- *)");
  F.line sc "(*";
  F.sub_scope sc (fun sc -> PP.print_type sc t);
  F.line sc "*)";
  true

let gen_sig ?and_ ~mode:_ t sc =
  ignore and_;
  ignore t;
  ignore sc;
  true

let ocamldoc_title = "Dump of internal representation for generated OCaml types"

let plugin : Pb_codegen_plugin.t =
  let module P = struct
    let gen_sig = gen_sig
    let gen_struct = gen_struct
    let ocamldoc_title = ocamldoc_title
  end in
  (module P)
