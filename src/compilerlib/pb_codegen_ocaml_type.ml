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

(** OCaml type representation *)

module Pt = Pb_parsing_parse_tree

type payload_kind =
  | Pk_varint of bool (** zigzag *)
  | Pk_bits32
  | Pk_bits64
  | Pk_bytes

type user_defined_type = {
  (* since code generated is split in multiple file (type, binary, json, ..)
     this defines the prefix for the given type, the suffix will 
     be defined by each generator *)
  udt_module_prefix : string option;

  (* OCaml type name ie not the type name in proto file *)
  udt_type_name : string; 

  (* Need to keep track of this since encoding logic in binary 
     format is quite different *)
  udt_type : [`Message | `Enum ];
}

type basic_type =
  | Bt_string
  | Bt_float
  | Bt_int
  | Bt_int32
  | Bt_int64
  | Bt_bytes
  | Bt_bool

type wrapper_type = {
  wt_type : basic_type; (* basic type being wrapped *)
  wt_pk : payload_kind; (* encoding used for the basic type *)
}

type field_type =
  | Ft_unit
  | Ft_basic_type        of basic_type
  | Ft_user_defined_type of user_defined_type
  (* New wrapper type which indicates that the corresponding ocaml
     Type should be an `option` along with the fact that it is encoded with
     special rules *)
  | Ft_wrapper_type      of wrapper_type

type default_value = Pb_option.constant option

type associative_type  =
  | At_list
  | At_hashtable
  (* Future work can include the following OCaml associative containers
  | Al_map *)

type repeated_type =
  | Rt_list
  | Rt_repeated_field

type encoding_number = int

type is_packed = bool

type record_field_type =
  | Rft_nolabel         of (field_type * encoding_number * payload_kind)
                           (* no default values in proto3 no label fields *)

  | Rft_required        of (field_type * encoding_number * payload_kind *
                            default_value)

  | Rft_optional        of (field_type * encoding_number * payload_kind *
                            default_value)

  | Rft_repeated        of (repeated_type * field_type * encoding_number *
                            payload_kind  * is_packed)

  | Rft_associative     of (associative_type           *
                              encoding_number            *
                             (basic_type * payload_kind) *
                             (field_type * payload_kind))

  | Rft_variant         of variant

and variant_constructor = {
  vc_constructor : string ;
  vc_field_type : variant_constructor_type;
  vc_encoding_number : encoding_number;
  vc_payload_kind: payload_kind;
}

and variant_constructor_type =
  | Vct_nullary
  | Vct_non_nullary_constructor of field_type

and variant = {
  v_name : string;
  v_constructors : variant_constructor list;
}

and record_field = {
  rf_label : string;
  rf_field_type : record_field_type;
  rf_mutable : bool;
}

and record = {
  r_name : string;
  r_fields : record_field list;
}

and const_variant_constructor = {
  cvc_name : string;
  cvc_binary_value : int;
  cvc_string_value : string;
}

and const_variant = {
  cv_name : string;
  cv_constructors : const_variant_constructor list;
}

and type_spec =
  | Record of record
  | Variant of variant
  | Const_variant of const_variant

type type_ = {
  module_prefix : string;
    (** code generation leads to several file/module being generated for
        a given [type_]. [module_prefix] is the common prefix for all those
        generated module and it is based on the `.proto` filename. *)
  spec : type_spec;
  type_level_ppx_extension : string option;
}
