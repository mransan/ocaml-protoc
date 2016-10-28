module T = Ocaml_types
module F = Fmt 
module E = Exception 
module L = Logger 

open Codegen_util

let default_value_of_basic_type ?field_name basic_type field_default = 
  match basic_type, field_default with 
  | T.Bt_string, None -> "\"\""
  | T.Bt_string, Some (Pbpt.Constant_string s) -> sp "\"%s\"" s 
  | T.Bt_float , None -> "0." 
  | T.Bt_float , Some (Pbpt.Constant_float f) -> string_of_float f
  | T.Bt_int   , None -> "0"
  | T.Bt_int   , Some (Pbpt.Constant_int i) -> string_of_int i
  | T.Bt_int32 , None -> "0l"
  | T.Bt_int32 , Some (Pbpt.Constant_int i) -> sp "%il" i
  | T.Bt_int64 , None -> "0L"
  | T.Bt_int64 , Some (Pbpt.Constant_int i) -> sp "%iL" i
  | T.Bt_bytes , None -> "Bytes.create 0"  
  | T.Bt_bytes , Some (Pbpt.Constant_string s) -> sp "Bytes.of_string \"%s\"" s  
  | T.Bt_bool  , None -> "false"
  | T.Bt_bool  , Some (Pbpt.Constant_bool b) -> string_of_bool b
  | _ -> E.invalid_default_value 
    ?field_name ~info:"invalid default type" ()

(* Generate the string which is the default value for a given field
   type and default information.
 *)
let default_value_of_field_type ?field_name field_type field_default = 
  match field_type with 
  | T.Ft_user_defined_type t -> 
    function_name_of_user_defined "default" t  ^ " ()"
  | T.Ft_unit          -> "()"
  | T.Ft_basic_type bt -> default_value_of_basic_type ?field_name bt field_default


(* This function returns [(field_name, field_default_value, field_type)] for 
   a record field.
 *)
let record_field_default_info record_field : (string * string * string) =  
  let { T.rf_label; T.rf_field_type; _ } = record_field in 
  let type_string = Codegen_util.string_of_record_field_type rf_field_type in  
  let field_name  = rf_label in 

  let dfvft field_type defalut_value = 
    default_value_of_field_type ~field_name field_type defalut_value 
  in

  let default_value = match rf_field_type with
    | T.Rft_nolabel (field_type, _, _) ->
      dfvft field_type None 
    | T.Rft_required (field_type, _, _, default_value) ->
      dfvft field_type default_value 
    | T.Rft_optional (field_type, _, _, default_value) -> 
      begin match default_value with
      | None   -> "None"
      | Some _ -> sp "Some (%s)" @@ dfvft field_type default_value   
      end 
    | T.Rft_repeated_field (rt, field_type, _, _, _) -> 
      begin match rt with
      | T.Rt_list -> "[]"
      | T.Rt_repeated_field -> sp "Pbrt.Repeated_field.make (%s)" (dfvft field_type None) 
      end
    | T.Rft_associative_field (at, _, _, _) -> 
      begin match at with
      | T.At_list -> "[]"
      | T.At_hashtable -> "Hashtbl.create 128"
        (* TODO 
         * This initial value could be configurable either via 
         * the default function or via a protobuf option.
         *)
      end
    | T.Rft_variant_field {T.v_constructors; _} -> 
       begin match v_constructors with
       | [] -> assert(false)
       | {T.vc_constructor; vc_field_type; _ }::_ -> 
         begin match vc_field_type with
         | T.Vct_nullary -> vc_constructor
         | T.Vct_non_nullary_constructor field_type -> 
           sp "%s (%s)" vc_constructor (dfvft field_type None) 
         end
       end 
  in
  (field_name, default_value, type_string)

let gen_default_record  ?mutable_ ?and_ {T.r_name; r_fields} sc = 

  let fields_default_info = List.map (fun r_field ->
    record_field_default_info r_field 
  ) r_fields in (* List.map *) 

  begin
  match mutable_ with
  | Some () -> ( 
    let rn = Codegen_util.mutable_record_name r_name in 
    F.line sc @@ sp "%s default_%s () : %s = {" (let_decl_of_and and_) rn rn;
    F.scope sc (fun sc -> 
      List.iter (fun (fname, fvalue, _ ) -> 
        F.line sc @@ sp "%s = %s;" fname fvalue
      ) fields_default_info; 
    );
  )
  | None -> (
    F.line sc @@ sp "%s default_%s " (let_decl_of_and and_) r_name;
    F.scope sc (fun sc ->
      List.iter (fun (fname, fvalue, ftype) ->
        F.line sc @@ sp "?%s:((%s:%s) = %s)" fname fname ftype fvalue; 
      ) fields_default_info;
      F.line sc @@ sp "() : %s  = {" r_name;
    );
    F.scope sc (fun sc -> 
      List.iter (fun (fname, _, _ ) -> 
        F.line sc @@ sp "%s;" fname
      ) fields_default_info; 
    );
  )
  end; 
  F.line sc "}"

let gen_default_variant ?and_ {T.v_name; T.v_constructors; } sc = 
  match v_constructors with
  | []     -> failwith "programmatic TODO error" 
  | {T.vc_constructor; vc_field_type; _ }::_ ->  
    let decl = let_decl_of_and and_ in 
    begin match vc_field_type with
    | T.Vct_nullary -> F.line sc @@ sp "%s default_%s (): %s = %s" decl v_name v_name vc_constructor 
    | T.Vct_non_nullary_constructor field_type -> 
      let default_value = default_value_of_field_type ~field_name:v_name field_type None in
        (* TODO need to fix the deault value *)
      F.line sc @@ sp "%s default_%s () : %s = %s (%s)" 
         decl v_name v_name vc_constructor default_value 
    end 

let gen_default_const_variant ?and_ {T.cv_name; T.cv_constructors; } sc = 
  let first_constructor_name = match cv_constructors with
    | []            -> failwith "programmatic TODO error"
    | (name, _) ::_ -> name 
  in  
  F.line sc @@ sp "%s default_%s () = (%s:%s)" 
    (let_decl_of_and and_) cv_name first_constructor_name cv_name

let gen_struct ?and_ t sc = 
  let (), has_encoded = 
    match t with 
    | {T.spec = T.Record r ; _ } ->
      (
        gen_default_record ?and_ r sc; 
        F.empty_line sc;
        gen_default_record ~mutable_:() ~and_:() r sc
      ), true 
    | {T.spec = T.Variant v; _ } -> gen_default_variant ?and_ v sc, true 
    | {T.spec = T.Const_variant v; _ } -> gen_default_const_variant v sc, true
  in
  has_encoded

 
let gen_sig_record sc {T.r_name; r_fields; } = 

  F.line sc @@ sp "val default_%s : " r_name;
  
  let fields_default_info = List.map record_field_default_info r_fields in 
  F.scope sc (fun sc -> 
    List.iter (fun (field_name, _, field_type) -> 
      F.line sc @@ sp "?%s:%s ->" field_name field_type
    ) fields_default_info;
    F.line sc "unit ->";
    F.line sc r_name;
  ); 
  let rn = r_name in 
  F.line sc @@ sp "(** [default_%s ()] is the default value for type [%s] *)" rn rn;
  ()


let gen_sig ?and_ t sc = 
  let _ = and_ in
  let f type_name =  
    F.line sc @@ sp "val default_%s : unit -> %s" type_name type_name;
    F.line sc @@ sp "(** [default_%s ()] is the default value for type [%s] *)" type_name type_name;
  in 
  let (), has_encoded = 
    match t with 
    | {T.spec = T.Record r; _} -> gen_sig_record sc r, true
    | {T.spec = T.Variant v; _} -> f v.T.v_name, true 
    | {T.spec = T.Const_variant {T.cv_name; _ ; }; _ } -> f cv_name, true
  in
  has_encoded

let ocamldoc_title = "Default values" 
