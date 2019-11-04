/*(*
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
  IMPLIED, INCLUDING BUT NOT LIMITED T_to THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, T_toRT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
  *)

*/

/*Tokens*/

%token T_required
%token T_optional
%token T_repeated
%token <Pb_location.t> T_one_of
%token T_message
%token T_enum
%token T_package
%token <Pb_location.t> T_import
%token T_public
%token T_option
%token T_extensions
%token T_extend
%token T_reserved
%token T_syntax
%token T_to
%token T_max
%token T_map
%token T_rbrace
%token T_lbrace
%token T_rbracket
%token T_lbracket
%token T_rparen
%token T_lparen
%token T_greater
%token T_less
%token T_equal
%token T_semi
%token T_comma
%token <string> T_string
%token <int>    T_int
%token <float>  T_float
%token <Pb_location.t * string> T_ident
%token T_eof

/*Entry points*/

%start field_options_
%type <Pb_option.set> field_options_
%start normal_field_
%type <Pb_parsing_parse_tree.message_field> normal_field_
%start enum_value_
%type <Pb_parsing_parse_tree.enum_body_content> enum_value_
%start enum_
%type <Pb_parsing_parse_tree.enum> enum_
%start oneof_
%type <Pb_parsing_parse_tree.oneof> oneof_
%start message_
%type <Pb_parsing_parse_tree.message> message_
%start proto_
%type <Pb_parsing_parse_tree.proto> proto_
%start import_
%type <Pb_parsing_parse_tree.import> import_
%start option_
%type <Pb_option.t> option_
%start extension_range_list_
%type <Pb_parsing_parse_tree.extension_range list> extension_range_list_
%start extension_
%type <Pb_parsing_parse_tree.extension_range list> extension_
%start reserved_
%type <Pb_parsing_parse_tree.extension_range list> reserved_

%%

/*Rules*/


/*(* The following symbol are for internal testing only *) */

field_options_   : field_options T_eof {$1}
normal_field_    : normal_field  T_eof {$1}
enum_value_      : enum_value    T_eof {$1}
enum_            : enum          T_eof {$1}
oneof_           : oneof         T_eof {$1}
message_         : message       T_eof {$1}
import_          : import        T_eof {$1}
option_          : option        T_eof {$1}
extension_range_list_ : extension_range_list T_eof {$1}
extension_       : extension     T_eof {$1}
reserved_        : reserved      T_eof {$1}

/* (* Main protobuf symbol *) */

proto_           : proto         T_eof {$1}

proto:
  | syntax proto_content {Pb_parsing_util.proto ~syntax:$1 ~proto:$2 ()}
  | proto_content        {$1}

proto_content:
  | import              {Pb_parsing_util.proto ~import:$1  ()}
  | option              {Pb_parsing_util.proto ~file_option:$1  ()}
  | package_declaration {Pb_parsing_util.proto ~package:$1 ()}
  | message             {Pb_parsing_util.proto ~message:$1 ()}
  | enum                {Pb_parsing_util.proto ~enum:$1 ()}
  | extend              {Pb_parsing_util.proto ~extend:$1 ()}

  | import              proto {Pb_parsing_util.proto ~import:$1  ~proto:$2 ()}
  | option              proto {Pb_parsing_util.proto ~file_option:$1  ~proto:$2 ()}
  | package_declaration proto {Pb_parsing_util.proto ~package:$1 ~proto:$2 ()}
  | message             proto {Pb_parsing_util.proto ~message:$1 ~proto:$2 ()}
  | enum                proto {Pb_parsing_util.proto ~enum:$1 ~proto:$2 ()}
  | extend              proto {Pb_parsing_util.proto ~extend:$1 ~proto:$2 ()}

syntax:
  | T_syntax T_equal T_string semicolon { $3 }

import:
  | T_import T_string semicolon         { Pb_parsing_util.import $2}
  | T_import T_public T_string semicolon  { Pb_parsing_util.import ~public:() $3 }
  | T_import T_ident T_string semicolon   { Pb_exception.invalid_import_qualifier $1 } /*HT*/

package_declaration :
  | T_package T_ident semicolon {snd $2}

message :
  | T_message T_ident T_lbrace message_body_content_list rbrace {
    Pb_parsing_util.message ~content:$4 (snd $2)
  }
  | T_message T_ident T_lbrace rbrace {
    Pb_parsing_util.message ~content:[]  (snd $2)
  }

message_body_content_list:
  | message_body_content  { [$1] }
  | message_body_content message_body_content_list { $1::$2 }

message_body_content :
  | normal_field { Pb_parsing_util.message_body_field  $1 }
  | map          { Pb_parsing_util.message_body_map_field $1 }
  | oneof        { Pb_parsing_util.message_body_oneof_field $1 }
  | message      { Pb_parsing_util.message_body_sub $1 }
  | enum         { Pb_parsing_util.message_body_enum $1 }
  | extension    { Pb_parsing_util.message_body_extension $1 }
  | reserved     { Pb_parsing_util.message_body_reserved $1 }
  | option       { Pb_parsing_util.message_body_option $1 }

extend :
  | T_extend T_ident T_lbrace normal_field_list rbrace {
    Pb_parsing_util.extend (snd $2) $4
  }
  | T_extend T_ident T_lbrace rbrace {
    Pb_parsing_util.extend (snd $2) []
  }

normal_field_list :
  | normal_field                   {$1 :: []}
  | normal_field normal_field_list {$1 :: $2}

extension :
  | T_extensions extension_range_list semicolon {$2}

reserved :
  | T_reserved extension_range_list semicolon {$2}
/* T_toDO: incomplete, reserved field can also be defined by field names */

extension_range_list :
  | extension_range                            {$1 :: []}
  | extension_range T_comma extension_range_list {$1 :: $3}

extension_range :
  | T_int { Pb_parsing_util.extension_range_single_number $1}
  | T_int T_to T_int { Pb_parsing_util.extension_range_range $1 (`Number $3) }
  | T_int T_to T_max { Pb_parsing_util.extension_range_range $1 `Max }

oneof :
  | T_one_of field_name T_lbrace oneof_field_list rbrace {
    Pb_parsing_util.oneof ~fields:$4 $2
  }
  | T_one_of T_lbrace oneof_field_list rbrace {
    Pb_exception.missing_one_of_name $1
  }

oneof_field_list :
  |                                     { []   }
  | oneof_field oneof_field_list        { $1::$2 }

oneof_field :
  | T_ident field_name T_equal T_int field_options semicolon {
    Pb_parsing_util.oneof_field ~type_:(snd $1) ~number:$4 ~options:$5 $2
  }
  | T_ident field_name T_equal T_int semicolon {
    Pb_parsing_util.oneof_field ~type_:(snd $1) ~number:$4 $2
  }

map :
  | T_map T_less T_ident T_comma T_ident T_greater field_name T_equal T_int semicolon {
    Pb_parsing_util.map_field
        ~key_type:(snd $3) ~value_type:(snd $5) ~number:$9 $7
  }
  | T_map T_less T_ident T_comma T_ident T_greater field_name T_equal T_int field_options semicolon {
    Pb_parsing_util.map_field
        ~options:$10 ~key_type:(snd $3) ~value_type:(snd $5) ~number:$9 $7
  }

normal_field :
  | label T_ident field_name T_equal T_int field_options semicolon {
    Pb_parsing_util.field ~label:$1 ~type_:(snd $2) ~number:$5 ~options:$6 $3
  }
  | label T_ident field_name T_equal T_int semicolon {
    Pb_parsing_util.field ~label:$1 ~type_:(snd $2) ~number:$5 $3
  }
  | T_ident field_name T_equal T_int field_options semicolon {
    Pb_parsing_util.field
        ~label:`Nolabel ~type_:(snd $1) ~number:$4 ~options:$5 $2
  }
  | T_ident field_name T_equal T_int semicolon {
    Pb_parsing_util.field ~label:`Nolabel ~type_:(snd $1) ~number:$4 $2
  }

field_name :
  | T_ident     {snd $1}
  | T_required  {"required"}
  | T_optional  {"optional"}
  | T_repeated  {"repeated"}
  | T_one_of    {"oneof"}
  | T_enum      {"enum"}
  | T_package   {"package"}
  | T_import    {"import"}
  | T_public    {"public"}
  | T_option    {"option"}
  | T_extensions{"extensions"}
  | T_extend    {"extend"}
  | T_reserved  {"reserved"}
  | T_syntax    {"syntax"}
  | T_message   {"message"}
  | T_to        {"to"}
  | T_max       {"max"}
  | T_map       {"map"}

label :
  | T_required { `Required }
  | T_repeated { `Repeated }
  | T_optional { `Optional }

field_options :
  | T_lbracket field_option_list T_rbracket { $2 };
  | T_lbracket T_rbracket                   { Pb_option.empty };

field_option_list :
  | field_option                          {
    let option_name, option_value = $1 in
    Pb_option.add Pb_option.empty option_name option_value
  }
  | field_option T_comma field_option_list  {
    Pb_option.add $3 (fst $1) (snd $1)
  }

field_option :
  | T_ident T_equal constant               { (snd $1, $3) }
  | T_lparen T_ident T_rparen T_equal constant { (snd $2, $5)}

option_identifier_item :
  | T_ident                   {snd $1}
  | T_lparen T_ident T_rparen     {snd $2}

option_identifier :
  | option_identifier_item    {$1}
  | option_identifier T_ident   {$1 ^ (snd $2)}

option :
  | T_option option_identifier T_equal constant semicolon { ($2, $4) }

constant :
  | T_int        { Pb_option.Constant_int $1 }
  | T_float      { Pb_option.Constant_float $1 }
  | T_ident      {
    match (snd $1) with
    | "true"   -> Pb_option.Constant_bool true
    | "false"  -> Pb_option.Constant_bool false
    | litteral -> Pb_option.Constant_litteral litteral
  }
  | T_string     { Pb_option.Constant_string $1 };

enum:
  | T_enum T_ident T_lbrace enum_values rbrace { Pb_parsing_util.enum ~enum_body:$4 (snd $2) }

enum_values:
  |                                { [] }
  | enum_body_content enum_values  { $1::$2 }

enum_body_content :
  | option     { Pb_parsing_util.enum_option $1 }
  | enum_value { $1 }

enum_value :
  | T_ident T_equal T_int semicolon  { Pb_parsing_util.enum_value ~int_value:$3 (snd $1) }
  | T_ident T_equal T_int field_options semicolon  { Pb_parsing_util.enum_value ~int_value:$3 (snd $1) }
  | T_ident T_equal T_int {
    Pb_exception.missing_semicolon_for_enum_value (snd $1) (fst $1)
  }
  | T_ident T_equal T_int T_comma { Pb_exception.invalid_enum_specification (snd $1) (fst $1)}
  | T_ident T_comma           { Pb_exception.invalid_enum_specification (snd $1) (fst $1)}
  | T_ident T_semi       { Pb_exception.invalid_enum_specification (snd $1) (fst $1)}
  | T_ident                 { Pb_exception.invalid_enum_specification (snd $1) (fst $1)}

semicolon:
  | T_semi           {()}
  | semicolon T_semi {()}

rbrace :
  | T_rbrace           { () }
  | rbrace T_semi { () }
%%
