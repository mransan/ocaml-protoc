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
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
  *)
*/

%token REQUIRED
%token OPTIONAL
%token REPEATED

%token <Loc.t> ONE_OF

%token MESSAGE

%token ENUM 

%token PACKAGE

%token <Loc.t> IMPORT
%token PUBLIC

%token OPTION

%token EXTENSIONS

%token EXTEND

%token SYNTAX 

%token TO
%token MAX

%token MAP

%token RBRACE
%token LBRACE
%token RBRACKET
%token LBRACKET 
%token RPAREN  
%token LPAREN
%token RANGLEB  
%token LANGLEB
%token EQUAL
%token SEMICOLON
%token COMMA
%token <string> STRING 
%token <int>    INT
%token <float>  FLOAT
%token <Loc.t * string> IDENT
%token EOF

%start field_options_
%type <Pbpt.field_options> field_options_

%start normal_field_ 
%type <Pbpt.field_label Pbpt.field> normal_field_

%start enum_value_ 
%type <Pbpt.enum_value> enum_value_

%start enum_
%type <Pbpt.enum> enum_

%start oneof_
%type <Pbpt.oneof> oneof_

%start message_
%type <Pbpt.message> message_

%start proto_ 
%type <Pbpt.proto> proto_

%start import_ 
%type <Pbpt.import> import_

%start file_option_
%type <Pbpt.file_option> file_option_

%start extension_range_list_
%type <Pbpt.extension_range list> extension_range_list_

%start extension_
%type <Pbpt.extension_range list> extension_

%start extend_
%type <Pbpt.extend> extend_

%%

/*(* The following symbol are for internal testing only *) */ 

field_options_   : field_options EOF {$1}  
normal_field_    : normal_field  EOF {$1}
enum_value_      : enum_value    EOF {$1}
enum_            : enum          EOF {$1}
oneof_           : oneof         EOF {$1} 
message_         : message       EOF {$1} 
import_          : import        EOF {$1} 
file_option_     : file_option   EOF {$1} 
extension_range_list_ : extension_range_list EOF {$1}
extension_       : extension    EOF {$1}
extend_          : extend       EOF {$1}

/* (* Main protobuf symbol *) */

proto_           : proto         EOF {$1} 

proto:
  | syntax proto_content {Pbpt_util.proto ~syntax:$1 ~proto:$2 ()}
  | proto_content        {$1}

proto_content:
  | import              {Pbpt_util.proto ~import:$1  ()}
  | file_option         {Pbpt_util.proto ~file_option:$1  ()}
  | package_declaration {Pbpt_util.proto ~package:$1 ()}
  | message             {Pbpt_util.proto ~message:$1 ()}
  | enum                {Pbpt_util.proto ~enum:$1 ()}
  | extend              {Pbpt_util.proto ~extend:$1 ()}

  | import              proto {Pbpt_util.proto ~import:$1  ~proto:$2 ()}
  | file_option         proto {Pbpt_util.proto ~file_option:$1  ~proto:$2 ()}
  | package_declaration proto {Pbpt_util.proto ~package:$1 ~proto:$2 ()}
  | message             proto {Pbpt_util.proto ~message:$1 ~proto:$2 ()}
  | enum                proto {Pbpt_util.proto ~enum:$1 ~proto:$2 ()}
  | extend              proto {Pbpt_util.proto ~extend:$1 ~proto:$2 ()}

syntax:
  | SYNTAX EQUAL STRING semicolon { $3 }

import:
  | IMPORT STRING semicolon         { Pbpt_util.import $2} 
  | IMPORT PUBLIC STRING semicolon  { Pbpt_util.import ~public:() $3 }
  | IMPORT IDENT STRING semicolon   { Exception.invalid_import_qualifier $1 } /*HT*/ 

package_declaration :
  | PACKAGE IDENT semicolon {snd $2}  

message : 
  | MESSAGE IDENT LBRACE message_body_content_list rbrace { 
    Pbpt_util.message ~content:$4 (snd $2)
  } 
  | MESSAGE IDENT LBRACE rbrace { 
    Pbpt_util.message ~content:[]  (snd $2)
  } 

message_body_content_list:
  | message_body_content  { [$1] }
  | message_body_content message_body_content_list { $1::$2 }

message_body_content :
  | normal_field { Pbpt_util.message_body_field  $1 }
  | map          { Pbpt_util.message_body_map_field $1 }
  | oneof        { Pbpt_util.message_body_oneof_field $1 }
  | message      { Pbpt_util.message_body_sub $1 }
  | enum         { Pbpt_util.message_body_enum $1 }
  | extension    { Pbpt_util.message_body_extension $1 }
  | error        { Exception.syntax_error ()}

extend : 
  | EXTEND IDENT LBRACE normal_field_list rbrace {
    Pbpt_util.extend (snd $2) $4 
  }
  | EXTEND IDENT LBRACE rbrace {
    Pbpt_util.extend (snd $2) [] 
  }

normal_field_list :
  | normal_field                   {$1 :: []}
  | normal_field normal_field_list {$1 :: $2}

extension : 
  | EXTENSIONS extension_range_list semicolon {$2}  

extension_range_list : 
  | extension_range                            {$1 :: []}
  | extension_range COMMA extension_range_list {$1 :: $3} 

extension_range :
  | INT            { Pbpt_util.extension_range_single_number $1} 
  | INT TO INT     { Pbpt_util.extension_range_range $1 (`Number $3) } 
  | INT TO MAX     { Pbpt_util.extension_range_range $1 `Max } 

oneof :
  | ONE_OF IDENT LBRACE oneof_field_list rbrace { 
    Pbpt_util.oneof ~fields:$4 (snd $2) 
  }  
  | ONE_OF LBRACE oneof_field_list rbrace { 
    Exception.missing_one_of_name $1
  }  

oneof_field_list :
  |                                     { []   }
  | oneof_field oneof_field_list        { $1::$2 } 

oneof_field : 
  | IDENT field_name EQUAL INT field_options semicolon { 
    Pbpt_util.oneof_field ~type_:(snd $1) ~number:$4 ~options:$5 $2  
  } 
  | IDENT field_name EQUAL INT semicolon { 
    Pbpt_util.oneof_field ~type_:(snd $1) ~number:$4 $2  
  } 

map :
  | MAP LANGLEB IDENT COMMA IDENT RANGLEB field_name EQUAL INT semicolon {
    Pbpt_util.map ~key_type:(snd $3) ~value_type:(snd $5) ~number:$9 $7
  }

normal_field : 
  | label IDENT field_name EQUAL INT field_options semicolon { 
    Pbpt_util.field ~label:$1 ~type_:(snd $2) ~number:$5 ~options:$6 $3
  } 
  | label IDENT field_name EQUAL INT semicolon { 
    Pbpt_util.field ~label:$1 ~type_:(snd $2) ~number:$5 $3 
  } 
  | IDENT field_name EQUAL INT field_options semicolon { 
    Exception.missing_field_label (fst $1)
  }
  | IDENT field_name EQUAL INT semicolon { 
    Exception.missing_field_label (fst $1)
  }

field_name :
  | IDENT     {snd $1}
  | REQUIRED  {"required"}
  | OPTIONAL  {"optional"}
  | REPEATED  {"repeated"}
  | ONE_OF    {"oneof"}
  | ENUM      {"enum"}
  | PACKAGE   {"package"}
  | IMPORT    {"import"}
  | PUBLIC    {"public"}
  | OPTION    {"option"}
  | EXTENSIONS{"extensions"}
  | EXTEND    {"extend"}
  | SYNTAX    {"syntax"}
  | MESSAGE   {"message"}
  | TO        {"to"}
  | MAX       {"max"}

label :
  | REQUIRED { `Required }  
  | REPEATED { `Repeated }
  | OPTIONAL { `Optional }
  | IDENT    { Exception.invalid_field_label (fst $1) } /* HT */

field_options : 
  | LBRACKET field_option_list RBRACKET { $2 }; 
  | LBRACKET RBRACKET                   { [] }; 

field_option_list : 
  | field_option                          { [$1] } 
  | field_option COMMA field_option_list  { $1::$3 }

field_option :
  | IDENT EQUAL constant               { (snd $1, $3) } 
  | LPAREN IDENT RPAREN EQUAL constant { (snd $2, $5)} 

file_option_identifier_item :
  | IDENT                   {snd $1}
  | LPAREN IDENT RPAREN     {snd $2}

file_option_identifier : 
  | file_option_identifier_item    {$1}
  | file_option_identifier IDENT   {$1 ^ (snd $2)}

file_option :
  | OPTION file_option_identifier EQUAL constant semicolon { ($2, $4) }

constant : 
  | INT        { Pbpt.Constant_int $1 }
  | FLOAT      { Pbpt.Constant_float $1 }
  | IDENT      { 
    match (snd $1) with 
    | "true"   -> Pbpt.Constant_bool true 
    | "false"  -> Pbpt.Constant_bool false 
    | litteral -> Pbpt.Constant_litteral litteral 
  }
  | STRING     { Pbpt.Constant_string $1 }; 

enum:
  | ENUM IDENT LBRACE enum_values rbrace {Pbpt_util.enum ~enum_values:$4 (snd $2) } 

enum_values:
  |                              { [] }
  | enum_value enum_values       { $1::$2 }; 

enum_value : 
  | IDENT EQUAL INT semicolon  { Pbpt_util.enum_value ~int_value:$3 (snd $1) } 
  | IDENT EQUAL INT { 
    Exception.missing_semicolon_for_enum_value (snd $1) (fst $1) 
  }
  | IDENT EQUAL INT COMMA { Exception.invalid_enum_specification (snd $1) (fst $1)}
  | IDENT COMMA           { Exception.invalid_enum_specification (snd $1) (fst $1)}
  | IDENT SEMICOLON       { Exception.invalid_enum_specification (snd $1) (fst $1)}
  | IDENT                 { Exception.invalid_enum_specification (snd $1) (fst $1)}

semicolon:
  | SEMICOLON           {()} 
  | semicolon SEMICOLON {()} 

rbrace :
  | RBRACE           { () }
  | rbrace SEMICOLON { () }
%%
