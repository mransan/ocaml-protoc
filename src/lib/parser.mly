
%token REQUIRED
%token OPTIONAL
%token REPEATED

%token ONE_OF

%token MESSAGE

%token ENUM 

%token PACKAGE

%token IMPORT

%token OPTION

%token EXTENSIONS

%token EXTEND

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
%token <string> IDENT
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

field_options_   : field_options EOF {$1}  
normal_field_    : normal_field  EOF {$1}
enum_value_      : enum_value    EOF {$1}
enum_            : enum          EOF {$1}
oneof_           : oneof         EOF {$1} 
message_         : message       EOF {$1} 
proto_           : proto         EOF {$1} 
import_          : import        EOF {$1} 
file_option_     : file_option   EOF {$1} 
extension_range_list_ : extension_range_list EOF {$1}
extension_       : extension    EOF {$1}
extend_          : extend       EOF {$1}

/*
message = "message" messageName messageBody
messageBody = "{" { field | enum | message | extend | extensions | group |
option | oneof | mapField | reserved | emptyStatement } "}"
*/


proto:
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

import:
  | IMPORT STRING SEMICOLON       { Pbpt_util.import $2} 
  | IMPORT IDENT STRING SEMICOLON { 
    if $2 <> "public" 
    then raise @@ Exception.invalid_import_qualifier () 
    else Pbpt_util.import ~public:() $3
  } 

package_declaration :
  | PACKAGE IDENT SEMICOLON  {$2}  

message : 
  | MESSAGE IDENT LBRACE message_body_content_list RBRACE { 
    Pbpt_util.message ~content:$4 $2
  } 
  | MESSAGE IDENT LBRACE RBRACE { 
    Pbpt_util.message ~content:[]  $2
  } 

message_body_content_list:
  | message_body_content  { [$1] }
  | message_body_content message_body_content_list { $1::$2 }

message_body_content :
  | normal_field { Pbpt_util.message_body_field  $1 }
  | oneof        { Pbpt_util.message_body_oneof_field $1 }
  | message      { Pbpt_util.message_body_sub $1 }
  | enum         { Pbpt_util.message_body_enum $1 }
  | extension    { Pbpt_util.message_body_extension $1 }


extend : 
  | EXTEND IDENT LBRACE normal_field_list RBRACE {
    Pbpt_util.extend $2 $4 
  }
  | EXTEND IDENT LBRACE RBRACE {
    Pbpt_util.extend $2 [] 
  }

normal_field_list :
  | normal_field                   {$1 :: []}
  | normal_field normal_field_list {$1 :: $2}

extension : 
  | EXTENSIONS extension_range_list SEMICOLON {$2}  

extension_range_list : 
  | extension_range                            {$1 :: []}
  | extension_range COMMA extension_range_list {$1 :: $3} 

extension_range :
  | INT            { Pbpt_util.extension_range_single_number $1} 
  | INT IDENT INT  { 
    if $2 = "to"
    then Pbpt_util.extension_range_range $1 (`Number $3) 
    else failwith "Invalid exension range"}
  | INT IDENT IDENT{ 
    if $2 = "to" && $3 = "max" 
    then Pbpt_util.extension_range_range $1 `Max 
    else failwith "Invalid extension range"
  }

oneof :
  ONE_OF IDENT LBRACE oneof_field_list RBRACE { 
    Pbpt_util.oneof ~fields:$4 $2 
  }  

oneof_field_list :
  | oneof_field                  { [$1]   }
  | oneof_field oneof_field_list { $1::$2 } 

oneof_field : 
  | IDENT IDENT EQUAL INT field_options SEMICOLON { 
    Pbpt_util.oneof_field ~type_:$1 ~number:$4 ~options:$5 $2  
  } 
  | IDENT IDENT EQUAL INT SEMICOLON               { 
    Pbpt_util.oneof_field ~type_:$1 ~number:$4 $2  
  } 

normal_field : 
  | label IDENT IDENT EQUAL INT field_options SEMICOLON { 
    Pbpt_util.field ~label:$1 ~type_:$2 ~number:$5 ~options:$6 $3
  } 
  | label IDENT IDENT EQUAL INT SEMICOLON { 
    Pbpt_util.field ~label:$1 ~type_:$2 ~number:$5 $3 
  } 

label :
  | REQUIRED { `Required }  
  | REPEATED { `Repeated }
  | OPTIONAL { `Optional }

field_options : 
    LBRACKET field_option_list RBRACKET { $2 }; 
  | LBRACKET RBRACKET { [] }; 

field_option_list : 
  | field_option                          { [$1] } 
  | field_option COMMA field_option_list  { $1::$3 }

field_option :
  IDENT EQUAL constant { ($1, $3) } 

file_option_identifier_item :
  | IDENT                   {$1}
  | LBRACE IDENT RBRACE     {$2}

file_option_identifier : 
  | file_option_identifier_item    {$1}
  | file_option_identifier IDENT   {$1 ^ $2}

file_option :
  | OPTION file_option_identifier EQUAL constant SEMICOLON { ($2, $4) }

constant : 
  | INT        { Pbpt.Constant_int $1 }
  | FLOAT      { Pbpt.Constant_float $1 }
  | IDENT      { match $1 with 
    | "true"  -> Pbpt.Constant_bool true 
    | "false" -> Pbpt.Constant_bool false 
    | litteral -> Pbpt.Constant_litteral litteral 
  }
  | STRING     { Pbpt.Constant_string $1 }; 

enum:
  | ENUM IDENT LBRACE enum_values RBRACE {Pbpt_util.enum ~enum_values:$4 $2 } 

enum_values:
  | enum_value               { $1::[] }
  | enum_value enum_values   { $1::$2 } 

enum_value : 
  | IDENT EQUAL INT SEMICOLON { Pbpt_util.enum_value ~int_value:$3 $1 } 
%%
