
%token REQUIRED
%token OPTIONAL
%token REPEATED

%token ONE_OF

%token MESSAGE

%token ENUM 

%token PACKAGE

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

%start message_list_
%type <Pbpt.message list> message_list_

%start proto_ 
%type <Pbpt.proto> proto_

%%

field_options_ : field_options EOF {$1}  
normal_field_  : normal_field  EOF {$1}
enum_value_    : enum_value    EOF {$1}
enum_          : enum          EOF {$1}
oneof_         : oneof         EOF {$1} 
message_       : message       EOF {$1} 
message_list_  : message_list  EOF {$1} 
proto_         : proto         EOF {$1} 


/*
message = "message" messageName messageBody
messageBody = "{" { field | enum | message | extend | extensions | group |
option | oneof | mapField | reserved | emptyStatement } "}"
*/

proto:
  | package_declaration message_list {Pbpt_util.proto ~package:$1 $2}
  | message_list {Pbpt_util.proto $1}

package_declaration :
  | PACKAGE IDENT SEMICOLON  {$2}  

message_list:  
  | message  {[$1]}
  | message message_list {$1::$2}

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
