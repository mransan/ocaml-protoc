module E = Pb_exception
module Pt = Pb_parsing_parse_tree

let run proto =
  let maybe_protos =
    try
      Pb_parsing.parse_file
        (fun f ->
          match f with
          | "test.proto" -> f, proto
          | _ -> f, "")
        "test.proto"
      |> Result.ok
    with e -> Error e
  in
  let pp_maybe_protos ppf = function
    | Ok protos ->
      Format.fprintf ppf "%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@.")
           Pt.pp_proto)
        protos
    | Error exn -> Format.fprintf ppf "[!] EXN: %s" (Printexc.to_string exn)
  in
  let ppf = Format.std_formatter in
  Format.set_margin 150;
  Format.fprintf ppf
    "====================== <PROTO> \
     ======================@.%a@.===================== </PROTO> \
     ======================@.======================= <AST> \
     =======================@.%a@.====================== </AST> \
     =======================@.@.@."
    Format.pp_print_string proto pp_maybe_protos maybe_protos

let test_cases =
  [
    {|
      // Syntax error reporting check #1
      syntax = "proto3"!#$%
    |};
    {|
      // Syntax error reporting check #2
      syntax = "proto3" , message int32 { }
    |};
    {|
      syntax = "proto3";

      message SimpleMessage {
        int32 id = 1;
        string name = 2;
        bool active = 3;
      }
    |};
    {|
      syntax = "proto3";

      message NestedMessage {
        int32 id = 1;
        string description = 2;
      }

      message ComplexMessage {
        string title = 1;
        repeated NestedMessage items = 2;
      }
    |};
    {|
      syntax = "proto3";

      enum Status {
        UNKNOWN = 0;
        ACTIVE = 1;
        INACTIVE = 2;
      }

      message SubMessage {
        string content = 1;
      }

      message DataMessage {
        oneof data {
          string text = 1;
          int32 number = 2;
          SubMessage sub = 3;
        }
        Status status = 4;
      }
    |};
    {|
      syntax = "proto3";
  
      message OptionMessage {
        option (my_option) = true;
  
        int32 id = 1 [(my_field_option) = "value"];
        string name = 2;
  
        map<string, int32> scores = 3;
      }
    |};
    {|
      syntax = "proto3";
      
      service MyService {
        rpc UnaryRPC (UnaryRequest) returns (UnaryResponse);
        rpc ServerStreamingRPC (ServerStreamingRequest) returns (stream ServerStreamingResponse);
        rpc ClientStreamingRPC (stream ClientStreamingRequest) returns (ClientStreamingResponse);
        rpc BidirectionalStreamingRPC (stream BidirectionalRequest) returns (stream BidirectionalResponse);
      }
      
      message UnaryRequest {
        string input = 1;
      }
      
      message UnaryResponse {
        string output = 1;
      }
      
      message ServerStreamingRequest {
        string query = 1;
      }
      
      message ServerStreamingResponse {
        repeated string results = 1;
      }
      
      message ClientStreamingRequest {
        repeated int32 values = 1;
      }
      
      message ClientStreamingResponse {
        int32 sum = 1;
      }
      
      message BidirectionalRequest {
        string message = 1;
      }
      
      message BidirectionalResponse {
        string reply = 1;
      }
    |};
    {|
      syntax = "proto2";
      message Foo {
        optional int32 x = 1 [default=1];
        optional int32 y = 1 [default=1] ; ; ;; 
        optional .M1 z = 1;
      }
    |};
    {|
      syntax = "proto3";
      message Bar {
        oneof foo {string name = 4;SubMessage sub_message = 9 [a=1];}
        /* below test is to check resilience with respect to semi colon */
        oneof foo {string name = 4; ; SubMessage sub_message = 9 [a=1]; ; ;;}; ;;
      }
    |};
    {|
      syntax = "proto3";
      
      package examplepb;
      
      import "validate/validate.proto";
      
      message Person {
        uint64 id = 1 [(validate.rules).uint64.gt = 999];
      
        string email = 2 [(validate.rules).string.email = true];
      
        string name = 3 [(validate.rules).string = {
          pattern:   "^[^[0-9]A-Za-z]+( [^[0-9]A-Za-z]+)*$",
          max_bytes: 256,
        }];
      
        Location home = 4 [(validate.rules).message.required = true];
      
        message Location {
          double lat = 1 [(validate.rules).double = {gte: -90,  lte: 90}];
          double lng = 2 [(validate.rules).double = {gte: -180, lte: 180}];
        }
      }

      message Other {
        oneof id {
          // either x, y, or z must be set.
          option (validate.required) = true;

          string x = 1 [(validate.rules).string.len = 5];
          int32  y = 2;
          Person z = 3;
        }

        // x1 must be either 1, 2, or 3
        uint32 x1 = 1 [(validate.rules).uint32 = {in: [1,2,3]}];

        // x2 cannot be 0 nor 0.99
        float x2 = 1 [(validate.rules).float = {not_in: [0, 0.99]}];
      }
    |};
    {|
      message Shelf {}
      message GetShelfRequest {}
      message GetShelfResponse {}
      message ListShelvesRequest {}
      message ListShelvesResponse {}

      service ShelfService {}
      service ShelfServiceV2 {
        option (some.config) = "service_shelf";

        rpc ListShelves (ListShelvesRequest) returns (ListShelvesResponse);

        rpc ListShelvesStreamerClient (stream ListShelvesRequest) returns (ListShelvesResponse);
        rpc ListShelvesStreamerServer (ListShelvesRequest) returns (stream ListShelvesResponse);
        rpc ListShelvesStreamerBidi (stream ListShelvesRequest) returns (stream ListShelvesResponse);

        rpc GetShelf (GetShelfRequest) returns (GetShelfResponse) {
          option (const.config) = "rpc_shelf";
          option (google.api.http) = {
            post: "/v1/shelves/{shelf}",
            body: "body"
          };
        }

        rpc GetShelfWithSemicolon (GetShelfRequest) returns (GetShelfResponse) {};
      }
    |}
  ]

let () = List.iter run test_cases
