(* Aggregate (message literal) option values, as used by every Google API .proto:

     rpc Search(Req) returns (Res) {
       option (google.api.http) = {
         post: "/v18/customers/{customer_id=*}/googleAds:search"
         body: "*"
       };
     }

   Entries of a message literal are separated by whitespace; a comma or a semicolon
   may appear between them but neither is required. Requiring a comma is what made
   the file above fail to parse (issue #254).

   Every case below pins a distinct production of the grammar, so that mutating any
   single one of them is caught by a named case rather than by the suite as a whole. *)

module O = Pb_option

let parse s = Pb_parsing_parser.option_ Pb_parsing_lexer.lexer (Lexing.from_string s)
let value s = snd (parse s)
let str s = O.Scalar_value (O.Constant_string s)
let int i = O.Scalar_value (O.Constant_int i)
let msg l = O.Message_literal l

(* Each case names the production it pins, so a failure report says what broke. *)
let accepted =
  [ ( "separator: none (issue #254)",
      {|option (google.api.http) = { post: "a" body: "b" };|},
      msg [ "post", str "a"; "body", str "b" ] );
    ( "separator: comma (pre-existing, must not regress)",
      {|option (a) = { post: "a", body: "b" };|},
      msg [ "post", str "a"; "body", str "b" ] );
    ( "separator: semicolon",
      {|option (a) = { post: "a"; body: "b" };|},
      msg [ "post", str "a"; "body", str "b" ] );
    ( "separator: mixed, three entries",
      {|option (a) = { x: 1, y: 2; z: 3 };|},
      msg [ "x", int 1; "y", int 2; "z", int 3 ] );
    ( "separator: trailing comma",
      {|option (a) = { x: 1, };|},
      msg [ "x", int 1 ] );
    ( "separator: trailing semicolon",
      {|option (a) = { x: 1; };|},
      msg [ "x", int 1 ] );
    ( "separator: newline only, the real googleapis layout",
      "option (google.api.http) = {\n  post: \"/v1/{name=x}:cancel\"\n  body: \"*\"\n};",
      msg [ "post", str "/v1/{name=x}:cancel"; "body", str "*" ] );
    ( "empty message literal",
      {|option (a) = {};|},
      msg [] );
    ( "colon omitted before a message value",
      {|option (a) = { http { get: "/v1/x" } };|},
      msg [ "http", msg [ "get", str "/v1/x" ] ] );
    ( "colon omitted, then a whitespace-separated sibling",
      {|option (a) = { http { get: "/v1/x" } selector: "s" };|},
      msg [ "http", msg [ "get", str "/v1/x" ]; "selector", str "s" ] );
    ( "colon present before a message value (pre-existing)",
      {|option (a) = { http: { get: "/v1/x" } };|},
      msg [ "http", msg [ "get", str "/v1/x" ] ] );
    ( "nesting two deep, no separators anywhere",
      {|option (a) = { a { b { c: 1 } d: 2 } e: 3 };|},
      msg [ "a", msg [ "b", msg [ "c", int 1 ]; "d", int 2 ]; "e", int 3 ] );
    (* Adjacent string literals concatenate. googleapis wraps long option values this
       way, and it is why pubsub, spanner, firestore and language_service could not be
       parsed even once the separator was fixed. *)
    ( "adjacent string literals concatenate",
      "option (google.api.oauth_scopes) =\n\
      \    \"https://www.googleapis.com/auth/cloud-platform,\"\n\
      \    \"https://www.googleapis.com/auth/pubsub\";",
      str
        "https://www.googleapis.com/auth/cloud-platform,https://www.googleapis.com/auth/pubsub"
    );
    ( "three adjacent string literals",
      {|option (a) = "x" "y" "z";|},
      str "xyz" );
    ( "concatenation inside a message literal",
      {|option (a) = { post: "/v1/" "shelves" body: "*" };|},
      msg [ "post", str "/v1/shelves"; "body", str "*" ] );
    ( "a lone string is unaffected by concatenation",
      {|option (a) = "solo";|},
      str "solo" );
    (* protoc accepts a keyword as a field name inside a message literal. No proto in
       the googleapis corpus measured for this change actually does so, but the key
       position now uses the field_name nonterminal the grammar already keeps for this
       purpose, which makes it a strict superset of what T_ident accepted. *)
    ( "keyword field names as keys",
      {|option (a) = { to: 1 option: 2 message: 3 max: 4 stream: 5 map: 6 };|},
      msg
        [ "to", int 1; "option", int 2; "message", int 3;
          "max", int 4; "stream", int 5; "map", int 6 ] );
    ( "list value still parses (pre-existing)",
      {|option (a) = { nums: [1, 2] };|},
      msg [ "nums", O.List_literal [ int 1; int 2 ] ] );
    (* protoc rejects a trailing comma in a LIST, so this is a divergence, but it
       predates this change and is pinned here on purpose: the separator work above is
       about the MAP form, and nobody should quietly "fix" the list form while passing
       through. *)
    ( "trailing comma in a list is still accepted, as before",
      {|option (a) = { nums: [1, 2,] };|},
      msg [ "nums", O.List_literal [ int 1; int 2 ] ] );
    ( "braces inside a string are not delimiters",
      {|option (a) = { post: "/v18/customers/{customer_id=*}/googleAds:search" };|},
      msg [ "post", str "/v18/customers/{customer_id=*}/googleAds:search" ] )
  ]

(* The colon is optional only before a MESSAGE value. protoc rejects `post "a"`, and
   so must we: dropping the colon requirement everywhere would silently accept protos
   that protoc refuses. *)
let rejected =
  [ "colon is mandatory before a scalar value", {|option (a) = { post "a" };|};
    "a key with no value", {|option (a) = { post: };|};
    "a value with no key", {|option (a) = { : 1 };|};
    "doubled colon", {|option (a) = { post:: "a" };|};
    "doubled separator", {|option (a) = { x: 1 ,, y: 2 };|}
  ]

(* The parser reports a rejection by raising, so this is the single place that turns
   those raises into an option; every case below is expressed in terms of it. The
   constructors are named rather than caught with a wildcard, so a genuine fault such
   as Stack_overflow still fails the test instead of being read as "protoc rejects it
   too". *)
let value_opt s =
  try Some (value s) with
  | Parsing.Parse_error -> None
  | Failure _ -> None
  | Pb_exception.Compilation_error _ -> None

let () =
  let accept_failures =
    List.filter_map
      (fun (label, src, expected) ->
        Option.fold ~none:(Some (label ^ " -- did not parse at all"))
          ~some:(fun v ->
            if v = expected then None
            else Some (label ^ " -- parsed, but into the wrong structure"))
          (value_opt src))
      accepted
  in
  let reject_failures =
    List.filter_map
      (fun (label, src) ->
        Option.map
          (fun _ -> label ^ " -- was accepted, but protoc rejects it")
          (value_opt src))
      rejected
  in
  let failures = accept_failures @ reject_failures in
  List.iter (fun f -> print_endline ("FAIL: " ^ f)) failures;
  if failures <> [] then exit 1;
  Printf.printf "Parse Aggregate Option ... Ok (%d accepted, %d rejected)\n"
    (List.length accepted) (List.length rejected)
