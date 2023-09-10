let pf = Printf.printf
let spf = Printf.sprintf

let codes =
  [
    "canceled", 408, "The operation was cancelled.";
    ( "unknown",
      500,
      "An unknown error occurred. For example, this can be used when handling \
       errors raised by APIs that do not return any error information." );
    ( "invalid_argument",
      400,
      "The client specified an invalid argument. This indicates arguments that \
       are invalid regardless of the state of the system (i.e. a malformed \
       file name, required argument, number out of range, etc.)." );
    ( "malformed",
      400,
      "The client sent a message which could not be decoded. This may mean \
       that the message was encoded improperly or that the client and server \
       have incompatible message definitions." );
    ( "deadline_exceeded",
      408,
      "Operation expired before completion. For operations that change the \
       state of the system, this error may be returned even if the operation \
       has completed successfully (timeout)." );
    "not_found", 404, "Some requested entity was not found.";
    ( "bad_route",
      404,
      {|The requested URL path wasn't routable to a Twirp service and method. This is returned by generated server code and should not be returned by application code (use " not_found " or " unimplemented " instead).|}
    );
    ( "already_exists",
      409,
      "An attempt to create an entity failed because one already exists." );
    ( "permission_denied",
      403,
      {|The caller does not have permission to execute the specified operation. 
       It must not be used if the caller cannot be identified (use "
      unauthenticated " instead).|}
    );
    ( "unauthenticated",
      401,
      "The request does not have valid authentication credentials for the \
       operation." );
    ( "resource_exhausted",
      429,
      "Some resource has been exhausted or rate-limited, perhaps a per-user \
       quota, or perhaps the entire file system is out of space." );
    ( "failed_precondition",
      412,
      "The operation was rejected because the system is not in a state \
       required for the operation's execution. For example, doing an rmdir \
       operation on a directory that is non-empty, or on a non-directory \
       object, or when having conflicting read-modify-write on the same \
       resource." );
    ( "aborted",
      409,
      "The operation was aborted, typically due to a concurrency issue like \
       sequencer check failures, transaction aborts, etc." );
    ( "out_of_range",
      400,
      {|The operation was attempted past the valid range. For example, seeking  or reading past end of a paginated collection. Unlike " invalid_argument ", this error indicates a problem that may be fixed if the system  state changes (i.e. adding more items to the collection). There is a  fair bit of overlap between " failed_precondition " and " out_of_range ". We recommend using " out_of_range " (the more specific error) when it applies so that callers who are  iterating through a space can easily look for an " out_of_range " error to detect when they are done.|}
    );
    ( "unimplemented",
      501,
      "The operation is not implemented or not supported/enabled in this \
       service." );
    ( "internal",
      500,
      "When some invariants expected by the underlying system have been \
       broken. In other words, something bad happened in the library or \
       backend service. Twirp specific issues like wire and serialization \
       problems are also reported as \"internal\" errors." );
    ( "unavailable",
      503,
      "The service is currently unavailable. This is most likely a transient \
       condition and may be corrected by retrying with a backoff." );
    ( "dataloss",
      500,
      "The operation resulted in unrecoverable data loss or corruption." );
  ]

let () =
  let code =
    codes
    |> List.map (fun (c, _, doc) ->
           spf "  | %s (** %s *)" (String.capitalize_ascii c) doc)
    |> String.concat "\n"
  in

  pf "type t =\n%s\n\n" code;

  let to_msg_and_code =
    codes
    |> List.map (fun (c, n, _) ->
           spf "  | %s -> (%S, %d)" (String.capitalize_ascii c) c n)
    |> String.concat "\n"
  in

  pf "(** Convert to a precise message and an http code *)\n";
  pf "let to_msg_and_code = function\n%s\n\n" to_msg_and_code;

  let to_descr =
    codes
    |> List.map (fun (c, _, doc) ->
           spf "  | %s -> %S" (String.capitalize_ascii c) doc)
    |> String.concat "\n"
  in

  pf "(** Get doc about this error *)\n";
  pf "let to_descr = function\n%s\n\n" to_descr
