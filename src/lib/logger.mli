
(** Logging functionality allow printing debugging information
 *)


val setup_from_out_channel : out_channel -> unit 
(** [setup_from_out_channel oc] will configure the logger to print all 
    logging statement to [oc]. 

    Function can be called multiple times, each call will disable (override) the 
    previously setup out channel
 *)

val log : ('a, out_channel, unit) format -> 'a
(** [log format x y ] same as [Printf.printf] except that the message will 
    get printed to the previously setup [out_channel] in the
    [setup_from_out_channel] function. 
 *) 

val endline : string -> unit 
(** [endline format x y ] same as [Pervasives.print_endline] except that the message will 
    get printed to the previously setup [out_channel] in the
    [setup_from_out_channel] function. 
 *) 
