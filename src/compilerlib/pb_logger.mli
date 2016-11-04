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
