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

(** [ocaml-protoc] provides the ability to override all the custom
  
   protobuf file options defined in
   [src/include/ocaml-protoc/ocamloptions.proto] as command line arguments.
  
   This module implements the bridge functionality between the 2
   mechanism; command line options are converted to file options and
   appended there.
 *)
module File_options = struct
  type t = {
    mutable int32_type: string option;
    mutable int64_type: string option;
    mutable ocaml_file_ppx: string option;
    mutable ocaml_all_types_ppx: string option;
  }
  (** all file options supported. *)

  (* NOTE: this needs to be kept in sync with
     src/include/ocaml-protoc/ocamloptions.proto *)

  let make () =
    {
      int32_type = None;
      int64_type = None;
      ocaml_file_ppx = None;
      ocaml_all_types_ppx = None;
    }

  (** Compute the command line arguments be used with the {!Arg} module.  *)
  let cmd_line_args t =
    [
      ( "--int32_type",
        Arg.String
          (function
          | "int_t" -> t.int32_type <- Some "int_t"
          | x -> failwith @@ Printf.sprintf "Invalid int32_type value %s" x),
        " int32_type file option" );
      ( "--int64_type",
        Arg.String
          (function
          | "int_t" -> t.int64_type <- Some "int_t"
          | x -> failwith @@ Printf.sprintf "Invalid int64_type value %s" x),
        " int64_type file option" );
      ( "--ocaml_file_ppx",
        Arg.String (fun s -> t.ocaml_file_ppx <- Some s),
        " ocaml_file_ppx file option" );
      ( "--ocaml_all_types_ppx",
        Arg.String (fun s -> t.ocaml_all_types_ppx <- Some s),
        " ocaml_all_types_ppx file option" );
    ]

  (** Converts the command line values to Parse Tree file options
    *)
  let to_file_options t : Pb_option.set =
    let { int32_type; int64_type; ocaml_file_ppx; ocaml_all_types_ppx } = t in

    let map x f options =
      match x with
      | None -> options
      | Some x ->
        let option_name, option_value = f x in
        Pb_option.add options option_name option_value
    in
    Pb_option.empty
    |> map int32_type (fun s ->
           "int32_type", Pb_option.(Scalar_value (Constant_literal s)))
    |> map int64_type (fun s ->
           "int64_type", Pb_option.(Scalar_value (Constant_literal s)))
    |> map ocaml_file_ppx (fun s ->
           "ocaml_file_ppx", Pb_option.(Scalar_value (Constant_string s)))
    |> map ocaml_all_types_ppx (fun s ->
           "ocaml_all_types_ppx", Pb_option.(Scalar_value (Constant_string s)))
end

(** Command line argument for the ocaml-protoc *)
module Cmdline = struct
  type t = {
    mutable ml_out: string;  (** output directory *)
    mutable proto_file_name: string;
        (** proto file name as given on the cmd line *)
    mutable include_dirs: string list;
        (** include directories given with -I argument *)
    binary: bool ref;  (** whether binary encoding is enabled *)
    yojson: bool ref;  (** whether yojson encoding is enabled *)
    bs: bool ref;  (** whether BuckleScript encoding is enabled *)
    pp: bool ref;  (** whether pretty printing is enabled *)
    dump_type_repr: bool ref;
        (** whether comments with debug ocaml type representation are added *)
    quickcheck: bool ref;  (** whether quickcheck code generation is enabled *)
    services: bool ref;  (** whether services code generation is enabled *)
    make: bool ref;  (** whether to generate "make" functions *)
    mutable cmd_line_file_options: File_options.t;
        (** file options override from the cmd line *)
    unsigned_tag: bool ref;
        (** if true, unsigned int32/64s will be generated with a polymorphic
           variant [`unsigned int32/64], otherwise will be emitted as
           immediate [int32/int64]. *)
  }

  let make () =
    {
      ml_out = "";
      proto_file_name = "";
      include_dirs = [];
      binary = ref false;
      yojson = ref false;
      bs = ref false;
      pp = ref false;
      dump_type_repr = ref false;
      quickcheck = ref false;
      services = ref false;
      make = ref false;
      cmd_line_file_options = File_options.make ();
      unsigned_tag = ref false;
    }

  let cmd_line_args t =
    [
      "--yojson", Arg.Set t.yojson, " generate yojson encoding";
      "--bs", Arg.Set t.bs, " generate BuckleScript encoding";
      "--binary", Arg.Set t.binary, " generate binary encoding";
      "--pp", Arg.Set t.pp, " generate pretty print functions";
      ( "--dump_type_repr",
        Arg.Set t.dump_type_repr,
        " generate comments with internal representation on generated OCaml \
         types (useful for debugging ocaml-protoc itself)" );
      "--quickcheck", Arg.Set t.quickcheck, " generate quickcheck helpers";
      ( "--services",
        Arg.Set t.services,
        " generate code for services (requires json+binary)" );
      ( "-I",
        Arg.String (fun s -> t.include_dirs <- s :: t.include_dirs),
        " include directories" );
      "--ml_out", Arg.String (fun s -> t.ml_out <- s), " output directory";
      ( "--debug",
        Arg.Unit (fun () -> Pb_logger.setup_from_out_channel stderr),
        " print logs on stderr" );
      ( "--unsigned",
        Arg.Set t.unsigned_tag,
        " tag uint32 and uint64 types with `unsigned" );
      "--make", Arg.Set t.make, " generate `make` functions";
    ]
    @ File_options.cmd_line_args t.cmd_line_file_options

  let usage = "ocaml-protoc -ml_out <output_directory> <file_name>.proto"
  let anon_fun t proto_file_name = t.proto_file_name <- proto_file_name

  let validate t =
    if
      (not !(t.yojson)) && (not !(t.binary)) && (not !(t.pp)) && (not !(t.bs))
      && not !(t.dump_type_repr)
    then (
      t.binary := true;
      t.pp := true
    );

    if !(t.quickcheck) then (
      t.binary := true;
      t.yojson := true
    );

    if !(t.services) then (
      t.binary := true;
      t.yojson := true
    );

    if t.proto_file_name = "" then
      failwith "Missing proto file name from command line argument";

    if t.ml_out = "" then
      failwith "Missing -ml_out (output directory) from command line argument"

  let parse () =
    let args = make () in
    let anon_fun = anon_fun args in
    let cmd_line_args = cmd_line_args args |> Arg.align in
    Arg.parse cmd_line_args anon_fun usage;
    validate args;
    args
end
(* Cmdline *)
