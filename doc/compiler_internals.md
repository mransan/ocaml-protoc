## Compiler Internals

* [Overview](#overview)
* [Runtime](#rutime)
* [Code Structure](#code-structure)

##### Overview 

`ocaml-protoc` follows the classic compiler pipeline as described below:

```
            .proto file 
                |
                |  Lexing (lexer.mll) and Parsing (parser.mly)
                |
                V
          Parsed Tree (pbpt.mli) 
                |
                |  Type resolution (pbtt_util.{mli|ml}) 
                |  and mutally recursive type analysis (graph.{mli|ml}) 
                |
                V
            Typed Tree (pbtt.mli)
                |
                |  OCaml compilation (backend_ocaml.{mli|ml})
                | 
                V
       Simplified OCaml AST (ocaml_type.mli)
                |
                | Printing (ocaml/codegen_{encode|decode|default|pp}.{mli|ml}) 
                |
                V
        OCaml code (.mli + .ml)
```

##### Runtime 

The generated code relies a [runtime library](src/runtime/pbrt.mli) called `pbrt`. 
This runtime is mostly based on previous work by [whiteshark](https://github.com/whitequark/).

#### Code structure 

| file                     | Description | 
|--------------------------|-------------|
|lexer.mll                 | Ocamllex lexing rules|
|parser.mly                | Ocamlyacc parsing rules|
|pbpt.ml                   | Protobuf parse tree AST|
|pbpt_util.mli             | Protobuf parse tree creator functions|
|pbtt.ml                   | Protobuf typed tree AST|
|pbtt_util.mli             | Protobuf typed tree utilities and type checking routines| 
|graph.mli                 | graph algorithm with Strongly Connected Component algo for deducing mutually recursive types|
|encoding_util.mli         | Small type to keep track in the OCaml AST of the rules for protobuf encoding|
|exception.mli             | All compiler exceptions |
|logger.mli                | Small logging utily|
|util.mli                  | Classic missing standard library functions| 
|fmt.mli                   | Simplify formatting utilities to generate code|
|ocaml/ocaml_types.ml      | OCaml type AST|
|ocaml/ocaml_backend.mli   | Compilation of the typed tree to the OCaml AST| 
|ocaml/codegen_encode.mli  | Printing of the OCaml code|
|ocaml/codegen_decode.mli  | Printing of the OCaml code|
|ocaml/codegen_pp.mli      | Printing of the OCaml code|
|ocaml/codegen_default.mli | Printing of the OCaml code|
