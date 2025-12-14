(** {1 Merlot - OCaml to BEAM Compiler Library}

    Merlot compiles OCaml source code to BEAM bytecode for the Erlang VM.

    {2 Compilation Pipeline}

    The compiler works in these stages:

    + Parse and typecheck OCaml source using the OCaml compiler frontend
    + Convert the typed AST to {!module:Beam_ir} intermediate representation
    + Generate Core Erlang code via {!module:Core_erlang}
    + Compile to BEAM bytecode using [erlc]

    {2 Usage Example}

    {[
      let typedtree = Compile.compile_implementation "hello.ml" in
      let beam_module = Typed_to_beam.convert_structure ~module_name:"Hello" typedtree in
      Core_erlang.write_module "hello.core" beam_module
    ]}
*)

(** BEAM intermediate representation.

    Defines the AST types for BEAM-compatible code: expressions, patterns,
    literals, and module definitions. This IR is independent of both
    OCaml's typed tree and Core Erlang syntax. *)
module Beam_ir = Beam_ir

(** OCaml compilation frontend.

    Parses and typechecks OCaml source files, producing a typed AST
    that can be converted to BEAM code. *)
module Compile = Compile

(** Typed AST to BEAM IR conversion.

    Converts OCaml's typed tree to BEAM IR, handling:
    - Function definitions and applications
    - Pattern matching and case expressions
    - Records and variants
    - Let bindings and recursion
    - FFI external declarations *)
module Typed_to_beam = Typed_to_beam

(** Core Erlang code generation.

    Pretty-prints BEAM IR as valid Core Erlang source code that can
    be compiled by [erlc +from_core]. *)
module Core_erlang = Core_erlang

(** Documentation extraction.

    Extracts doc comments from OCaml source and generates:
    - Markdown documentation files
    - EEP-48 compatible documentation chunks for BEAM tooling *)
module Doc_extract = Doc_extract

(** Macro expansion.

    Handles compile-time macro expansion for the BEAM IR. Macros transform
    AST fragments using quote/unquote/splice, similar to Elixir's macro system.
    Includes hygiene support to prevent variable capture. *)
module Macro_expand = Macro_expand
