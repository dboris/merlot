(** Macro - Compile-time metaprogramming primitives

    This module provides Elixir-style macros for compile-time code transformation.
    Macros allow you to generate code at compile time, enabling powerful abstractions
    without runtime overhead.

    {1 Overview}

    Macros in Merlot work similarly to Elixir:
    - [quote] captures an expression's AST
    - [unquote] splices values into quoted code
    - [to_string] converts expressions to their source representation

    The functions in this module are compile-time markers - they are intercepted
    during compilation and converted to BEAM IR macro nodes, never actually
    executed at runtime.

    {1 Defining Macros}

    Use [defmacro] to define a macro. The compiler automatically generates a stub
    function for typechecking:

    {[
      open Merlot_stdlib

      (* A debug macro that prints an expression and its value *)
      let debug = Macro.defmacro (fun expr ->
        Macro.quote (
          let value = Macro.unquote expr in
          Io.format "DEBUG: ~s = ~p~n" [Macro.to_string expr; value];
          value
        )
      )

      (* Usage - expands at compile time *)
      let result = debug (compute x y)
    ]}

    {1 How Macros Expand}

    When you call [debug (x * 2)], the compiler expands it to:
    {[
      let value = x * 2 in
      Io.format "DEBUG: ~s = ~p~n" ["(X * 2)"; value];
      value
    ]}

    {1 Hygiene}

    Macros are hygienic - variables introduced inside the macro body don't clash
    with variables in user code. The compiler automatically renames internal
    variables with unique suffixes to prevent collisions.

    {1 Comparison with Elixir}

    {v
    | Elixir                      | Merlot                                    |
    |-----------------------------|-------------------------------------------|
    | defmacro debug(expr) do     | let debug = Macro.defmacro (fun expr ->   |
    | quote do ... end            | Macro.quote (...)                         |
    | unquote(expr)               | Macro.unquote expr                        |
    | Macro.to_string(expr)       | Macro.to_string expr                      |
    | var!(name)                  | let name [@expose] = ...                  |
    v}

    {1 Unhygienic Bindings}

    By default, macro-introduced variables are hygienic (renamed to avoid conflicts).
    Use the [[\@expose]] attribute on a let binding to bypass hygiene and expose
    the variable to the caller's scope:

    {[
      let with_conn = Macro.defmacro (fun body ->
        Macro.quote (
          let conn [@expose] = Database.connect () in
          let result = Macro.unquote body in
          Database.close conn;
          result
        )
      )

      (* Usage - conn is available in the body *)
      with_conn (Database.query conn "SELECT *")
    ]}

    Note: Use sparingly. Most macros should use hygienic variables.
*)

(** Define a macro. The compiler extracts the function and generates a stub.
    The binding name becomes the macro name.

    Example:
    {[
      let debug = Macro.defmacro (fun expr ->
        Macro.quote (...)
      )
    ]}

    This defines a macro [debug] that can be called like a regular function. *)
let defmacro : ('a -> 'b) -> 'a -> 'b = fun f x -> f x

(** Quote an expression, capturing its AST representation.
    This is recognized at compile-time and converted to Expr_quote. *)
let quote : 'a -> 'a = fun x -> x

(** Unquote (splice) a macro parameter into a quoted expression.
    Pass the macro parameter variable directly - the compiler extracts its name.
    This is recognized at compile-time and converted to Expr_unquote.

    Example: [Macro.unquote expr] splices the AST passed for [expr] *)
let unquote : 'a -> 'b = fun _ -> failwith "Macro.unquote should be expanded at compile-time"

(** Convert a macro parameter to its source text representation.
    Pass the macro parameter variable directly.
    This is recognized at compile-time and converted to Expr_stringify.

    Example: [Macro.to_string expr] returns the source text of [expr] *)
let to_string : 'a -> string = fun _ -> failwith "Macro.to_string should be expanded at compile-time"
