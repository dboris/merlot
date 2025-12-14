# 9-macro

This example demonstrates Merlot's compile-time macro system, inspired by
Elixir's quote/unquote. Macros allow code generation and transformation
at compile time.

## Features demonstrated

- **Macro definition** - Using `Macro.defmacro`
- **Quote** - `Macro.quote` captures expression AST
- **Unquote** - `Macro.unquote expr` inserts parameter AST
- **To string** - `Macro.to_string expr` converts expression to string
- **Hygiene** - Macro-introduced variables don't clash with user code

## The Debug Macro

The debug macro is similar to Rust's `dbg!` or Elixir's `IO.inspect/2`:

```ocaml
(* Definition *)
let debug = Macro.defmacro (fun expr ->
  Macro.quote (
    let __debug_value = Macro.unquote expr in
    let _ = Io.format "DEBUG: ~s = ~p~n" [Macro.to_string expr; __debug_value] in
    __debug_value
  )
)

(* Usage *)
let result = debug (add a b)

(* Expands to *)
let result =
  let __debug_value__macro_0 = add a b in
  let _ = Io.format "DEBUG: ~s = ~p~n" ["add(A, B)"; __debug_value__macro_0] in
  __debug_value__macro_0
```

## Build & Run

```bash
cd example/9-macro

# Compile to BEAM
make

# Run test (prints debug output)
make test
```

## Expected output

```
DEBUG: add(A, B) = 30
DEBUG: (A * 2) = 20
DEBUG: multiply(Sum, Doubled) = 600
```

## How macros work

| Concept | Syntax | Effect |
|---------|--------|--------|
| Define macro | `let name = Macro.defmacro (fun arg -> ...)` | Define a macro |
| Quote | `Macro.quote (expr)` | Capture expression as AST template |
| Unquote | `Macro.unquote param` | Insert parameter's AST |
| To string | `Macro.to_string param` | Convert parameter to string literal |

## Hygiene

Variables introduced by the macro are automatically renamed to prevent
capture. In the debug macro, `__debug_value` becomes `__debug_value__macro_N`
where N is unique per expansion. This prevents conflicts when using
the macro multiple times.

To intentionally expose a binding to the caller (bypassing hygiene), use
the `[@expose]` attribute on the let binding:

```ocaml
let with_resource = Macro.defmacro (fun body ->
  Macro.quote (
    let resource [@expose] = acquire () in
    let result = Macro.unquote body in
    release resource;
    result
  )
)

(* Usage - 'resource' is available in the body *)
with_resource (use resource)
```
