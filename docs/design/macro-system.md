# Macro System Design

This document describes the design of Merlot's macro system, inspired by Elixir's
ergonomic metaprogramming while learning from the failures of OCaml's Camlp4 and
the limitations of PPX.

## Motivation

Elixir's success is largely due to its powerful macro system, which enables:
- Ergonomic DSLs (Ecto schemas, Phoenix routers, ExUnit tests)
- Compile-time code generation with minimal boilerplate
- Metaprogramming that feels natural to use

OCaml has PPX for metaprogramming, but:
- PPX is notoriously unergonomic for developers
- Breaks frequently with OCaml compiler updates (Parsetree changes)
- Complex toolchain (ppxlib, separate compilation)
- Limited expressiveness compared to Elixir macros

## Design Goals

1. **Elixir-like ergonomics** - Quote/unquote pattern, hygienic by default
2. **Stability** - Avoid PPX's version fragility
3. **Single parser** - No Camlp4-style dual parser problems
4. **IDE-friendly** - Work with standard OCaml tooling
5. **Incremental** - Can be adopted gradually

## Key Insight: BEAM IR as the Macro Level

Merlot's compilation pipeline:

```
OCaml Source
    |
    v
TypedTree (OCaml compiler-libs)  <- Unstable, version-dependent
    |
    v
============================================
    |
    v
BEAM IR (beam_ir.ml)             <- STABLE, our control
    |
    v
============================================
    |
    v
Core Erlang -> BEAM bytecode
```

By doing macro expansion at the **BEAM IR level**, we:
- Avoid PPX's version fragility (Parsetree changes between OCaml versions)
- Avoid Camlp4's dual-parser problem (use standard OCaml parser)
- Get a simple, stable AST to work with
- Can leverage Elixir-style patterns (BEAM IR is semantically close to Elixir's AST)

## Core Concepts

### Quote: Capturing Code as Data

The `quote` construct captures BEAM IR expressions as manipulable data:

```ocaml
let my_ast = [%quote 1 + 2]
(* Produces: Expr_binary (Op_add, Expr_lit (Lit_int 1), Expr_lit (Lit_int 2)) *)
```

### Unquote: Splicing Values into Quoted Code

The `unquote` construct splices values into quoted expressions:

```ocaml
let x = 42
let ast = [%quote [%u x] + 1]
(* Result: Expr_binary (Op_add, Expr_lit (Lit_int 42), Expr_lit (Lit_int 1)) *)
```

For splicing AST fragments (as opposed to values):

```ocaml
let fragment = [%quote a + b]
let combined = [%quote [%s fragment] * 2]
(* Result: (a + b) * 2 *)
```

Shorthand:
- `[%u x]` - unquote value
- `[%s x]` - splice AST fragment

### Macro Definition

Macros are functions that transform BEAM IR at compile time:

```ocaml
let%macro unless condition body =
  [%quote
    if not [%s condition] then
      [%s body]
    else
      ()
  ]

(* Usage *)
let result = unless (x = 0) (100 / x)
```

### Hygiene

Variables in macros are hygienic by default - they won't capture or be captured
by variables in the calling context:

```ocaml
let%macro swap a b =
  [%quote
    let tmp = [%s a] in      (* tmp is macro-local *)
    let () = [%s a] <- [%s b] in
    let () = [%s b] <- tmp in
    ()
  ]

(* Safe - caller's tmp is not affected *)
let tmp = 100
swap x y
```

Escape hatch with the `[@expose]` attribute for deliberate unhygienic access:

```ocaml
(* Using actual implemented syntax *)
let with_resource = Macro.defmacro (fun body ->
  Macro.quote (
    let resource [@expose] = acquire_resource () in
    let result = Macro.unquote body in
    release_resource resource;
    result
  )
)

(* 'resource' is available in the body *)
with_resource (use resource)
```

## Implementation

### Extended BEAM IR Types

```ocaml
(* Hygiene context for variables *)
type hygiene_context = {
  module_name: string;
  unique_id: int;
}

(* Extended expression type *)
type expr =
  | ... (* existing variants *)
  | Expr_quote of expr                    (* Quoted expression *)
  | Expr_unquote of expr                  (* Unquote value *)
  | Expr_splice of expr                   (* Splice AST fragment *)
  | Expr_var_unhygienic of string         (* Unhygienic variable reference *)

(* Macro definition *)
type macro_def = {
  name: string;
  params: string list;
  body: expr;  (* The quoted template *)
}
```

### Compilation Pipeline

```
OCaml Source (with [%quote], [%macro], etc.)
    |
    v
OCaml Parser (standard - no changes!)
    |
    v
TypedTree
    |
    v
+-------------------------------+
|  typed_to_beam.ml             |
|  (recognizes macro constructs)|
+-------------------------------+
    |
    v
BEAM IR (with quote/unquote/splice nodes)
    |
    v
+-------------------------------+
|  macro_expand.ml              |  <- NEW
|  - Collect macro definitions  |
|  - Expand macro invocations   |
|  - Apply hygiene renaming     |
+-------------------------------+
    |
    v
BEAM IR (pure, no macro nodes)
    |
    v
Core Erlang -> BEAM
```

### Two-Pass Expansion

Similar to existing `externals` and `top_level_functions` handling:

1. **First pass**: Collect all macro definitions into a table
2. **Second pass**: Expand macro invocations, applying hygiene

```ocaml
(* Macro registry *)
let macros : (string, macro_def) Hashtbl.t = Hashtbl.create 16

(* First pass *)
let collect_macros (module_def : module_def) = ...

(* Second pass *)
let expand_macros (expr : expr) : expr = ...
```

### Hygiene Implementation

Variables are tagged with their origin context:

```ocaml
type var_ref = {
  name: string;
  context: hygiene_context option;  (* None = caller context *)
}
```

During expansion:
1. Variables defined in macro body get tagged with macro's context
2. Variables from `[%s ...]` splices keep their original context
3. Variables with `[%var! ...]` get `None` context (caller's scope)
4. At code generation, same-name variables with different contexts get unique suffixes

## Examples

### Debug Macro

```ocaml
let%macro debug expr =
  [%quote
    let value__ = [%s expr] in
    let expr_str__ = [%stringify expr] in
    Io.format "[DEBUG] ~s = ~p~n" [expr_str__; value__];
    value__
  ]

(* Usage *)
let result = debug (compute x y)

(* Expands to *)
let result =
  let value__42 = compute x y in
  let expr_str__42 = "compute x y" in
  Io.format "[DEBUG] ~s = ~p~n" [expr_str__42; value__42];
  value__42
```

### Assert Macro

```ocaml
let%macro assert_ condition =
  [%quote
    if not [%s condition] then
      raise (Assertion_error [%stringify condition])
    else
      ()
  ]

(* Usage *)
assert_ (x > 0)

(* Expands to *)
if not (x > 0) then
  raise (Assertion_error "x > 0")
else
  ()
```

### Router DSL (Future)

```ocaml
let%macro routes items =
  (* Parse route definitions and generate handler list *)
  ...

let router = [%routes
  get "/users" Users.index;
  get "/users/:id" Users.show;
  post "/users" Users.create;
]
```

## Comparison with Other Systems

| Aspect | Merlot Macros | Elixir | PPX | Template Haskell |
|--------|---------------|--------|-----|------------------|
| Level | BEAM IR | Elixir AST | Parsetree | Haskell AST |
| Parser | Standard OCaml | Standard Elixir | Standard OCaml | Standard Haskell |
| Hygiene | Default | Default | N/A | Manual |
| Stability | High (stable IR) | High | Low (breaks often) | Medium |
| Type info | Limited | None | Full | Full |
| Complexity | Medium | Low | High | High |

## Limitations

1. **Type information**: At BEAM IR level, we've lost some type information.
   For `deriving`-style macros, may need to pass type info explicitly.

2. **No new syntax**: Cannot create truly new syntax (like Camlp4 could).
   Limited to what `[%extension]` points allow.

3. **Cross-module macros**: Macro definitions must be compiled before use.
   Similar to Elixir's `require`.

4. **Debugging**: Expanded code differs from source. Need good error messages
   that point back to original source locations.

## Future Extensions

1. **Deriving macros**: Generate code based on type structure
2. **Attribute macros**: Transform annotated definitions
3. **Macro-defining macros**: Higher-order macros (carefully limited)
4. **IDE integration**: Show expanded code on hover

## Implementation Plan

### Phase 1: Foundation
- [x] Add quote/unquote/splice to BEAM IR types
- [x] Recognize `Macro.quote`, `Macro.unquote`, `Macro.to_string` in typed_to_beam.ml
- [x] Basic macro expansion without hygiene

### Phase 2: Hygiene
- [x] Implement hygiene context tracking
- [x] Add `[@expose]` attribute escape hatch (replaces `[%var!]` design)
- [x] Variable renaming during expansion

### Phase 3: Ergonomics
- [x] `Macro.defmacro` for defining macros with auto-generated stubs
- [x] `Macro.to_string` for source-to-string (stringify)
- [ ] Good error messages with source locations

### Phase 4: Stdlib Macros
- [x] `debug` macro (example in example/9-macro/)
- [ ] `assert_` macro
- [ ] `todo` macro (like Rust's todo!())

## References

- [Elixir Macros Documentation](https://hexdocs.pm/elixir/macros.html)
- [Template Haskell User Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html)
- [The End of Camlp4](https://discuss.ocaml.org/t/the-end-of-camlp4/4216)
- [PPX and Metaprogramming in OCaml](https://ocaml.org/docs/metaprogramming)
