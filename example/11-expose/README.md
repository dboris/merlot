# 11-expose

This example demonstrates the `[@expose]` attribute for bypassing macro hygiene.

## The Problem

By default, macros are *hygienic* - variables introduced inside a macro are
automatically renamed to avoid conflicts with the caller's variables:

```ocaml
(* WITHOUT [@expose] - hygienic (default) *)
let my_macro = Macro.defmacro (fun expr ->
  Macro.quote (
    let tmp = Macro.unquote expr in  (* becomes tmp__my_macro_0 *)
    tmp * 2
  )
)
```

## The Solution: [@expose]

The `[@expose]` attribute bypasses hygiene for specific bindings:

```ocaml
(* WITH [@expose] - variable keeps its name *)
let my_macro = Macro.defmacro (fun expr ->
  Macro.quote (
    let tmp [@expose] = Macro.unquote expr in  (* stays as 'tmp' *)
    tmp * 2
  )
)
```

## Examples in this file

### 1. `timed` - Timing with exposed elapsed time
Times an expression and exposes `elapsed`:
```ocaml
let result = timed (expensive_computation ()) in
(* 'elapsed' is available here *)
```

### 2. `debug` - Debug macro with exposed value
Exposes `dbg` containing the evaluated expression:
```ocaml
let x = debug (10 + 20) in
(* prints: DEBUG (10 + 20) = 30 *)
```

### 3. `compare` - Hygienic vs exposed side-by-side
Demonstrates both in one macro:
- `hidden` is hygienic (renamed to `hidden__compare_N`)
- `visible` is exposed (keeps its name)

## Comparison with Elixir

| Elixir | Merlot |
|--------|--------|
| `var!(name) = value` | `let name [@expose] = value in` |

## Build & Run

```bash
cd example/11-expose
make
make test
```

## When to use [@expose]

Use `[@expose]` sparingly - only when you *intentionally* want a macro to
introduce a binding that the caller can reference. Common use cases:

- Resource management (files, connections, locks)
- Context injection (request, session, transaction)
- Binding results that need to be referenced later

Most macros should remain hygienic by default to prevent accidental variable capture.
