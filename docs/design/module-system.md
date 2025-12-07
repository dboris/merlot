# Module System Support

Future enhancement to support OCaml's module language (modules, functors, first-class modules).

## Current Approach

Merlot compiles from **Typedtree** directly to BEAM IR, bypassing Lambda. This works well for the core language but doesn't support modules/functors since `Typemod` produces module-level typed trees that we'd need to flatten.

## Challenge: Variant Constructor Names

OCaml's Lambda IR converts variant constructors to integer tags:
```
type color = Red | Green | Blue
(* In Lambda: Red → tag 0, Green → tag 1, Blue → tag 2 *)
```

For BEAM interop, we need the actual names to generate Erlang atoms.

## Proposed Solution: Side Table

Build a mapping during Typedtree traversal before converting to Lambda:

```ocaml
(* Collect during Typedtree analysis *)
type constructor_info = {
  type_path: Path.t;
  tag: int;
  name: string;
  arity: int;
}

(* Global or per-module table *)
val constructor_table : (Path.t * int, constructor_info) Hashtbl.t
```

### Implementation Steps

1. **Pre-pass on Typedtree**: Walk type declarations, collect constructor mappings
2. **Store alongside Lambda**: Pass the table to BEAM code generation
3. **Lookup during codegen**: When seeing `Const_block(tag, _)`, lookup the constructor name

### Alternative Approaches Considered

1. **PPX transformation**: Transform `Red` → `__merlot_atom "Red"` at Parsetree stage
   - Pro: String literals survive to Lambda
   - Con: Requires PPX, changes semantics

2. **Fork Translcore**: Modify OCaml's Typedtree→Lambda to preserve names
   - Pro: Clean integration
   - Con: Maintenance burden, version coupling

3. **Hybrid compilation**: Use Lambda for functions, Typedtree for data
   - Pro: No forking needed
   - Con: Complex to maintain two paths

### Decision

The **side table approach** is preferred:
- No compiler forking required
- Minimal changes to existing architecture
- Can be built incrementally

## Related Work

- Melange: Forks compiler-libs, modifies Lambda representation
- BuckleScript: Similar fork approach
- js_of_ocaml: Works at bytecode level, different tradeoffs

## Status

Not yet implemented. Current merlot works at Typedtree level where constructor names are available. This becomes necessary only when adding module/functor support.
