# Stdlib Support TODO

This document tracks the work needed to fully support the OCaml-compatible stdlib modules (List, Option, Result, String) on the BEAM.

## Current State

The stdlib provides two types of bindings:

1. **External bindings** - Map OCaml functions directly to Erlang/Elixir calls via `external` declarations. These work at compile time and don't need BEAM modules.

2. **OCaml implementations** - Functions with actual OCaml code (like `List.fold_left`, `Option.map`) that need to be compiled to BEAM bytecode.

## Compiler Limitations

### 1. ~~Nested `let rec` Functions~~ ✅ FIXED

~~**Issue**: Local recursive functions inside function bodies generate "unbound variable" errors.~~

**Fixed**: The compiler now registers letrec-bound functions in scope so recursive calls generate proper `apply 'name'/arity` instead of undefined variables.

### 2. ~~Illegal Guard Expressions~~ ✅ FIXED

~~**Issue**: Pattern guards with function calls are rejected by Core Erlang.~~

**Fixed**: The compiler now automatically transforms guards with function calls into nested case expressions in the body, preserving OCaml's fallthrough semantics:
```erlang
case Opt of
  {some, X} -> case apply Pred (X) of
                 'true' -> {some, X};
                 'false' -> <fallback to next clause>
               end;
  _ -> none
end
```

### 3. ~~Function Aliases~~ ✅ FIXED

~~**Issue**: `let f = g` generates incorrect code.~~

**Fixed**: The compiler now detects function aliases and generates proper wrapper functions with matching arity.

### 4. ~~Module Shadowing~~ ✅ FIXED

~~**Issue**: Modules named `process`, `supervisor`, etc. would shadow Erlang/OTP modules at runtime, causing kernel crashes when loaded with `-pa`.~~

**Fixed**: All compiled modules now use a `Merlot.` prefix (e.g., `'Merlot.Process'`, `'Merlot.Supervisor'`). This follows Elixir's convention (`'Elixir.Module'`) for namespace isolation. Output files are named accordingly (`Merlot.Process.beam`).

### 5. Module Resolution from Stdlib Directory

**Issue**: Compiling stdlib modules from within the stdlib directory causes `Env.Error` due to conflicts with pre-compiled `.cmi` files.

**Workaround**: Compile from project root, not from stdlib directory.

## Tasks

### High Priority

- [x] **Fix nested `let rec`** - Emit proper `letrec` bindings or lift to module level
- [x] **Fix guard expressions** - Move function calls from guards to case bodies
- [x] **Fix function aliases** - Generate proper wrapper functions
- [x] **Fix module shadowing** - Add `Merlot.` prefix to prevent OTP conflicts

### Medium Priority

- [x] **Stdlib BEAM compilation** - Makefile updated for `Merlot.*.beam` output
- [ ] **Test stdlib functions** - Add tests that exercise the OCaml implementations
- [ ] **Remove external workarounds** - Some functions use `Lists.reverse` etc. to avoid OCaml implementations

### Low Priority

- [ ] **Optimize generated code** - Avoid unnecessary wrapper functions
- [ ] **Add missing functions** - Complete OCaml stdlib API coverage

## Module Naming Strategy

All Merlot modules are compiled with a `Merlot.` prefix to prevent shadowing Erlang/OTP modules. This follows Elixir's convention where all modules are prefixed with `Elixir.`.

### Output Files

| Source | Module Name | Output Files |
|--------|-------------|--------------|
| `process.ml` | `'Merlot.Process'` | `Merlot.Process.core`, `Merlot.Process.beam` |
| `supervisor.ml` | `'Merlot.Supervisor'` | `Merlot.Supervisor.core`, `Merlot.Supervisor.beam` |
| `actor.ml` | `'Merlot.Actor'` | `Merlot.Actor.core`, `Merlot.Actor.beam` |

### Erlang Usage

```erlang
%% Load Merlot modules without shadowing OTP
erl -pa /path/to/merlot/stdlib

%% OTP supervisor still works
supervisor:start_link(...).

%% Merlot modules use prefix
'Merlot.Process':spawn_link(Fun).
'Merlot.Supervisor':count_children(Sup).
```

### Module Categories

#### 1. External-Only Modules (No BEAM Output)

These modules only provide `external` declarations that map directly to Erlang/Elixir calls. They don't need BEAM files.

| Module | Maps To | Notes |
|--------|---------|-------|
| `lists` | Erlang `lists` | Direct mapping to Erlang stdlib |
| `io` | Erlang `io` | Direct mapping to Erlang stdlib |
| `erlang` | Erlang BIFs | Direct mapping to Erlang BIFs |
| `timer` | Erlang `timer` | Direct mapping to Erlang stdlib |
| `list` | Erlang `lists` | OCaml-style API forwarding to `lists` |
| `string` | Erlang string fns | OCaml-style API |

#### 2. Compiled Modules (BEAM Output)

These modules have OCaml implementations and are compiled to BEAM bytecode with the `Merlot.` prefix.

| Module | BEAM File | Notes |
|--------|-----------|-------|
| `actor` | `Merlot.Actor.beam` | Actor abstraction |
| `enum` | `Merlot.Enum.beam` | Enumerable operations (externals) |
| `keyword` | `Merlot.Keyword.beam` | Keyword lists |
| `map` | `Merlot.Map.beam` | Erlang maps |
| `option` | `Merlot.Option.beam` | Option type (31 functions) |
| `process` | `Merlot.Process.beam` | Process operations |
| `result` | `Merlot.Result.beam` | Result type (29 functions) |
| `supervisor` | `Merlot.Supervisor.beam` | OTP supervisor |

## Modules Status

| Module | Externals | Implementations | BEAM | Status |
|--------|-----------|-----------------|------|--------|
| List | 15 | 27 | ❌ OCaml conflict | Externals only |
| Option | 0 | 31 | ✅ `Merlot.Option` | Fully compiled |
| Result | 0 | 29 | ✅ `Merlot.Result` | Fully compiled |
| String | 34 | 15 | ❌ OCaml conflict | Externals only |
| Enum | 39 | 0 | ✅ `Merlot.Enum` | Externals only (no impl) |
| Map | 20 | 0 | ✅ `Merlot.Map` | Externals only (no impl) |
| Keyword | 17 | 1 | ✅ `Merlot.Keyword` | Mostly externals |
| Lists | 34 | 6 | ❌ Erlang conflict | Externals only |
| Process | 14 | 6 | ✅ `Merlot.Process` | Compiled |
| Actor | 4 | 3 | ✅ `Merlot.Actor` | Compiled |
| Supervisor | 3 | 2 | ✅ `Merlot.Supervisor` | Compiled |
| Io | 10 | 0 | ❌ Erlang conflict | Externals only |
| Erlang | 30 | 0 | ❌ Erlang conflict | Externals only |
| Timer | 8 | 0 | ❌ Erlang conflict | Externals only |

## Notes

All four high-priority compiler limitations have been fixed. The compiler now handles:
- Nested `let rec` with proper Core Erlang `letrec` bindings
- Guards with function calls by transforming them into nested case expressions
- Function aliases by generating wrapper functions with correct arity
- Variable names with apostrophes (e.g., `r'` → `R_prime`) via sanitization
- Module namespacing with `Merlot.` prefix to prevent OTP shadowing
