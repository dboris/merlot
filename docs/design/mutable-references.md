# Design: Mutable References via Process Dictionary

## Overview

OCaml's mutable references (`ref`, `:=`, `!`) are fundamental to imperative code patterns, including `for` loops, `while` loops, and stateful algorithms. This document proposes implementing OCaml refs using Erlang's process dictionary.

## OCaml Reference Semantics

```ocaml
let x = ref 0       (* Create reference with initial value 0 *)
x := 5              (* Update reference to 5 *)
let v = !x          (* Dereference to get value 5 *)
```

References are:
- Mutable cells holding a single value
- Identity-based (two refs with same value are distinct)
- Process-local (not shared across processes)

## Erlang Process Dictionary

The process dictionary is a per-process key-value store:

```erlang
put(Key, Value) -> OldValue | undefined
get(Key) -> Value | undefined
erase(Key) -> Value | undefined
```

Properties:
- O(1) access
- Process-local (natural fit for OCaml semantics)
- Survives function calls within the process
- Automatically cleaned up on process exit

## Proposed Implementation

### Representation

An OCaml `ref` becomes a unique atom key into the process dictionary:

```ocaml
ref 42
```

Compiles to:

```erlang
%% Generate unique ref ID
RefKey = merlot_ref:new(42),
%% RefKey is an atom like '$ref_1', '$ref_2', etc.
```

### Operations

| OCaml | Core Erlang |
|-------|-------------|
| `ref v` | `call 'merlot_ref':'new' (V)` |
| `!r` | `call 'merlot_ref':'get' (R)` |
| `r := v` | `call 'merlot_ref':'set' (R, V)` |

### Runtime Module

```erlang
%% priv/merlot_ref.erl
-module(merlot_ref).
-export([new/1, get/1, set/2]).

%% Counter stored in process dictionary under special key
-define(REF_COUNTER, '$merlot_ref_counter').

%% Create a new reference with initial value
new(Value) ->
    Counter = case get(?REF_COUNTER) of
        undefined -> 0;
        N -> N
    end,
    Key = list_to_atom("$ref_" ++ integer_to_list(Counter)),
    put(?REF_COUNTER, Counter + 1),
    put(Key, Value),
    Key.

%% Dereference - get current value
get(Key) ->
    erlang:get(Key).

%% Assignment - set new value, return unit
set(Key, Value) ->
    put(Key, Value),
    ok.
```

### Compiler Changes

In `typed_to_beam.ml`, detect ref operations:

```ocaml
(* In convert_apply or convert_expr *)
| Texp_apply (func, args) ->
    match get_ref_operation func args with
    | Some (Ref_new, [init_val]) ->
        Expr_call {
          module_ = Expr_lit (Lit_atom "merlot_ref");
          func = Expr_lit (Lit_atom "new");
          args = [convert_expr init_val];
        }
    | Some (Ref_deref, [ref_expr]) ->
        Expr_call {
          module_ = Expr_lit (Lit_atom "merlot_ref");
          func = Expr_lit (Lit_atom "get");
          args = [convert_expr ref_expr];
        }
    | Some (Ref_assign, [ref_expr; new_val]) ->
        Expr_call {
          module_ = Expr_lit (Lit_atom "merlot_ref");
          func = Expr_lit (Lit_atom "set");
          args = [convert_expr ref_expr; convert_expr new_val];
        }
    | None -> (* regular application *)
```

Detection requires checking for:
- `Stdlib.ref` or `Pervasives.ref` for creation
- `Stdlib.(!)` or prefix `!` for dereference
- `Stdlib.(:=)` for assignment

## Example Transformations

### For Loop with Accumulator

```ocaml
let sum_to n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  !total
```

Compiles to:

```erlang
'sum_to'/1 =
    fun (N) ->
      let <Total> = call 'merlot_ref':'new' (0) in
      do letrec 'for_loop'/1 = fun (I) ->
           case call 'erlang':'=<' (I, N) of
             <'true'> when 'true' ->
               do call 'merlot_ref':'set' (Total,
                    call 'erlang':'+' (call 'merlot_ref':'get' (Total), I))
               apply 'for_loop'/1 (call 'erlang':'+' (I, 1))
             <'false'> when 'true' -> 'ok'
           end
         in apply 'for_loop'/1 (1)
      call 'merlot_ref':'get' (Total)
```

### While Loop

```ocaml
let countdown n =
  let i = ref n in
  let result = ref [] in
  while !i > 0 do
    result := !i :: !result;
    i := !i - 1
  done;
  !result
```

Compiles to:

```erlang
'countdown'/1 =
    fun (N) ->
      let <I> = call 'merlot_ref':'new' (N) in
      let <Result> = call 'merlot_ref':'new' ([]) in
      do letrec 'while_loop'/0 = fun () ->
           case call 'erlang':'>' (call 'merlot_ref':'get' (I), 0) of
             <'true'> when 'true' ->
               do call 'merlot_ref':'set' (Result,
                    [call 'merlot_ref':'get' (I) | call 'merlot_ref':'get' (Result)])
               do call 'merlot_ref':'set' (I,
                    call 'erlang':'-' (call 'merlot_ref':'get' (I), 1))
               apply 'while_loop'/0 ()
             <'false'> when 'true' -> 'ok'
           end
         in apply 'while_loop'/0 ()
      call 'merlot_ref':'get' (Result)
```

## Alternatives Considered

### 1. ETS Tables

```erlang
%% Create table per module/process
ets:new(refs, [set, private])
ets:insert(refs, {Key, Value})
ets:lookup(refs, Key)
```

**Pros:** Explicit, can be shared across processes if needed
**Cons:** More overhead, requires cleanup, overkill for local refs

### 2. Transform to Pure Functional Style

Convert ref usage to state threading:

```ocaml
let sum_to n =
  let rec loop i total =
    if i > n then total
    else loop (i + 1) (total + i)
  in loop 1 0
```

**Pros:** Pure functional, no mutable state
**Cons:** Complex transformation, changes semantics for aliased refs

### 3. Process-per-ref (gen_server)

Each ref becomes a process holding state.

**Pros:** True isolation, can share across processes
**Cons:** Massive overhead, complex, changes semantics

## Decision: Process Dictionary

The process dictionary approach is chosen because:

1. **Semantics match**: Process-local, like OCaml refs
2. **Performance**: O(1) operations, minimal overhead
3. **Simplicity**: Direct mapping, easy to implement
4. **Cleanup**: Automatic on process exit
5. **Idiomatic**: Used by Erlang for similar purposes (e.g., random seed)

## Limitations

1. **Cross-process sharing**: Refs cannot be sent to other processes (but neither can OCaml refs meaningfully)

2. **Memory**: Refs persist until process exits or explicit cleanup. Long-running processes with many short-lived refs may accumulate garbage.

3. **Debugging**: Process dictionary is implicit state, harder to inspect than explicit data structures.

## Future Enhancements

1. **Ref cleanup**: Add `merlot_ref:delete/1` for explicit cleanup

2. **Weak refs**: Consider weak references for caches

3. **Debug mode**: Optional ref tracking for debugging memory leaks

4. **Alternative backends**: Command-line flag to use ETS instead for different use cases

## Implementation Plan

1. Create `priv/merlot_ref.erl` runtime module
2. Add ref detection in `typed_to_beam.ml`
3. Generate appropriate calls for ref operations
4. Update for-loop transformation to use refs properly
5. Add while-loop support using refs
6. Test with stdlib patterns that use refs
