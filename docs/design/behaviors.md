# Behaviors in Merlot - Design Document

## Problem

OTP behaviors (gen_server, supervisor, gen_statem) are the foundation of robust Erlang applications. They provide:
- Process lifecycle management
- Standardized callback patterns
- Integration with supervision trees
- Hot code reloading support
- Debugging/tracing via sys module

Currently, Merlot can call Erlang's `gen_server:start_link` etc., but can't easily implement the callback modules that behaviors require.

## Erlang Behavior Pattern

In Erlang, a behavior consists of:

```erlang
-module(my_server).
-behaviour(gen_server).  % Declares the behavior
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% Callbacks
init(Args) -> {ok, InitialState}.
handle_call(Request, From, State) -> {reply, Response, NewState}.
handle_cast(Msg, State) -> {noreply, NewState}.
handle_info(Info, State) -> {noreply, NewState}.
terminate(Reason, State) -> ok.
```

## Proposed Merlot Design

### Option A: Behavior Modules with Annotations

Use OCaml attributes to declare behaviors:

```ocaml
(* my_server.ml *)
[@@@behavior gen_server]

type state = { count : int; name : string }

let init args =
  GenServer.ok { count = 0; name = args }

let handle_call request _from state =
  match request with
  | `Get -> GenServer.reply state.count state
  | `Inc -> GenServer.reply (state.count + 1) { state with count = state.count + 1 }

let handle_cast msg state =
  match msg with
  | `Reset -> GenServer.noreply { state with count = 0 }

let handle_info _info state =
  GenServer.noreply state

let terminate _reason _state = ()
```

The compiler would:
1. Recognize `[@@@behavior gen_server]`
2. Generate proper `-behaviour(gen_server)` attribute
3. Export callbacks with correct arities
4. Transform return values to Erlang tuples

### Option B: GenServer as a Functor

Use OCaml's module system:

```ocaml
module MyServer = GenServer.Make(struct
  type state = int
  type call = GetCount | Inc
  type cast = Reset

  let init () = 0

  let handle_call req state =
    match req with
    | GetCount -> `Reply (state, state)
    | Inc -> `Reply (state + 1, state + 1)

  let handle_cast msg state =
    match msg with
    | Reset -> `NoReply 0
end)

(* Usage *)
let main () =
  let pid = MyServer.start_link () in
  let count = MyServer.call pid GetCount in
  count
```

### Option C: Record-Based Callbacks (Gleam-style)

Like Gleam's actors, use records with handlers:

```ocaml
type state = int

let server = GenServer.define {
  init = (fun () -> 0);
  handle_call = (fun req state ->
    match req with
    | `Get -> `Reply (state, state)
    | `Inc -> `Reply (state + 1, state + 1));
  handle_cast = (fun msg state ->
    match msg with
    | `Reset -> `NoReply 0);
}

let main () =
  let pid = GenServer.start_link server () in
  let count = GenServer.call pid `Get in
  count
```

## Compilation Strategy

For any option, the compiler needs to:

1. **Generate callback exports**: Transform OCaml functions to Erlang exports with correct arities

2. **Transform return values**:
   - `GenServer.reply x state` → `{reply, X, State}`
   - `GenServer.noreply state` → `{noreply, State}`
   - `GenServer.stop reason` → `{stop, Reason, State}`

3. **Handle the From parameter**: In gen_server, `handle_call/3` receives a `From` tuple for replies. Need to expose this properly.

4. **Generate module attributes**: `-behaviour(gen_server)` in Core Erlang

## Implementation Plan

### Phase 1: GenServer Return Type Helpers
Add external declarations for return value construction:
```ocaml
external reply : 'a -> 'state -> ('a, 'state) call_result = "gen_server" "reply"
external noreply : 'state -> 'state cast_result = "gen_server" "noreply"
```

### Phase 2: Callback Module Generation
Recognize when a module exports `init`, `handle_call`, etc. and:
- Add `-behaviour` attribute
- Transform exports to correct format

### Phase 3: Type-Safe Wrapper
Provide `GenServer.start_link` that knows about the module's types.

## Alternative: Compile Stdlib to BEAM

Instead of behaviors, we could:
1. Compile Actor.ml to actor.beam
2. Include compiled stdlib with Merlot

This requires:
- Dependency resolution for stdlib modules
- Build system for stdlib -> BEAM
- Runtime path configuration

## Questions to Resolve

1. How to handle the `From` parameter for replies?
2. Should behaviors be compile-time or runtime?
3. How to integrate with existing Erlang supervision trees?
4. How to support hot code reloading?

## Recommended Approach

**Start with Option C (record-based)** because:
- Simplest to implement
- Type-safe handlers
- No special syntax needed
- Compatible with existing Merlot

Then add compile-time behavior support (Option A) for better OTP integration.
