# Actor Example - Counter

Demonstrates the Actor module for building stateful OTP-style processes.

## What is an Actor?

An Actor is a stateful process that:
- Holds state that persists between messages
- Handles messages via a handler function
- Returns whether to continue or stop after each message

This is similar to Erlang's gen_server or Gleam's actors.

## The Counter Actor

```ocaml
(* Handler: state -> message -> next_state *)
let counter_handler state msg =
  match msg with
  | Inc -> Actor.continue (state + 1)
  | Dec -> Actor.continue (state - 1)
  | Get caller ->
      let _ = Process.send caller state in
      Actor.continue state
  | Stop -> Actor.stop state

(* Start with initial state *)
let counter = Actor.start 0 counter_handler
```

## Build & Run

```bash
make
make run
```

Expected output:
```
Result: {3,13,12,0}
```

- 3: After three increments
- 13: After adding 10
- 12: After decrement
- 0: After reset

## API Reference

| Function | Description |
|----------|-------------|
| `Actor.start init handler` | Start actor with initial state |
| `Actor.start_link init handler` | Start and link to current process |
| `Actor.send actor msg` | Send async message (no response) |
| `Actor.continue state` | Continue with new state |
| `Actor.stop state` | Stop the actor |
