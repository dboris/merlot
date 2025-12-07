# Process Example - Counter

A stateful counter process demonstrating Erlang-style concurrency in Merlot.

## Features

- Process spawning with `Process.spawn`
- Message passing with `Process.send`
- Selective receive with pattern matching
- Polymorphic variants for typed messages
- Tail-recursive process loops for state

## Build & Run

```bash
# Compile to BEAM
make

# Run
make run
```

Expected output:
```
Result: 2
```

## How it works

The counter process maintains an integer state and responds to messages:

| Message | Effect |
|---------|--------|
| `` `Inc `` | Increment counter |
| `` `Dec `` | Decrement counter |
| `` `Get pid `` | Send current value to pid |
| `` `Stop `` | Stop and return final value |

```ocaml
let rec counter count =
  Process.receive (function
    | `Inc -> counter (count + 1)
    | `Dec -> counter (count - 1)
    | `Get pid ->
        let _ = Process.send pid (`Value count) in
        counter count
    | `Stop -> count)
```
