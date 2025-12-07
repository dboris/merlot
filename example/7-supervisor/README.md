# Supervisor Example - Fault-Tolerant Workers

Demonstrates the Supervisor module for building fault-tolerant applications.

## What is a Supervisor?

A Supervisor is a process that:
- Starts and monitors child processes
- Automatically restarts children when they crash
- Enables the "let it crash" philosophy

This is similar to Erlang's supervisor behavior.

## Restart Strategies

| Strategy | Behavior |
|----------|----------|
| `one_for_one` | Only restart the crashed child |
| `one_for_all` | Restart all children if one crashes |
| `rest_for_one` | Restart crashed child and all after it |

## The Example

```ocaml
(* Define workers *)
let children = [
  Supervisor.worker "worker_a" (Actor.spec 0 handler);
  Supervisor.worker "worker_b" (Actor.spec 0 handler);
]

(* Start with one-for-one strategy *)
let sup = Supervisor.start_link Supervisor.one_for_one children

(* When worker_a crashes, only worker_a restarts *)
Process.send worker_a `Crash
(* Supervisor automatically restarts worker_a with fresh state *)
```

## Build & Run

```bash
make
make run
```

Expected output shows:
- Initial ping responses from all workers
- Count before crash (after increments)
- Count after restart (reset to 0)
- Confirmation that pid changed (restart happened)

## API Reference

| Function | Description |
|----------|-------------|
| `Supervisor.start strategy children` | Start supervisor |
| `Supervisor.start_link strategy children` | Start and link |
| `Supervisor.worker id start_fn` | Create permanent child spec |
| `Supervisor.temporary_worker id start_fn` | Never restart |
| `Supervisor.transient_worker id start_fn` | Restart on crash only |
| `Supervisor.stop sup` | Stop supervisor and children |
| `Supervisor.which_children sup` | Get (id, pid) list |

## Child Restart Types

| Type | Behavior |
|------|----------|
| `Permanent` | Always restart (default) |
| `Temporary` | Never restart |
| `Transient` | Restart on abnormal exit only |
