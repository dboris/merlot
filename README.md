# Merlot

OCaml to BEAM/Erlang compiler.

Merlot compiles OCaml source code to BEAM bytecode for the Erlang VM.

## Features

- **Arithmetic & Conditionals** - integers, floats, booleans, if/then/else
- **Data Types** - tuples, lists, records, variants, polymorphic variants
- **Pattern Matching** - match expressions with guards
- **Functions** - higher-order functions, recursion, tail-call optimization
- **Pipe Operator** - `|>` for Elixir-style data pipelines
- **Printf** - type-safe `Printf.sprintf` and `Printf.printf` support
- **Erlang FFI** - call Erlang functions via `external` declarations
- **Elixir FFI** - bindings to Elixir's Enum, String, Map modules
- **Process Primitives** - spawn, receive, send for Erlang-style concurrency

## Installation

Requires OCaml 5.2+, Erlang/OTP, and Elixir.

```bash
opam install . --deps-only
dune build
```

## Quick Start

Create a new project:

```bash
merlot init myapp
cd myapp
make run
```

This creates a project with:
```
myapp/
├── src/main.ml    # Entry point
├── ebin/          # Compiled .beam files
├── Makefile       # Build script
├── .merlin        # ocaml-lsp configuration
├── .gitignore
└── README.md
```

## Usage

```bash
# Create a new project
merlot init <project-name>

# Compile a single file
merlot hello.ml

# Compile to a specific output directory
merlot hello.ml -o ebin/

# Emit Core Erlang instead (for debugging)
merlot hello.ml --emit-core

# Run in Erlang shell (module name is Merlot.<Capitalized>)
erl -noshell -pa ebin -eval "'Merlot.Hello':main(ok), halt()."
```

**CLI Options:**
- `-o <path>` - Output file or directory
- `-I <dir>` - Add directory to module search path
- `--emit-core` - Emit Core Erlang (.core) instead of BEAM
- `--emit-docs` - Generate documentation (.md and EEP-48 .chunk)
- `--no-stdlib` - Don't include Merlot_stdlib

## Example

```ocaml
(* hello.ml *)
let greet name =
  name

let main () =
  greet "world"
```

Compiles to Core Erlang (module name is `Merlot.Hello`):

```erlang
module 'Merlot.Hello' ['greet'/1, 'main'/1]
    attributes []

'greet'/1 =
    fun (Name) ->
      Name
'main'/1 =
    fun (_) ->
      apply 'greet'/1 (<<"world">>)
end
```

## Erlang FFI

Call Erlang functions using `external` declarations:

```ocaml
external reverse : 'a list -> 'a list = "lists" "reverse"
external length : 'a list -> int = "erlang" "length"

let example () =
  let xs = [1; 2; 3] in
  length (reverse xs)
```

## Process Primitives

Erlang-style concurrency with spawn, receive, and send:

```ocaml
external spawn : (unit -> 'a) -> int = "erlang" "spawn"
external self : unit -> int = "erlang" "self"
external send : int -> 'a -> 'a = "erlang" "!"
external receive : ('a -> 'b) -> 'b = "erlang" "receive"

let rec counter count =
  receive (function
    | `Inc -> counter (count + 1)
    | `Dec -> counter (count - 1)
    | `Get pid ->
        let _ = send pid count in
        counter count
    | `Stop -> count)

let start_counter () =
  spawn (fun () -> counter 0)
```

## Selective Receive

Erlang's selective receive lets you match messages in any order from the mailbox. Messages that don't match stay in the mailbox for later:

```ocaml
external make_ref : unit -> int = "erlang" "make_ref"
external receive_timeout : int -> ('a -> 'b) -> 'b = "erlang" "receive_timeout"

(* Reference-based request/response pattern *)
let call server request =
  let ref = make_ref () in
  let me = self () in
  let _ = send server (`Call (ref, me, request)) in
  (* Only receive responses matching our reference *)
  receive (function
    | `Reply (r, response) when r = ref -> response)

(* Priority handling with timeout *)
let rec priority_loop () =
  receive_timeout 0 (function
    | `High msg -> handle_high msg; priority_loop ()
    | `Low _ -> priority_loop ())
```

Guards (`when` clauses) are fully supported and compile to Core Erlang guards.

## Printf Support

OCaml's type-safe `Printf` module is supported. Format strings are validated at compile time and converted to Erlang's `io_lib:format`:

```ocaml
let main () =
  (* sprintf returns a string *)
  let greeting = Printf.sprintf "Hello %s!" "world" in
  let info = Printf.sprintf "Name: %s, Age: %d" "Alice" 30 in
  let pi = Printf.sprintf "Pi: %.2f" 3.14159 in

  (* printf prints to stdout *)
  Printf.printf "Result: %s\n" greeting;

  (greeting, info, pi)
```

**Supported format specifiers:**
- `%s` - strings
- `%d`, `%i` - integers
- `%f` - floats
- `%.Nf` - floats with N decimal places (e.g., `%.2f`)
- `%c` - characters
- `%b` - booleans
- `%%` - literal percent sign

## Testing

```bash
dune runtest
```

## Standard Library

The standard library is automatically included. Use `open Merlot_stdlib` to access Erlang bindings:

```ocaml
open Merlot_stdlib

let echo_server () =
  Process.receive (function
    | `Echo (pid, msg) ->
        let _ = Process.send pid msg in
        echo_server ()
    | `Stop -> ())

let main () =
  let pid = Process.spawn echo_server in
  let me = Process.self () in
  let _ = Process.send pid (`Echo (me, "hello")) in
  Process.receive (function msg -> msg)
```

Compile:
```bash
merlot your_file.ml
```

Use `--no-stdlib` to disable automatic inclusion if needed.

Available modules:

**Erlang/OTP:**
- **Process** - spawn, receive, send, make_ref, link, monitor
- **Erlang** - type checks, list ops, conversions, system functions
- **Lists** - reverse, sort, flatten, keyfind, zip, seq
- **Timer** - send_after, sleep, send_interval
- **Io** - put_chars, format, get_line
- **Actor** - gen_server-style actors with state and message handling
- **Supervisor** - process supervision trees

**Elixir:**
- **Enum** - map, filter, reduce, sort, take, drop, zip, etc.
- **String** - upcase, downcase, trim, split, slice, replace, etc.
- **Map** - get, put, delete, merge, keys, values, etc.
- **Keyword** - keyword list operations

**OCaml-compatible:**
- **List** - OCaml List API backed by Erlang lists
- **Option** - Option type with combinators
- **Result** - Result type with combinators

## Limitations

Merlot is an experimental compiler. The following OCaml features are not yet supported:

**Not Supported:**
- **Modules and Functors** - OCaml's module system (signatures, functors, first-class modules) is not implemented. Use separate `.ml` files for modularity.
- **Objects and Classes** - OCaml's object-oriented features are not supported.
- **Exceptions** - `try`/`with` and `raise` are not implemented. Use `Result` or `Option` types instead.
- **Mutable State** - `ref` and mutable record fields are not supported. Use Erlang processes for state.
- **Extensible Variants** - `type t = ..` and `exception` declarations are not supported.
- **Local Modules** - `let module M = ...` is not supported.
- **Recursive Modules** - `module rec` is not supported.
- **PPX** - Preprocessing extensions are not available.

**Supported (with caveats):**
- **Arrays** - Supported via Erlang tuples. `Array.get`, `Array.set`, `Array.length`, `Array.make` work.
- **Lazy Values** - `lazy e` creates a thunk, `Lazy.force` evaluates it.
- **Binding Operators** - `let*` and `let+` work for monadic code. `and*` is not yet supported.
- **Local Opens** - `let open M in ...` works for opening modules in expressions.

**Partial Support:**
- **Standard Library** - Only `Printf.sprintf` and `Printf.printf` from OCaml's stdlib work. Use `Merlot_stdlib` for Erlang/Elixir interop.
- **Labeled Arguments** - Labels are erased; `~foo:x` becomes positional argument `x`.
- **Optional Arguments** - `?foo:default` parameters work; call-site encoding is `'none'` or `{'some', value}`.
- **Polymorphic Variants** - Supported, but open variants (`[> ...]`, `[< ...]`) may have limitations.

**By Design:**
- **Native Integers** - OCaml's `int` maps to Erlang integers (arbitrary precision, not 63-bit).
- **Strings** - Compiled as Erlang binaries (like Elixir). Compatible with stdlib `String` module.
- **No Shared Memory** - Erlang processes don't share memory; data is copied on send.

## Project Structure

```
merlot/
├── bin/main.ml          # CLI entry point
├── lib/
│   ├── beam_ir.ml       # BEAM intermediate representation
│   ├── compile.ml       # OCaml parsing and type-checking
│   ├── core_erlang.ml   # Core Erlang pretty printer
│   ├── typed_to_beam.ml # TypedTree to BEAM IR conversion
│   └── merlot.ml        # Library exports
├── stdlib/              # Standard library
│   ├── process.ml       # Process primitives
│   ├── erlang.ml        # Low-level BIFs
│   ├── lists.ml         # Erlang list operations
│   ├── timer.ml         # Timer functions
│   ├── io.ml            # I/O operations
│   ├── actor.ml         # Gen_server-style actors
│   ├── supervisor.ml    # Process supervision
│   ├── enum.ml          # Elixir Enum module
│   ├── string.ml        # Elixir String module
│   ├── map.ml           # Elixir Map module
│   ├── keyword.ml       # Elixir Keyword lists
│   ├── list.ml          # OCaml-compatible List
│   ├── option.ml        # OCaml-compatible Option
│   └── result.ml        # OCaml-compatible Result
├── test/
│   ├── cases/           # Test input files (.ml)
│   └── expected/        # Expected outputs (.core)
└── example/             # Example projects
```

## License

MIT
