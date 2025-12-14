# Merlot

OCaml to BEAM/Erlang compiler.

Merlot compiles OCaml source code to BEAM bytecode for the Erlang VM.

## Features

- **Arithmetic & Conditionals** - integers, floats, booleans, if/then/else
- **Data Types** - tuples, lists, records, variants, polymorphic variants
- **Pattern Matching** - match expressions with guards
- **Functions** - higher-order functions, recursion, tail-call optimization
- **Pipe Operator** - `|>` for data pipelines
- **Printf** - type-safe `Printf.sprintf` and `Printf.printf` support
- **Pretty-Printing** - auto-generated `pp_<type>` functions for all types
- **Exceptions** - `try`/`with`, `raise`, `failwith` for error handling
- **Macros** - Elixir-style compile-time metaprogramming with quote/unquote
- **Erlang/Elixir FFI** - call any BEAM function via `external` declarations
- **Standard Library** - bindings to Erlang and Elixir modules (Enum, String, Map, etc.)
- **Elixir Structs** - bidirectional struct interop with `[@@@elixir_struct]` and `[@@elixir]`
- **Process Primitives** - spawn, receive, send for Erlang-style concurrency

## Installation

Requires OCaml 5.4+, Erlang/OTP, and Elixir.

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

## Macros

Merlot supports Elixir-style compile-time macros for metaprogramming. Macros transform code at compile time, enabling powerful abstractions without runtime overhead.

### Defining Macros

Use `Macro.defmacro` to define a macro. The compiler auto-generates a stub function for typechecking:

```ocaml
open Merlot_stdlib

(* A debug macro that prints an expression and its value *)
let debug = Macro.defmacro (fun expr ->
  Macro.quote (
    let value = Macro.unquote expr in
    Io.format "DEBUG: ~s = ~p~n" [Macro.to_string expr; value];
    value
  )
)

let example () =
  let x = 10 in
  debug (x * 2)  (* Prints: DEBUG: (X * 2) = 20 *)
```

### Macro Primitives

- **`Macro.defmacro`** - Define a macro with auto-generated stub
- **`Macro.quote`** - Capture the AST of an expression
- **`Macro.unquote`** - Splice a macro parameter's AST into quoted code
- **`Macro.to_string`** - Convert a macro parameter to its source text
- **`[@expose]`** - Attribute to bypass hygiene and expose a binding to the caller (use sparingly)

### How It Works

When you write:
```ocaml
let result = debug (compute x y)
```

The macro expands at compile time to:
```ocaml
let result =
  let value = compute x y in
  Io.format "DEBUG: ~s = ~p~n" ["compute(X, Y)"; value];
  value
```

### Hygiene

Merlot macros are hygienic - variables introduced by the macro don't clash with user code. Internal variables are automatically renamed with unique suffixes.

When you need to intentionally expose a variable to the caller, use the `[@expose]` attribute on the let binding:

```ocaml
(* Macro that provides a 'resource' binding to the body *)
let with_resource = Macro.defmacro (fun body ->
  Macro.quote (
    let resource [@expose] = acquire_resource () in
    let result = Macro.unquote body in
    release_resource resource;
    result
  )
)

(* Usage - 'resource' is available in the body *)
with_resource (use resource)
```

### Comparison with Elixir

| Elixir | Merlot |
|--------|--------|
| `defmacro debug(expr)` | `let debug = Macro.defmacro (fun expr -> ...)` |
| `quote do ... end` | `Macro.quote (...)` |
| `unquote(expr)` | `Macro.unquote expr` |
| `Macro.to_string(expr)` | `Macro.to_string expr` |
| `var!(name)` | `let name [@expose] = ...` |

## Pretty-Printing and Debugging

### Inspect.pp - Type-Aware Pretty Printing

Use `Inspect.pp` to convert any value to an OCaml-style string representation. The compiler automatically determines the type and generates the appropriate formatting code:

```ocaml
open Merlot_stdlib

type person = { name: string; age: int }
type color = Red | Green | Blue
type shape = Circle of float | Rectangle of float * float

let main () =
  let alice = { name = "Alice"; age = 30 } in

  (* Just use Inspect.pp - no boilerplate! *)
  Inspect.puts (Inspect.pp alice);           (* {name = "Alice"; age = 30} *)
  Inspect.puts (Inspect.pp Green);           (* Green *)
  Inspect.puts (Inspect.pp (Circle 5.0));    (* Circle 5.00000 *)
  Inspect.puts (Inspect.pp (Rectangle (3.0, 4.0)));  (* Rectangle (3.00000, 4.00000) *)

  (* Works with primitives too *)
  Inspect.puts (Inspect.pp 42);              (* 42 *)
  Inspect.puts (Inspect.pp "hello");         (* "hello" *)
  Inspect.puts (Inspect.pp true);            (* true *)
```

`Inspect.pp` is a compiler intrinsic - at compile time, the compiler inspects the argument's type and generates inline formatting code. For user-defined types (records and variants), it calls the auto-generated `pp_<typename>` functions.

### Inspect Module

The `Inspect` module provides additional debugging utilities:

```ocaml
open Merlot_stdlib

(* Print any value (Erlang format with binary wrapper) *)
Inspect.print value          (* shows: {person,<<"Alice">>,30} *)

(* Print a string cleanly (no wrapper) *)
Inspect.puts "hello"         (* shows: hello *)
Inspect.puts (Inspect.pp p)  (* shows: {name = "Alice"; age = 30} *)

(* Pipeline-friendly: prints and returns the value *)
let result =
  compute_something ()
  |> Inspect.inspect         (* prints intermediate value *)
  |> further_processing

(* Labeled inspection *)
Inspect.inspect_label "result" value  (* prints: result: <value> *)

(* Convert to string (Erlang format) *)
let s = Inspect.to_string value
```

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

**OCaml-compatible:**
- **List** - OCaml List API backed by Erlang lists
- **Option** - Option type with combinators
- **Result** - Result type with combinators

**Debugging:**
- **Inspect** - Term inspection with `print`, `puts`, `inspect`, `to_string`

**Metaprogramming:**
- **Macro** - Compile-time macros with quote/unquote

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
- **String** - upcase, downcase, trim, split, slice, replace, Unicode support
- **Map** - get, put, delete, merge, keys, values, etc.
- **Keyword** - keyword list operations

## Elixir Struct Interop

Use `[@@@elixir_struct]` to create Elixir-compatible structs from Merlot:

```ocaml
[@@@elixir_struct]

type t = {
  name: string;
  age: int;
}

let new_ name age = { name; age }
let name p = p.name
let age p = p.age
let birthday p = { p with age = p.age + 1 }
let is_adult p = p.age >= 18
```

This enables:

- **Elixir module naming** - Module is `Elixir.Person` instead of `Merlot.Person`
- **Map-based structs** - `type t` compiles to a map with `__struct__` key
- **Auto-generated functions** - `__struct__/0` and `__struct__/1` for Elixir compatibility
- **Efficient helpers** - Other record types remain as tuples

From Elixir, you can use the Merlot-defined struct:

```elixir
# Load the module
Code.append_path(".")
:code.load_abs(~c"./Elixir.Person")

# Create and use structs
alice = Person.new_("Alice", 25)
IO.puts("Name: #{alice.name}")           # Field access works
IO.puts("Adult? #{Person.is_adult(alice)}")

# Pattern matching
case alice do
  %{__struct__: Person, name: name} -> IO.puts("Hello #{name}")
end

# Using __struct__/1 with keyword list
bob = Person.__struct__(name: "Bob", age: 30)
```

### Importing Elixir Structs

Use `[@@elixir]` on a type to consume Elixir structs with type safety:

```ocaml
(* Import an Elixir struct type *)
type user = {
  name: string;
  email: string;
  age: int;
} [@@elixir]

(* Use OCaml record syntax - compiles to map operations *)
let greet (u : user) = u.name
let is_adult (u : user) = u.age >= 18
let with_age (u : user) new_age = { u with age = new_age }
```

For explicit module names: `[@@elixir "Elixir.MyApp.User"]`

See `example/10-elixir-struct/` for a complete demo.

## Limitations

Merlot is an experimental compiler. The following OCaml features are not yet supported:

**Not Supported:**
- **Modules and Functors** - OCaml's module system (signatures, functors, first-class modules) is not implemented. Use separate `.ml` files for modularity.
- **Objects and Classes** - OCaml's object-oriented features are not supported.
- **Mutable State** - `ref` and mutable record fields are not supported. Use Erlang processes for state.
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
│   ├── result.ml        # OCaml-compatible Result
│   ├── macro.ml         # Compile-time macros
│   └── inspect.ml       # Debugging and inspection
├── test/
│   ├── cases/           # Test input files (.ml)
│   └── expected/        # Expected outputs (.core)
└── example/             # Example projects
```

## License

MIT
