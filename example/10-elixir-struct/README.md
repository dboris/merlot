# Elixir Struct Interop Demo

This demo shows bidirectional interoperability between Merlot and Elixir structs:

1. **Exporting** - Define Elixir-compatible structs from Merlot with `[@@@elixir_struct]`
2. **Importing** - Consume Elixir structs in Merlot with `[@@elixir]`

## What This Demonstrates

### Exporting Structs (Merlot → Elixir)

Using `[@@@elixir_struct]` module attribute:

- Module named `Elixir.Person` instead of `Merlot.Person`
- `type t` compiles to map with `__struct__` key
- Auto-generated `__struct__/0` and `__struct__/1` functions
- Callable from Elixir with full struct support

### Importing Structs (Elixir → Merlot)

Using `[@@elixir]` type attribute:

- Type-safe access to Elixir struct fields
- Field access compiles to `maps:get`
- Record creation compiles to map literals
- Full OCaml type checking

## Files

- `person.ml` - Exports `Person` struct to Elixir (`[@@@elixir_struct]`)
- `consumer.ml` - Imports Elixir structs into Merlot (`[@@elixir]`)
- `demo.exs` - Elixir script demonstrating the exported Person struct
- `Makefile` - Build automation

## Running the Demo

```bash
# Compile both modules
make

# Run the Elixir demo script
make test

# Show generated Core Erlang
make core
```

## Exporting: [@@@elixir_struct]

```ocaml
(* person.ml - Define an Elixir-compatible struct *)
[@@@elixir_struct]

type t = {
  name: string;
  age: int;
}

let new_ name age = { name; age }
let birthday p = { p with age = p.age + 1 }
let is_adult p = p.age >= 18
```

This compiles to an `Elixir.Person` module that Elixir can use:

```elixir
# From Elixir
alice = Person.new_("Alice", 25)
alice.name           # => "Alice"
Person.is_adult(alice)  # => true
```

## Importing: [@@elixir]

```ocaml
(* consumer.ml - Import and use Elixir structs *)

type person = {
  name: string;
  age: int;
} [@@elixir]

(* Now use OCaml record syntax with Elixir structs *)
let greet (p : person) = p.name
let is_senior (p : person) = p.age >= 65
let with_age (p : person) new_age = { p with age = new_age }
```

This compiles to:

```erlang
'greet'/1 =
    fun (P) ->
      call 'maps':'get' ('name', P)

'with_age'/2 =
    fun (P, New_age) ->
      ~{'name' => call 'maps':'get' ('name', P), 'age' => New_age}~
```

## Explicit Module Names

For structs where the Elixir module name differs from the type name:

```ocaml
type response = {
  status: int;
  body: string;
} [@@elixir "Elixir.Plug.Conn.Response"]
```

## Expected Output

```
=== Merlot-Elixir Struct Interop Demo ===

1. Creating a person with Person.new_/2:
   Alice: %{name: "Alice", __struct__: Person, age: 25}

2. Checking struct type:
   Is map? true
   Has __struct__ key? true
   Struct module: Elixir.Person

3. Accessing struct fields:
   Name: Alice
   Age: 25

4. Calling Merlot functions:
   Person.name(alice) = Alice
   Person.age(alice) = 25
   Person.is_adult(alice) = true

5. Using Person.birthday/1:
   After birthday: %{name: "Alice", __struct__: Person, age: 26}

...

=== Demo Complete ===
```

## Notes

- `[@@@elixir_struct]` is a **module-level** attribute that affects the entire module
- `[@@elixir]` is a **type-level** attribute that affects only the annotated type
- Regular record types (without attributes) remain as efficient Erlang tuples
- The `%Person{}` literal syntax in Elixir requires compile-time module availability
