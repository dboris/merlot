(* Person - An Elixir-compatible struct defined in Merlot

   This module demonstrates how to create Elixir structs from Merlot.
   The [@@@elixir_struct] attribute enables:
   - Module named Elixir.Person (not Merlot.Person)
   - type t compiles to a map with __struct__ key
   - Auto-generated __struct__/0 and __struct__/1 functions

   From Elixir, you can use this as:
     person = %Person{name: "Alice", age: 30}
     Person.birthday(person)
*)

[@@@elixir_struct]

(* The struct type - becomes an Elixir-compatible map *)
type t = {
  name: string;
  age: int;
}

(* Create a new person *)
let new_ name age = { name; age }

(* Get the person's name *)
let name p = p.name

(* Get the person's age *)
let age p = p.age

(* Increment age by one *)
let birthday p = { p with age = p.age + 1 }

(* Check if person is an adult *)
let is_adult p = p.age >= 18

(* Create a greeting string *)
let greet p =
  p.name
