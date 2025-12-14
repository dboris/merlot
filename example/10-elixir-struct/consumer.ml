(* Consumer - Importing Elixir structs into Merlot

   This module demonstrates how to consume Elixir structs from Merlot
   using the [@@elixir] attribute on type declarations.

   The type becomes a map-backed record with:
   - Field access compiled to maps:get
   - Record creation compiled to map literals
   - Full type safety from OCaml's type system
*)

(* Import the Person struct defined in Elixir (or by person.ml with [@@@elixir_struct]) *)
type person = {
  name: string;
  age: int;
} [@@elixir]

(* Example: importing a hypothetical Elixir.User struct *)
type user = {
  email: string;
  username: string;
  active: bool;
} [@@elixir]

(* Functions that work with imported Elixir structs *)

(* Get greeting for a person *)
let greet (p : person) =
  p.name

(* Check if person is a senior *)
let is_senior (p : person) =
  p.age >= 65

(* Create a modified person with updated age *)
let with_age (p : person) new_age =
  { p with age = new_age }

(* Process a user - demonstrates working with another elixir type *)
let user_display (u : user) =
  u.username

(* Check if user is active *)
let is_active (u : user) =
  u.active
