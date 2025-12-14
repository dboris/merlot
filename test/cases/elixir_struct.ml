(* Test [@@@elixir_struct] attribute for Elixir-compatible struct generation *)

[@@@elixir_struct]

(* The main struct type - becomes a map with __struct__ key *)
type t = {
  name: string;
  age: int;
}

(* A helper record type - should remain a tuple, NOT a map *)
type point = {
  x: int;
  y: int;
}

(* Record creation for type t - should produce a map with __struct__ key *)
let create name age = { name; age }

(* Field access on type t - should use maps:get *)
let get_name p = p.name
let get_age p = p.age

(* Record update on type t - should produce map with updated fields *)
let birthday p = { p with age = p.age + 1 }

(* Helper record creation - should produce a tuple {point, X, Y} *)
let make_point x y = { x; y }

(* Field access on helper record - should use element/2 *)
let get_x pt = pt.x
let get_y pt = pt.y
