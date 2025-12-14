(* Test [@@elixir] attribute for importing Elixir struct types *)

(* Import an Elixir User struct - infers Elixir.User from type name *)
type user = {
  name: string;
  email: string;
  age: int;
} [@@elixir]

(* Import with explicit module name *)
type response = {
  status: int;
  body: string;
} [@@elixir "Elixir.Plug.Conn.Response"]

(* Regular record for comparison - should remain a tuple *)
type point = {
  x: int;
  y: int;
}

(* Field access on elixir type - should use maps:get *)
let get_user_name (u : user) = u.name
let get_user_age (u : user) = u.age

(* Field access on explicit import - should use maps:get *)
let get_response_status (r : response) = r.status

(* Field access on regular record - should use element/2 *)
let get_point_x (p : point) = p.x

(* Record creation for elixir type - should produce a map *)
let make_user name email age = { name; email; age }

(* Record creation for regular type - should produce a tuple *)
let make_point x y = { x; y }

(* Record update on elixir type - should use map operations *)
let update_user_age (u : user) new_age = { u with age = new_age }
