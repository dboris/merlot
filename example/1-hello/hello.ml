(* 1-hello: A comprehensive Merlot example

   This example demonstrates the OCaml features that Merlot can compile
   to BEAM/Erlang, including:
   - Basic functions and arithmetic
   - Pattern matching with variants
   - Lists and recursion
   - Tuples
   - External FFI calls to Erlang
*)

(* --- Basic arithmetic --- *)

let add x y = x + y

let double x = x * 2

let square x = x * x

(* --- Float operations --- *)

let circle_area radius = 3.14159 *. radius *. radius

(* --- Recursive functions --- *)

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let rec fibonacci n =
  if n <= 1 then n
  else fibonacci (n - 1) + fibonacci (n - 2)

(* --- List operations --- *)

let rec sum_list lst =
  match lst with
  | [] -> 0
  | x :: xs -> x + sum_list xs

let rec length lst =
  match lst with
  | [] -> 0
  | _ :: xs -> 1 + length xs

(* Specialized map that doubles each element *)
let rec map_double lst =
  match lst with
  | [] -> []
  | x :: xs -> double x :: map_double xs

(* --- Tuples --- *)

let make_point x y = (x, y)

let add_points p1 p2 =
  match p1, p2 with
  | (x1, y1), (x2, y2) -> (x1 + x2, y1 + y2)

(* --- Custom variants (ADTs) --- *)

type color = Red | Green | Blue

let color_to_string c =
  match c with
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"

type shape =
  | Circle of float
  | Rectangle of float * float

let area shape =
  match shape with
  | Circle r -> 3.14159 *. r *. r
  | Rectangle (w, h) -> w *. h

(* --- Option type --- *)

let safe_divide a b =
  if b = 0 then None
  else Some (a / b)

let option_default default opt =
  match opt with
  | None -> default
  | Some x -> x

(* --- Erlang FFI --- *)

external erlang_self : unit -> int = "erlang" "self"
external lists_reverse : 'a list -> 'a list = "lists" "reverse"

let get_pid () = erlang_self ()

let reverse_list lst = lists_reverse lst

(* --- Entry point demonstrating usage --- *)

let main () =
  let _ = add 2 3 in                   (* 5 *)
  let _ = factorial 5 in               (* 120 *)
  let _ = fibonacci 10 in              (* 55 *)
  let _ = sum_list [1; 2; 3; 4] in     (* 10 *)
  let _ = map_double [1; 2; 3] in      (* [2; 4; 6] *)
  let _ = area (Circle 2.0) in         (* ~12.56 *)
  let _ = color_to_string Red in       (* "red" *)
  let _ = safe_divide 10 2 in          (* Some 5 *)
  let _ = reverse_list [1; 2; 3] in    (* [3; 2; 1] *)
  "done"
