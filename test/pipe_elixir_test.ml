(* Test pipe with Elixir Enum - now with proper currying! *)

open Merlot_elixir

let square x = x * x
let is_even x = x mod 2 = 0

(* Pipes now work seamlessly with Elixir functions *)
let test_pipeline () =
  [1; 2; 3; 4] |> Enum.filter is_even |> Enum.map square |> Enum.sum

(* Single-arg Enum functions *)
let test_sort_pipeline () =
  [5; 2; 8; 1] |> Enum.sort |> Enum.reverse

(* Chaining with take *)
let test_take_pipeline () =
  [5; 2; 8; 1; 9; 3] |> Enum.sort |> Enum.reverse |> Enum.take 3

let main () =
  (test_pipeline (), test_sort_pipeline (), test_take_pipeline ())
