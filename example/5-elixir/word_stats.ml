(* Word Stats - An Elixir Enum example

   Demonstrates Elixir's Enum module for functional data processing.

   Features:
   - Enum.map, filter for transformations
   - Enum.sort, reverse, take for ordering
   - Enum.sum, count, min, max for aggregations
   - Enum.any, all for predicates

   The API follows OCaml convention (function first) for pipe compatibility,
   while the compiler handles argument reordering for Elixir calls.

   Requires Elixir to be installed alongside Erlang/OTP.
*)

open Merlot_elixir

(* Square a number *)
let square x = x * x

(* Check if even *)
let is_even x = x mod 2 = 0

(* Check if positive *)
let is_positive x = x > 0

(* Double a number *)
let double x = x * 2

(* Process numbers using pipe operators *)
let sum_of_squared_evens nums =
  nums |> Enum.filter is_even |> Enum.map square |> Enum.sum

(* Get top N largest numbers *)
let top_n n nums =
  nums |> Enum.sort |> Enum.reverse |> Enum.take n

(* Double all numbers *)
let double_all nums =
  nums |> Enum.map double

(* Check predicates *)
let has_evens nums = nums |> Enum.any is_even
let all_positive nums = nums |> Enum.all is_positive

(* Main demonstration *)
let main () =
  let numbers = [5; 2; 8; 1; 9; 3; 7; 4; 6] in

  (* Sum of squared evens: 2^2 + 4^2 + 6^2 + 8^2 = 4 + 16 + 36 + 64 = 120 *)
  let squared_sum = sum_of_squared_evens numbers in

  (* Top 3: [9, 8, 7] *)
  let top3 = numbers |> top_n 3 in

  (* Aggregations *)
  let total = Enum.sum numbers in
  let cnt = Enum.count numbers in
  let smallest = Enum.min numbers in
  let largest = Enum.max numbers in

  (* Predicates *)
  let has_even = has_evens numbers in
  let all_pos = all_positive numbers in

  (* Doubled: [10, 4, 16, 2, 18, 6, 14, 8, 12] *)
  let doubled = double_all numbers in

  (* Return results *)
  (squared_sum, top3, total, cnt, smallest, largest, has_even, all_pos, doubled)
