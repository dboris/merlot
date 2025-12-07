(* Data Pipeline - A list processing example

   Demonstrates:
   - Lists.map, filter, foldl for list processing
   - Lists.sort, reverse
   - Helper functions as first-class values

   Note: This example uses the Erlang lists module which is
   always available. For Elixir's Enum module, argument order
   differences require special handling.
*)

open Merlot_stdlib

(* Square a number *)
let square x = x * x

(* Check if even *)
let is_even x = x mod 2 = 0

(* Check if positive *)
let is_positive x = x > 0

(* Add two numbers (for fold) *)
let add a b = a + b

(* Process numbers: filter evens, square them, sum *)
let process_evens nums =
  let evens = Lists.filter is_even nums in
  let squared = Lists.map square evens in
  Lists.foldl add 0 squared

(* Get top N largest numbers *)
let top_n n nums =
  let sorted = Lists.sort nums in
  let reversed = Lists.reverse sorted in
  Lists.sublist reversed n

(* Sum a list *)
let sum nums = Lists.foldl add 0 nums

(* Double each element *)
let double x = x * 2

let double_all nums = Lists.map double nums

(* Get length *)
let len nums = Lists.length nums

(* Check membership *)
let contains x nums = Lists.member x nums

(* Main demonstration *)
let main () =
  let numbers = [5; 2; 8; 1; 9; 3; 7; 4; 6] in

  (* Sum of squared evens: 2^2 + 8^2 + 4^2 + 6^2 = 4 + 64 + 16 + 36 = 120 *)
  let sum_squared_evens = process_evens numbers in

  (* Top 3: [9; 8; 7] *)
  let top3 = top_n 3 numbers in

  (* Sum: 45 *)
  let total = sum numbers in

  (* Length: 9 *)
  let cnt = len numbers in

  (* Doubled: [10; 4; 16; 2; 18; 6; 14; 8; 12] *)
  let doubled = double_all numbers in

  (* Contains 5? true *)
  let has_five = contains 5 numbers in

  (* Contains 10? false *)
  let has_ten = contains 10 numbers in

  (* Return key results *)
  (sum_squared_evens, top3, total, cnt, doubled, has_five, has_ten)
