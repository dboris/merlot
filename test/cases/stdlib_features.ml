(* Test file for OCaml stdlib compatibility features:
   - Assert expressions
   - Nested patterns
*)

(* Test 1: Simple pattern matching with nested tuples *)
let test_tuple pair =
  match pair with
  | (a, (b, c)) -> a + b + c

(* Test 2: Assert expression *)
let positive_only x =
  assert (x > 0);
  x * 2

(* Test 3: Recursive sum (pure functional) *)
let rec sum_to n =
  if n <= 0 then 0
  else n + sum_to (n - 1)

(* Test 4: Recursive countdown (pure functional) *)
let rec countdown n =
  if n <= 0 then []
  else n :: countdown (n - 1)

(* Test entry point *)
let main () =
  (* Test tuple: (1, (2, 10)) -> 1 + 2 + 10 = 13 *)
  let tuple_result = test_tuple (1, (2, 10)) in

  (* Test sum: 5+4+3+2+1 = 15 *)
  let sum = sum_to 5 in

  (* Test countdown: [3, 2, 1] *)
  let cd = countdown 3 in

  (* Test assert: 5 * 2 = 10 *)
  let doubled = positive_only 5 in

  (tuple_result, sum, cd, doubled)
