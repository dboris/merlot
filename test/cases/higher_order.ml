(* Higher-order functions test *)

let double x = x * 2
let add1 x = x + 1

(* Function taking a function as argument *)
let apply_twice f x = f (f x)

(* Recursive HOF *)
let rec map f lst =
  match lst with
  | [] -> []
  | x :: xs -> f x :: map f xs

(* Anonymous functions (lambdas) *)
let test_lambda () =
  let f = fun x -> x + 1 in
  f 5

(* Passing top-level function as argument *)
let test_apply_twice () = apply_twice double 3

let test_map () = map add1 [1; 2; 3]
