(* Test Printf support *)

let test_sprintf_string () =
  Printf.sprintf "Hello %s" "world"

let test_sprintf_int () =
  Printf.sprintf "Value: %d" 42

let test_sprintf_mixed () =
  Printf.sprintf "Name: %s, Age: %d" "Alice" 30

let test_sprintf_float () =
  Printf.sprintf "Pi: %.2f" 3.14159

let main () =
  let s1 = test_sprintf_string () in
  let s2 = test_sprintf_int () in
  let s3 = test_sprintf_mixed () in
  let s4 = test_sprintf_float () in
  (s1, s2, s3, s4)
