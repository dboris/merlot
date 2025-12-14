(* Test exception handling *)

exception Not_found
exception My_error of int

let safe_div a b =
  try
    if b = 0 then raise (My_error 42)
    else a / b
  with
  | My_error _ -> -1
  | Not_found -> -2

let test_failwith () =
  try
    failwith "error"
  with
  | Failure _ -> 0
