let hello () = "Hello, World!"

let add x y = x + y

let factorial n =
  let rec loop acc n =
    if n <= 1 then acc
    else loop (acc * n) (n - 1)
  in
  loop 1 n
