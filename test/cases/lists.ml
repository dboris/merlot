let empty = []

let singleton x = [x]

let pair x y = [x; y]

let cons h t = h :: t

let head = function
  | [] -> 0
  | x :: _ -> x

let tail = function
  | [] -> []
  | _ :: xs -> xs
