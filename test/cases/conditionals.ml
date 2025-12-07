let max x y = if x > y then x else y

let min x y = if x < y then x else y

let abs x = if x < 0 then -x else x

let sign x =
  if x > 0 then 1
  else if x < 0 then -1
  else 0
