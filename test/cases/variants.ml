type color = Red | Green | Blue

type shape =
  | Circle of float
  | Rectangle of float * float

let red = Red
let green = Green
let blue = Blue

let circle r = Circle r
let rect w h = Rectangle (w, h)

let color_to_int = function
  | Red -> 0
  | Green -> 1
  | Blue -> 2

let area = function
  | Circle r -> r *. r *. 3.14159
  | Rectangle (w, h) -> w *. h
