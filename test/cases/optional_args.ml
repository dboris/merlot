(* Test optional arguments *)

(* Function with optional argument and default value *)
let greet ?(multiplier = 2) n =
  n * multiplier

(* Function with multiple optional arguments *)
let calculate ?(add = 0) ?(mult = 1) x =
  (x + add) * mult

(* Calling with and without optional args *)
let main () =
  let r1 = greet 5 in                        (* Uses default: 5*2 = 10 *)
  let r2 = greet ~multiplier:3 5 in          (* Explicit: 5*3 = 15 *)
  let r3 = calculate 10 in                   (* All defaults: (10+0)*1 = 10 *)
  let r4 = calculate ~add:5 10 in            (* One override: (10+5)*1 = 15 *)
  let r5 = calculate ~add:5 ~mult:2 10 in    (* Both: (10+5)*2 = 30 *)
  (r1, r2, r3, r4, r5)
