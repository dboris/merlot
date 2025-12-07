(** Lists - Erlang lists module bindings *)

external reverse : 'a list -> 'a list = "lists" "reverse"
external sort : 'a list -> 'a list = "lists" "sort"
external length : 'a list -> int = "erlang" "length"

(** Basic list operations (re-exported from Erlang BIFs) *)
external hd : 'a list -> 'a = "erlang" "hd"
external tl : 'a list -> 'a list = "erlang" "tl"
external append2 : 'a list -> 'a list -> 'a list = "erlang" "++"
external map : ('a -> 'b) -> 'a list -> 'b list = "lists" "map"
external filter : ('a -> bool) -> 'a list -> 'a list = "lists" "filter"
external foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = "lists" "foldl"
external foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = "lists" "foldr"
external sublist : 'a list -> int -> 'a list = "lists" "sublist"
external all : ('a -> bool) -> 'a list -> bool = "lists" "all"
external any : ('a -> bool) -> 'a list -> bool = "lists" "any"
external usort : 'a list -> 'a list = "lists" "usort"
external flatten : 'a list -> 'b list = "lists" "flatten"
external nth : int -> 'a list -> 'a = "lists" "nth"
external nthtail : int -> 'a list -> 'a list = "lists" "nthtail"
external last : 'a list -> 'a = "lists" "last"
external member : 'a -> 'a list -> bool = "lists" "member"
external keyfind : 'a -> int -> 'b list -> 'b = "lists" "keyfind"
external keystore : 'a -> int -> 'b list -> 'b -> 'b list = "lists" "keystore"
external keydelete : 'a -> int -> 'b list -> 'b list = "lists" "keydelete"
external keymember : 'a -> int -> 'b list -> bool = "lists" "keymember"
external zip : 'a list -> 'b list -> ('a * 'b) list = "lists" "zip"
external unzip : ('a * 'b) list -> 'a list * 'b list = "lists" "unzip"
external seq : int -> int -> int list = "lists" "seq"
external seq_step : int -> int -> int -> int list = "lists" "seq"
external duplicate : int -> 'a -> 'a list = "lists" "duplicate"
external sum : int list -> int = "lists" "sum"
external max : 'a list -> 'a = "lists" "max"
external min : 'a list -> 'a = "lists" "min"
external concat : 'a list list -> 'a list = "lists" "concat"
external append : 'a list list -> 'a list = "lists" "append"
external subtract : 'a list -> 'a list -> 'a list = "lists" "subtract"

(** Find first element matching predicate, raises if not found *)
let rec find pred = function
  | [] -> failwith "not_found"
  | x :: xs -> if pred x then x else find pred xs

(** Find first element matching predicate, returns option *)
let rec find_opt pred = function
  | [] -> None
  | x :: xs -> if pred x then Some x else find_opt pred xs

(** Apply function to each element for side effects *)
external iter : ('a -> unit) -> 'a list -> unit = "lists" "foreach"

(** Apply function to each element with index *)
let iteri f xs =
  let rec loop i = function
    | [] -> ()
    | x :: rest -> f i x; loop (i + 1) rest
  in
  loop 0 xs

(** Map with index *)
let mapi f xs =
  let rec loop i = function
    | [] -> []
    | x :: rest -> f i x :: loop (i + 1) rest
  in
  loop 0 xs

(** Filter map - map and filter in one pass *)
let rec filter_map f = function
  | [] -> []
  | x :: xs ->
      match f x with
      | Some y -> y :: filter_map f xs
      | None -> filter_map f xs
