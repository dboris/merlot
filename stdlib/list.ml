(** List - OCaml-compatible List module for Merlot

    Provides the standard OCaml List API, implemented using Erlang's
    lists module where possible for performance.

    This module uses OCaml naming conventions (snake_case, fold_left)
    while delegating to efficient Erlang implementations.

    Usage:
    {[
      open Merlot_stdlib

      let doubled = List.map (fun x -> x * 2) [1; 2; 3]
      let sum = List.fold_left (+) 0 [1; 2; 3; 4; 5]
      let evens = List.filter (fun x -> x mod 2 = 0) [1; 2; 3; 4]
    ]}
*)

(** {1 Basic Operations} *)

(** Return the length of a list. O(n). *)
external length : 'a list -> int = "erlang" "length"

(** Return the first element. Raises if empty. *)
external hd : 'a list -> 'a = "erlang" "hd"

(** Return the tail (all but first). Raises if empty. *)
external tl : 'a list -> 'a list = "erlang" "tl"

(** Return the n-th element (0-indexed). Raises if out of bounds. *)
let nth lst n =
  (* Erlang lists:nth is 1-indexed *)
  Lists.nth (n + 1) lst

(** Safe nth - returns None if out of bounds. *)
let nth_opt lst n =
  if n < 0 || n >= Lists.length lst then None
  else Some (nth lst n)

(** Reverse a list. *)
external rev : 'a list -> 'a list = "lists" "reverse"

(** Append two lists. *)
external append : 'a list -> 'a list -> 'a list = "erlang" "++"

(** Concatenate a list of lists. *)
external concat : 'a list list -> 'a list = "lists" "append"

(** Flatten a list of lists (alias for concat). *)
external flatten : 'a list list -> 'a list = "lists" "append"

(** {1 Iterators} *)

(** Apply function to each element for side effects. *)
let rec iter f = function
  | [] -> ()
  | x :: xs -> f x; iter f xs

(** Apply function to each element with index. *)
let iteri f xs =
  let rec loop i = function
    | [] -> ()
    | x :: rest -> f i x; loop (i + 1) rest
  in
  loop 0 xs

(** {1 Transformers} *)

(** Map a function over a list. *)
external map : ('a -> 'b) -> 'a list -> 'b list = "lists" "map"

(** Map with index. *)
let mapi f xs =
  let rec loop i = function
    | [] -> []
    | x :: rest -> f i x :: loop (i + 1) rest
  in
  loop 0 xs

(** Map and concatenate results. *)
let concat_map f xs =
  concat (map f xs)

(** Alias for concat_map. *)
let flat_map = concat_map

(** Map with reversed result (tail-recursive). *)
let rev_map f xs =
  let rec loop acc = function
    | [] -> acc
    | x :: rest -> loop (f x :: acc) rest
  in
  loop [] xs

(** {1 Filtering} *)

(** Filter elements matching predicate. *)
external filter : ('a -> bool) -> 'a list -> 'a list = "lists" "filter"

(** Filter and map in one pass. *)
let rec filter_map f = function
  | [] -> []
  | x :: xs ->
      match f x with
      | Some y -> y :: filter_map f xs
      | None -> filter_map f xs

(** Partition into (matching, not matching). *)
let partition pred xs =
  let rec loop yes no = function
    | [] -> (rev yes, rev no)
    | x :: rest ->
        if pred x then loop (x :: yes) no rest
        else loop yes (x :: no) rest
  in
  loop [] [] xs

(** {1 Folding} *)

(** Fold left: fold_left f init [a; b; c] = f (f (f init a) b) c *)
let fold_left f init xs =
  (* Erlang foldl has different argument order: fun(Elem, Acc) *)
  Lists.foldl (fun acc x -> f acc x) init xs

(** Fold right: fold_right f [a; b; c] init = f a (f b (f c init)) *)
let fold_right f xs init =
  (* Erlang foldr has: fun(Elem, Acc) *)
  Lists.foldr (fun x acc -> f x acc) init xs

(** {1 Searching} *)

(** Check if element exists in list. *)
external mem : 'a -> 'a list -> bool = "lists" "member"

(** Check if any element satisfies predicate. *)
external exists : ('a -> bool) -> 'a list -> bool = "lists" "any"

(** Check if all elements satisfy predicate. *)
external for_all : ('a -> bool) -> 'a list -> bool = "lists" "all"

(** Find first element matching predicate. Raises if not found. *)
let rec find pred = function
  | [] -> failwith "Not_found"
  | x :: xs -> if pred x then x else find pred xs

(** Find first element matching predicate, returns option. *)
let rec find_opt pred = function
  | [] -> None
  | x :: xs -> if pred x then Some x else find_opt pred xs

(** Find index of first matching element. *)
let find_index pred xs =
  let rec loop i = function
    | [] -> None
    | x :: rest -> if pred x then Some i else loop (i + 1) rest
  in
  loop 0 xs

(** {1 Association Lists} *)

(** Find value associated with key. Raises if not found. *)
let rec assoc key = function
  | [] -> failwith "Not_found"
  | (k, v) :: _ when k = key -> v
  | _ :: rest -> assoc key rest

(** Find value associated with key, returns option. *)
let rec assoc_opt key = function
  | [] -> None
  | (k, v) :: _ when k = key -> Some v
  | _ :: rest -> assoc_opt key rest

(** Check if key exists in association list. *)
let rec mem_assoc key = function
  | [] -> false
  | (k, _) :: _ when k = key -> true
  | _ :: rest -> mem_assoc key rest

(** Remove association for key. *)
let rec remove_assoc key = function
  | [] -> []
  | (k, _) :: rest when k = key -> rest
  | pair :: rest -> pair :: remove_assoc key rest

(** {1 Sorting} *)

(** Sort a list in ascending order. *)
external sort : ('a -> 'a -> int) -> 'a list -> 'a list = "lists" "sort"

(** Sort and remove duplicates. *)
let sort_uniq cmp xs =
  Lists.usort (sort cmp xs)

(** Stable sort (preserves order of equal elements). *)
let stable_sort = sort

(** {1 Combining} *)

(** Zip two lists into pairs. Lists must have same length. *)
external combine : 'a list -> 'b list -> ('a * 'b) list = "lists" "zip"

(** Unzip pairs into two lists. *)
external split : ('a * 'b) list -> 'a list * 'b list = "lists" "unzip"

(** Map over two lists simultaneously. *)
let map2 f xs ys =
  let rec loop = function
    | [], [] -> []
    | x :: xs, y :: ys -> f x y :: loop (xs, ys)
    | _ -> failwith "List.map2: lists have different lengths"
  in
  loop (xs, ys)

(** Iterate over two lists. *)
let iter2 f xs ys =
  let rec loop = function
    | [], [] -> ()
    | x :: xs, y :: ys -> f x y; loop (xs, ys)
    | _ -> failwith "List.iter2: lists have different lengths"
  in
  loop (xs, ys)

(** Fold over two lists. *)
let fold_left2 f init xs ys =
  let rec loop acc = function
    | [], [] -> acc
    | x :: xs, y :: ys -> loop (f acc x y) (xs, ys)
    | _ -> failwith "List.fold_left2: lists have different lengths"
  in
  loop init (xs, ys)

(** {1 List Creation} *)

(** Create list by calling function n times. *)
let init n f =
  let rec loop i acc =
    if i < 0 then acc
    else loop (i - 1) (f i :: acc)
  in
  loop (n - 1) []

(** {1 Comparison} *)

(** Compare two lists lexicographically. *)
let rec compare cmp xs ys =
  match xs, ys with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let c = cmp x y in
      if c <> 0 then c else compare cmp xs ys

(** Check if two lists are equal. *)
let rec equal eq xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal eq xs ys
  | _ -> false
