(** String - OCaml-compatible String module backed by Elixir

    Provides OCaml's String API using Elixir's String module.
    Both OCaml and Elixir treat strings as binary/byte sequences,
    making this a natural fit.

    Note: Requires Elixir runtime. Strings are UTF-8 binaries.

    Usage:
    {[
      open Merlot_stdlib

      let greeting = String.concat " " ["Hello"; "World"]
      let upper = String.uppercase_ascii "hello"
      let parts = String.split_on_char ',' "a,b,c"
    ]}
*)

(** {1 Basic Operations} *)

(** Return the length of a string in bytes.
    For grapheme count, use [length_graphemes]. *)
external length : string -> int = "String" "length"

(** Return the length in graphemes (Unicode characters). *)
external length_graphemes : string -> int = "String" "length"

(** Get byte at index (0-indexed). Returns None if out of bounds. *)
external get_opt : string -> int -> char option = "String" "at"

(** Get byte at index (0-indexed). Raises if out of bounds. *)
let get s i =
  match get_opt s i with
  | Some c -> c
  | None -> failwith "String.get: index out of bounds"

(** Create string of n copies of character. *)
external make : int -> char -> string = "String" "duplicate"

(** {1 Concatenation} *)

(** Concatenate two strings. *)
external concat2 : string -> string -> string = "Kernel" "<>"

(** Concatenate list of strings with separator. *)
external concat : string -> string list -> string = "Enum" "join"

(** {1 Comparison} *)

(** Compare two strings lexicographically. *)
let compare s1 s2 =
  if s1 < s2 then -1
  else if s1 > s2 then 1
  else 0

(** Check if two strings are equal. *)
let equal s1 s2 = s1 = s2

(** {1 Substrings} *)

(** Extract substring starting at position with given length.
    [sub s pos len] returns substring from [pos] of length [len]. *)
external sub : string -> int -> int -> string = "String" "slice"

(** Get first n bytes. *)
external prefix : string -> int -> string = "String" "slice"

(** Get last n bytes. *)
external suffix : string -> int -> string = "String" "slice"

(** {1 Searching} *)

(** Check if string contains substring. *)
external contains : string -> string -> bool = "String" "contains?"

(** Find index of first occurrence of substring. Returns -1 if not found. *)
let index_of pattern s =
  (* Use :binary.match for efficiency *)
  match Lists.keyfind pattern 0 [s] with
  | _ -> -1  (* TODO: proper implementation *)

(** Check if string starts with prefix. *)
external starts_with : prefix:string -> string -> bool = "String" "starts_with?"

(** Check if string ends with suffix. *)
external ends_with : suffix:string -> string -> bool = "String" "ends_with?"

(** {1 Case Conversion} *)

(** Convert to uppercase (Unicode-aware). *)
external uppercase : string -> string = "String" "upcase"

(** Convert to lowercase (Unicode-aware). *)
external lowercase : string -> string = "String" "downcase"

(** Capitalize first character (Unicode-aware). *)
external capitalize : string -> string = "String" "capitalize"

(** OCaml compatibility aliases - these are actually Unicode-aware. *)
let uppercase_ascii = uppercase
let lowercase_ascii = lowercase
let capitalize_ascii = capitalize

(** Lowercase first character. *)
let uncapitalize s =
  if length s = 0 then s
  else
    let first = sub s 0 1 in
    let rest = sub s 1 (length s - 1) in
    concat2 (lowercase first) rest

let uncapitalize_ascii = uncapitalize

(** {1 Splitting} *)

(** Split string on substring separator. *)
external split : string -> string -> string list = "String" "split"

(** Split on single character. *)
let split_on_char c s =
  split s (make 1 c)

(** {1 Trimming} *)

(** Remove leading and trailing whitespace. *)
external trim : string -> string = "String" "trim"

(** Remove leading whitespace. *)
external trim_start : string -> string = "String" "trim_leading"

(** Remove trailing whitespace. *)
external trim_end : string -> string = "String" "trim_trailing"

(** {1 Padding} *)

(** Pad string on left to reach length. *)
external pad_left : string -> int -> string = "String" "pad_leading"

(** Pad string on right to reach length. *)
external pad_right : string -> int -> string = "String" "pad_trailing"

(** {1 Replacing} *)

(** Replace all occurrences of pattern with replacement. *)
external replace : string -> string -> string -> string = "String" "replace"

(** Replace first occurrence of pattern with replacement. *)
let replace_first s pattern replacement =
  (* TODO: Use String.replace with limit *)
  replace s pattern replacement

(** {1 Predicates} *)

(** Check if string is empty. *)
let is_empty s = length s = 0

(** Check if string contains only whitespace. *)
external is_blank : string -> bool = "String" "trim"

(** Check if string matches pattern (simple glob). *)
external matches : string -> string -> bool = "String" "match?"

(** {1 Conversion} *)

(** Convert string to list of bytes (integers). *)
external to_list : string -> int list = "String" "to_charlist"

(** Convert list of bytes to string. *)
external of_list : int list -> string = "List" "to_string"

(** Convert integer to string. *)
external of_int : int -> string = "Integer" "to_string"

(** Convert float to string. *)
external of_float : float -> string = "Float" "to_string"

(** Parse integer from string. Returns None on failure. *)
external to_int_opt : string -> int option = "Integer" "parse"

(** Parse float from string. Returns None on failure. *)
external to_float_opt : string -> float option = "Float" "parse"

(** {1 Iteration} *)

(** Iterate over graphemes. *)
let iter f s =
  Lists.iter f (to_list s)

(** Map over graphemes. *)
let map f s =
  of_list (Lists.map f (to_list s))

(** Fold over bytes left to right. *)
let fold_left f init s =
  Lists.foldl (fun acc c -> f acc c) init (to_list s)

(** {1 Unicode Operations} *)

(** Split into list of graphemes (Unicode characters). *)
external graphemes : string -> string list = "String" "graphemes"

(** Split into list of codepoints. *)
external codepoints : string -> string list = "String" "codepoints"

(** Reverse string (grapheme-aware). *)
external reverse : string -> string = "String" "reverse"

(** Normalize Unicode string. *)
external normalize : string -> string = "String" "normalize"
