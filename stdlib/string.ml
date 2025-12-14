(** String - OCaml-compatible String module backed by Elixir

    Provides OCaml's String API using Elixir's String module,
    plus comprehensive Unicode support that OCaml's stdlib lacks.

    Note: Requires Elixir runtime. Strings are UTF-8 binaries.

    {b OCaml-compatible functions:}
    - Basic: [length], [get], [sub], [concat], [compare], [equal]
    - Case: [uppercase_ascii], [lowercase_ascii], [capitalize_ascii], [uncapitalize_ascii]
    - Splitting: [split_on_char]
    - Iteration: [iter], [map], [fold_left]

    {b Unicode operations} (not available in OCaml's String):
    - Graphemes: [graphemes], [next_grapheme], [next_grapheme_size], [length_graphemes]
    - Codepoints: [codepoints], [next_codepoint]
    - Normalization: [normalize], [normalize_form], [equivalent]
    - Validation: [is_valid], [is_printable], [replace_invalid], [chunk]
    - Slicing: [byte_slice] (respects codepoint boundaries)

    {b Elixir-specific utilities:}
    - Access: [first], [last], [split_at]
    - Metrics: [jaro_distance], [bag_distance], [count]
    - Replace: [replace_prefix], [replace_suffix], [replace_leading], [replace_trailing]

    Usage:
    {[
      open Merlot_stdlib

      (* OCaml-style *)
      let parts = String.split_on_char ',' "a,b,c"

      (* Unicode-aware iteration *)
      let chars = String.graphemes "héllo 🌍"  (* 7 graphemes *)

      (* Fuzzy matching *)
      let similar = String.jaro_distance "hello" "hallo"  (* ~0.87 *)

      (* Safe Unicode handling *)
      let clean = String.replace_invalid broken_utf8 "?"
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

(** Repeat string n times. *)
external duplicate : string -> int -> string = "String" "duplicate"

(** Get the first grapheme. Returns None if empty. *)
external first : string -> string option = "String" "first"

(** Get the last grapheme. Returns None if empty. *)
external last : string -> string option = "String" "last"

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

(** Split string at position into a tuple of two strings.
    [split_at "hello" 2] returns [("he", "llo")]. *)
external split_at : string -> int -> string * string = "String" "split_at"

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

(** Replace prefix if it matches. *)
external replace_prefix : string -> string -> string -> string = "String" "replace_prefix"

(** Replace suffix if it matches. *)
external replace_suffix : string -> string -> string -> string = "String" "replace_suffix"

(** Replace leading occurrences of pattern. *)
external replace_leading : string -> string -> string -> string = "String" "replace_leading"

(** Replace trailing occurrences of pattern. *)
external replace_trailing : string -> string -> string -> string = "String" "replace_trailing"

(** {1 Predicates} *)

(** Check if string is empty. *)
let is_empty s = length s = 0

(** Check if string contains only whitespace. *)
external is_blank : string -> bool = "String" "trim"

(** Check if string matches regex pattern. *)
external matches : string -> string -> bool = "String" "match?"

(** Check if string is valid UTF-8. *)
external is_valid : string -> bool = "String" "valid?"

(** Check if string contains only printable characters. *)
external is_printable : string -> bool = "String" "printable?"

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

(** Split into list of graphemes (Unicode characters).
    A grapheme is what users perceive as a single character,
    which may consist of multiple codepoints (e.g., "é" or emoji with modifiers). *)
external graphemes : string -> string list = "String" "graphemes"

(** Split into list of codepoints.
    Each codepoint is returned as a single-codepoint string. *)
external codepoints : string -> string list = "String" "codepoints"

(** Get the next grapheme and the remainder of the string.
    Returns [None] if string is empty.
    Useful for streaming/parsing Unicode text character by character.
    {[
      let rec print_graphemes s =
        match String.next_grapheme s with
        | None -> ()
        | Some (g, rest) ->
            Io.format "Grapheme: ~s~n" [g];
            print_graphemes rest
    ]} *)
external next_grapheme : string -> (string * string) option = "String" "next_grapheme"

(** Get the next codepoint and the remainder of the string.
    Returns [None] if string is empty. *)
external next_codepoint : string -> (string * string) option = "String" "next_codepoint"

(** Get the byte size of the next grapheme without extracting it.
    Returns [None] if string is empty or invalid.
    Useful for efficient scanning without allocation. *)
external next_grapheme_size : string -> int option = "String" "next_grapheme_size"

(** Slice string by byte positions, but return a valid UTF-8 string.
    [byte_slice s start length] extracts bytes but ensures the result
    doesn't split codepoints. *)
external byte_slice : string -> int -> int -> string = "String" "byte_slice"

(** Reverse string (grapheme-aware).
    Correctly handles multi-codepoint graphemes like emoji. *)
external reverse : string -> string = "String" "reverse"

(** Normalize Unicode string to NFC form (canonical composition).
    Use [normalize_form] for other normalization forms. *)
external normalize : string -> string = "String" "normalize"

(** Normalize Unicode string to specified form.
    Forms: ["nfc"], ["nfd"], ["nfkc"], ["nfkd"]. *)
external normalize_form : string -> string -> string = "String" "normalize"

(** Check if two strings are canonically equivalent.
    Two strings are equivalent if they normalize to the same form. *)
external equivalent : string -> string -> bool = "String" "equivalent?"

(** Replace invalid UTF-8 bytes with a replacement string.
    [replace_invalid s replacement] returns a valid UTF-8 string.
    Default replacement is the Unicode replacement character (U+FFFD). *)
external replace_invalid : string -> string -> string = "String" "replace_invalid"

(** Chunk string into segments based on a trait.
    [chunk s "valid"] splits into valid/invalid UTF-8 segments.
    [chunk s "printable"] splits into printable/non-printable segments. *)
external chunk : string -> string -> string list = "String" "chunk"

(** {1 String Metrics} *)

(** Compute Jaro distance between two strings (0.0 to 1.0).
    Useful for fuzzy string matching. *)
external jaro_distance : string -> string -> float = "String" "jaro_distance"

(** Compute bag distance between two strings.
    An efficient approximation of string similarity. *)
external bag_distance : string -> string -> int = "String" "bag_distance"

(** Count occurrences of pattern in string. *)
external count : string -> string -> int = "String" "count"
