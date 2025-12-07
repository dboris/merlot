(** Option - OCaml-compatible Option module for Merlot

    Provides the standard OCaml Option API for working with optional values.
    Options represent values that may or may not be present.

    The Option type is represented as:
    - Some x -> {some, X} in Erlang
    - None   -> none in Erlang

    Usage:
    {[
      open Merlot_stdlib

      let safe_head lst =
        match lst with
        | [] -> None
        | x :: _ -> Some x

      let doubled = Option.map (fun x -> x * 2) (Some 5)  (* Some 10 *)
      let value = Option.value ~default:0 None            (* 0 *)
    ]}
*)

(** Option type - either Some value or None *)
type 'a t = 'a option

(** {1 Constructors} *)

(** The None value *)
let none = None

(** Create a Some value *)
let some x = Some x

(** {1 Inspecting} *)

(** Check if option is None *)
let is_none = function
  | None -> true
  | Some _ -> false

(** Check if option is Some *)
let is_some = function
  | None -> false
  | Some _ -> true

(** {1 Extracting Values} *)

(** Get the value or a default *)
let value opt ~default =
  match opt with
  | Some x -> x
  | None -> default

(** Get the value or compute a default lazily *)
let value_lazy opt ~default =
  match opt with
  | Some x -> x
  | None -> default ()

(** Get the value, raising an exception if None.
    Use sparingly - prefer pattern matching or combinators. *)
let get = function
  | Some x -> x
  | None -> failwith "Option.get called on None"

(** Get the value or raise Invalid_argument with message *)
let get_exn_msg msg = function
  | Some x -> x
  | None -> failwith msg

(** {1 Transforming} *)

(** Map a function over the option value *)
let map f = function
  | Some x -> Some (f x)
  | None -> None

(** Map and flatten - apply a function returning option *)
let bind opt f =
  match opt with
  | Some x -> f x
  | None -> None

(** Alias for bind *)
let flat_map f opt = bind opt f

(** Join nested options *)
let join = function
  | Some (Some x) -> Some x
  | _ -> None

(** {1 Filtering} *)

(** Filter based on predicate *)
let filter pred = function
  | Some x when pred x -> Some x
  | _ -> None

(** {1 Combining} *)

(** Return first Some, or None if both are None *)
let or_ opt1 opt2 =
  match opt1 with
  | Some _ -> opt1
  | None -> opt2

(** Return first Some, lazily evaluating second *)
let or_lazy opt1 opt2_fn =
  match opt1 with
  | Some _ -> opt1
  | None -> opt2_fn ()

(** Combine two options into option of pair *)
let both opt1 opt2 =
  match opt1, opt2 with
  | Some x, Some y -> Some (x, y)
  | _ -> None

(** Apply function in option to value in option *)
let apply opt_f opt_x =
  match opt_f, opt_x with
  | Some f, Some x -> Some (f x)
  | _ -> None

(** {1 Converting} *)

(** Convert option to list (empty or singleton) *)
let to_list = function
  | Some x -> [x]
  | None -> []

(** Convert option to result with given error *)
let to_result ~none = function
  | Some x -> Ok x
  | None -> Error none

(** Convert result to option, discarding error *)
let of_result = function
  | Ok x -> Some x
  | Error _ -> None

(** {1 Side Effects} *)

(** Execute function if Some *)
let iter f = function
  | Some x -> f x
  | None -> ()

(** {1 Folding} *)

(** Fold over option *)
let fold ~none ~some = function
  | Some x -> some x
  | None -> none

(** {1 Predicates} *)

(** Check if Some value satisfies predicate *)
let exists pred = function
  | Some x -> pred x
  | None -> false

(** Check if None or Some value satisfies predicate *)
let for_all pred = function
  | Some x -> pred x
  | None -> true

(** {1 Comparison} *)

(** Compare two options *)
let compare cmp opt1 opt2 =
  match opt1, opt2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x, Some y -> cmp x y

(** Check if two options are equal *)
let equal eq opt1 opt2 =
  match opt1, opt2 with
  | None, None -> true
  | Some x, Some y -> eq x y
  | _ -> false

(** {1 Infix Operators} *)

(** Infix map: opt >>| f *)
let ( >>| ) opt f = map f opt

(** Infix bind: opt >>= f *)
let ( >>= ) opt f = bind opt f

(** Infix or: opt1 |? opt2 *)
let ( |? ) = or_

(** {1 Let Syntax}

    For monadic let syntax:
    {[
      let ( let* ) = Option.bind
      let ( let+ ) opt f = Option.map f opt

      let example =
        let* x = get_x () in
        let* y = get_y () in
        let+ z = get_z () in
        x + y + z
    ]}
*)
let ( let* ) = bind
let ( let+ ) opt f = map f opt
