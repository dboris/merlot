(** Result - Combinators for Result type error handling

    Provides functional combinators for working with Result types,
    enabling clean error handling pipelines without nested matching.

    Example:
    {[
      let validate_user input =
        input
        |> parse_json
        |> Result.flat_map validate_email
        |> Result.flat_map check_password
        |> Result.map create_user

      (* Or using Result.try_ for early returns *)
      let process_order order_id =
        Result.try_ (fun () ->
          let* order = get_order order_id in
          let* user = get_user order.user_id in
          let* payment = charge_user user order.total in
          Ok (order, payment))
    ]}
*)

(** Result type - either Ok with value or Error with error *)
type ('a, 'e) t = Ok of 'a | Error of 'e

(** {1 Constructors} *)

(** Create a successful result *)
let ok x = Ok x

(** Create an error result *)
let error e = Error e

(** {1 Inspecting} *)

(** Check if result is Ok *)
let is_ok = function
  | Ok _ -> true
  | Error _ -> false

(** Check if result is Error *)
let is_error = function
  | Ok _ -> false
  | Error _ -> true

(** {1 Extracting Values} *)

(** Get the Ok value or a default *)
let unwrap_or default = function
  | Ok x -> x
  | Error _ -> default

(** Get the Ok value or compute a default *)
let unwrap_or_else f = function
  | Ok x -> x
  | Error e -> f e

(** Get the Ok value, raising an exception if Error.
    Use sparingly - prefer pattern matching or combinators. *)
let unwrap = function
  | Ok x -> x
  | Error _ -> failwith "Result.unwrap called on Error"

(** Get the Error value, raising an exception if Ok *)
let unwrap_error = function
  | Ok _ -> failwith "Result.unwrap_error called on Ok"
  | Error e -> e

(** {1 Transforming} *)

(** Map a function over the Ok value *)
let map f = function
  | Ok x -> Ok (f x)
  | Error e -> Error e

(** Map a function over the Error value *)
let map_error f = function
  | Ok x -> Ok x
  | Error e -> Error (f e)

(** Apply a Result-returning function to the Ok value (flatMap/bind) *)
let flat_map f = function
  | Ok x -> f x
  | Error e -> Error e

(** Alias for flat_map *)
let bind = flat_map

(** Alias for flat_map (Gleam-style) *)
let try_ f = function
  | Ok x -> f x
  | Error e -> Error e

(** {1 Combining} *)

(** Apply a function wrapped in Result to a value wrapped in Result *)
let apply rf rx =
  match rf with
  | Ok f -> map f rx
  | Error e -> Error e

(** Combine two results, returning first error if any *)
let both r1 r2 =
  match r1, r2 with
  | Ok x, Ok y -> Ok (x, y)
  | Error e, _ -> Error e
  | _, Error e -> Error e

(** Combine a list of results into a result of list *)
let all results =
  let rec rev_append l1 l2 =
    match l1 with
    | [] -> l2
    | x :: rest -> rev_append rest (x :: l2)
  in
  let rec loop acc = function
    | [] -> Ok (rev_append acc [])
    | Ok x :: rest -> loop (x :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] results

(** Return first Ok, or last Error if all fail *)
let any results =
  let rec loop last_err = function
    | [] -> (match last_err with Some e -> Error e | None -> Error "empty")
    | Ok x :: _ -> Ok x
    | Error e :: rest -> loop (Some e) rest
  in
  loop None results

(** {1 Converting} *)

(** Convert Option to Result with given error *)
let from_option error = function
  | Some x -> Ok x
  | None -> Error error

(** Convert Result to Option, discarding error *)
let to_option = function
  | Ok x -> Some x
  | Error _ -> None

(** {1 Side Effects} *)

(** Execute function if Ok *)
let iter f = function
  | Ok x -> f x
  | Error _ -> ()

(** Execute function if Error *)
let iter_error f = function
  | Ok _ -> ()
  | Error e -> f e

(** {1 Folding} *)

(** Fold over Result *)
let fold ~ok ~error = function
  | Ok x -> ok x
  | Error e -> error e

(** {1 Predicates} *)

(** Check if Ok value satisfies predicate *)
let exists p = function
  | Ok x -> p x
  | Error _ -> false

(** Check if Error or Ok value satisfies predicate *)
let for_all p = function
  | Ok x -> p x
  | Error _ -> true

(** {1 Infix Operators} *)

(** Infix map: result >>| f *)
let ( >>| ) r f = map f r

(** Infix flat_map: result >>= f *)
let ( >>= ) r f = flat_map f r

(** Infix apply: rf <*> rx *)
let ( <*> ) = apply

(** {1 Let Syntax (requires ppx or manual desugaring)}

    For monadic let syntax, use:
    {[
      let ( let* ) = Result.bind
      let ( let+ ) = Result.map

      let example =
        let* x = get_x () in
        let* y = get_y () in
        let+ z = get_z () in
        x + y + z
    ]}
*)
let ( let* ) = bind
let ( let+ ) r f = map f r
