let none = None

let some x = Some x

let is_some = function
  | None -> false
  | Some _ -> true

let get_or_default default = function
  | None -> default
  | Some x -> x

let map f = function
  | None -> None
  | Some x -> Some (f x)
