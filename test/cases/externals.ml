external erlang_self : unit -> int = "erlang" "self"
external lists_reverse : 'a list -> 'a list = "lists" "reverse"
external lists_length : 'a list -> int = "erlang" "length"
external erlang_hd : 'a list -> 'a = "erlang" "hd"
external erlang_tl : 'a list -> 'a list = "erlang" "tl"

let my_pid () = erlang_self ()

let reverse_list xs = lists_reverse xs

let list_length xs = lists_length xs

let first xs = erlang_hd xs

let rest xs = erlang_tl xs

let example () =
  let xs = [1; 2; 3] in
  let rev = lists_reverse xs in
  let len = lists_length rev in
  len
