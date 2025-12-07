(* Supervised Workers Example

   Demonstrates the Supervisor module for fault-tolerant applications.
   A supervisor monitors child processes and restarts them when they crash.
*)

open Merlot_stdlib

(* A simple worker that can be asked to crash *)
let worker_handler id state msg =
  match msg with
  | `Ping caller ->
      let _ = Process.send caller (`Pong id) in
      `Continue state
  | `GetCount caller ->
      let _ = Process.send caller (id, state) in
      `Continue state
  | `Increment ->
      `Continue (state + 1)
  | `Crash ->
      (* Simulate a crash by calling exit *)
      Process.exit `crashed
  | `Stop ->
      `Stop state

(* Create a worker start spec *)
let make_worker id =
  Supervisor.worker id (fun () ->
    Actor.start 0 (worker_handler id))

(* Ping a worker and wait for response *)
let ping_worker pid =
  let me = Process.self () in
  let _ = Process.send pid (`Ping me) in
  Process.receive (function
    | `Pong id -> id)

(* Get worker's count *)
let get_count pid =
  let me = Process.self () in
  let _ = Process.send pid (`GetCount me) in
  Process.receive (function
    | (id, count) -> (id, count))

(* Main demonstration *)
let main () =
  (* Define our child workers *)
  let children = [
    make_worker "worker_a";
    make_worker "worker_b";
    make_worker "worker_c";
  ] in

  (* Start supervisor with one-for-one strategy *)
  let sup = Supervisor.start_link `OneForOne children in

  (* Give workers time to start *)
  Timer.sleep 100;

  (* Get the child pids *)
  let worker_pids = Supervisor.which_children sup in

  (* Ping all workers to verify they're running *)
  let responses = Lists.map (fun (id, pid) ->
    let response = ping_worker pid in
    (id, response)
  ) worker_pids in

  (* Increment worker_a a few times *)
  let (_, worker_a_pid) = Lists.hd worker_pids in
  let _ = Process.send worker_a_pid `Increment in
  let _ = Process.send worker_a_pid `Increment in
  let _ = Process.send worker_a_pid `Increment in

  let (_, count_before) = get_count worker_a_pid in

  (* Now crash worker_a - supervisor should restart it *)
  let _ = Process.send worker_a_pid `Crash in

  (* Wait for supervisor to restart the worker *)
  Timer.sleep 200;

  (* Get new worker pids - worker_a should have a new pid *)
  let new_pids = Supervisor.which_children sup in
  let (_, new_worker_a_pid) = Lists.hd new_pids in

  (* The restarted worker should have reset state (count = 0) *)
  let (_, count_after) = get_count new_worker_a_pid in

  (* Stop supervisor *)
  Supervisor.stop sup;

  (* Return results:
     - Initial ping responses
     - Count before crash
     - Count after restart (should be 0)
     - Whether pid changed (restart happened)
  *)
  (responses, count_before, count_after, worker_a_pid <> new_worker_a_pid)
