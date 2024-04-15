open Elevator_simulator.PriorityQueue
open Elevator_simulator.Types
open Alcotest

let top_floor = 20
let people = [|{
  id = 1;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 2;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 3;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 4;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 5;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 5;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 7;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
|]
let travel_time_fn per_floor per_stop initial final = per_floor *. (Int.to_float @@ Int.abs (final - initial)) +. if final != initial then per_stop else 0.;;
let elevator = {
  capacity = 10;
  travel_time = travel_time_fn 3. 10.;
  current_headcount = 0;
  floor = 0;
  direction = Up;
};;

let windows ?(start=0) l = 
  let prev = ref start in
  List.map (fun x -> let tmp = !prev in prev := x; (tmp, x)) l

let verify_order pqueue expected_order =
  let rec check_queue expected_order queue = match (queue, expected_order) with
    | (Empty, []) -> true
    | (Node (_, c, _, _, { contents = true }), v :: rem_order) when (extract_floor c == v) -> check_queue rem_order (remove_min queue)
    | (Node (_, _, _, _, { contents = false }), _) -> check_queue expected_order (remove_min queue)
    | (_, _) -> false
  in
  check_queue expected_order pqueue

let verify_times pqueue expected_times =
  let rec check_queue expected_times queue = match (queue, expected_times) with
    | (Empty, []) -> true
    | (Node (p, _, _, _, { contents = true }), t :: rem_times) when p == t -> check_queue rem_times (remove_min queue)
    | (Node (_, _, _, _, { contents = false }), _) -> check_queue expected_times (remove_min queue)
    | (_, _) -> false
  in
  check_queue expected_times pqueue

let out_of_order_board_order () =
  let pqueue = Empty
    |> insert (Board (people.(0), elevator, 1))
    |> insert (Board (people.(1), elevator, 10))
    |> insert (Board (people.(3), elevator, 6))
    |> insert (Board (people.(4), elevator, 3))
  in
  let expected_order = [1; 3; 6; 10] in
  check bool "Order Correct" (verify_order pqueue expected_order) true

let out_of_order_board_times () =
  let pqueue = Empty
    |> insert (Board (people.(0), elevator, 1))
    |> insert (Board (people.(1), elevator, 10))
    |> insert (Board (people.(3), elevator, 6))
    |> insert (Board (people.(4), elevator, 3))
  in
  let expected_order = [1; 3; 6; 10] in
  let expected_times = 
    let prev = ref 0. in
    windows expected_order
    |> List.map (fun (a, b) -> prev := !prev +. travel_time_fn 3. 10. a b +. 4.; !prev)
  in
  check bool "Times Correct" (verify_times pqueue expected_times) true

let correct_order_suite =
  [ "out of order boards", `Quick, out_of_order_board_order
  ]

let correct_times_suite =
  [ "out of order boards", `Quick, out_of_order_board_times
  ]

let () =
  Alcotest.run "PQueue"
  [ "Ordering", correct_order_suite
  ; "Timing", correct_times_suite
  ]
