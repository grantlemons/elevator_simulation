open Elevator_simulator
open Elevator_simulator.PriorityQueue
open Elevator_simulator.Elevator

let top_floor = 20;;
let simulation_time = 1000.;;
let travel_time_fn per_floor per_stop initial final = per_floor *. (Int.to_float @@ Int.abs (final - initial)) +. if final != initial then per_stop else 0.;;

Random.self_init ();;
let random_floor () = Random.int (top_floor + 1);;
let random_time () = Random.float simulation_time;;
let random_capacity () = Random.int (15 - 5) + 5;;
let random_direction () = match Random.bool () with
  | true -> Up
  | false -> Down
;;

let rec create_people = function
  | n when n == 0 -> []
  | n -> {
    id = n;
    destination = random_floor ();
    board_exit_duration = (Random.float 16.) +. 4.;
    call_time = None;
    board_time = None;
    exit_time = None;
  } :: create_people (n - 1)
;;

let rec create_elevators = function
  | n when n == 0 -> []
  | n -> {
    capacity = random_capacity ();
    travel_time = travel_time_fn 3. 10.;
    current_headcount = 0;
    floor = random_floor ();
    direction = random_direction ();
  } :: create_elevators (n - 1)
;;

let elevator = {
  capacity = 10;
  travel_time = travel_time_fn 3. 10.;
  current_headcount = 0;
  floor = 0;
  direction = Up;
};;

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
;;

let queue = Empty
  |> insert (Board (people.(0), elevator, 1))
  |> insert (Board (people.(1), elevator, 10))
  |> insert (Board (people.(3), elevator, 6))
  |> insert (Board (people.(4), elevator, 3))
;;

let windows ?(start=0) l = 
  let prev = ref start in
  List.map (fun x -> let tmp = !prev in prev := x; (tmp, x)) l
;;

let rec check_queue expected_order expected_times queue = match (queue, expected_order, expected_times) with
  | (Empty, [], []) -> true
  | (Node (p, c, _, _, { contents = true }), v :: rem_order, t :: rem_times) when (extract_floor c == v) && p == t -> check_queue rem_order rem_times (remove_min queue)
  | (Node (_, _, _, _, { contents = false }), _, _) -> check_queue expected_order expected_times (remove_min queue)
  | (_, _, _) -> false
;;

let expected_order = [1; 3; 6; 10];;
let prev = ref 0.;;
let expected_times = 
  windows expected_order
  |> List.map (fun (a, b) -> prev := !prev +. travel_time_fn 3. 10. a b; !prev)
;;
check_queue expected_order expected_times queue;;
