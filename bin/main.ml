open Elevator_simulator
open Elevator_simulator.Elevator

let people = [|{
  id = 1;
  call_time = 0.;
  destination = 0;
  board_exit_duration = 4.;
  arrival_time = None;
}
;{
  id = 2;
  call_time = 0.;
  destination = 3;
  board_exit_duration = 4.;
  arrival_time = None;
}|];;

let top_floor = 20;;
let travel_time_fn per_floor per_stop initial final = per_floor *. (Int.to_float @@ Int.abs (final - initial)) +. if (final - initial) != 0 then per_stop else 0.;;

let elevators = [|{
  capacity = 3;
  travel_time = travel_time_fn 3. 10.;
  current_headcount = 0;
  floor = 20;
  direction = Down;
  elapsed_time = 0.;
}|];;

let queue =
  PriorityQueue.insert (
    PriorityQueue.insert (
      PriorityQueue.insert
        PriorityQueue.Empty
      0 (Call (people.(0), 10, Down)))
    top_floor (ToggleDirection elevators.(0)))
  0 (Call (people.(1), 2, Up))
;;

run_simulation queue (Array.to_list elevators);;
