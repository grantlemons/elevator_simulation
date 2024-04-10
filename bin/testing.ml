open Elevator_simulator
open Elevator_simulator.PriorityQueue
open Elevator_simulator.Elevator

let top_floor = 20;;
let simulation_time = 1000.;;
let travel_time_fn per_floor per_stop initial final = per_floor *. (Int.to_float @@ Int.abs (final - initial)) +. if (final - initial) != 0 then per_stop else 0.;;

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
  capacity = random_capacity ();
  travel_time = travel_time_fn 3. 10.;
  current_headcount = 0;
  floor = 0;
  direction = random_direction ();
};;

let people = [|{
    id = 1;
    destination = top_floor;
    board_exit_duration = (Random.float 16.) +. 4.;
    call_time = None;
    board_time = None;
    exit_time = None;
}
;{
    id = 2;
    destination = top_floor;
    board_exit_duration = (Random.float 16.) +. 4.;
    call_time = None;
    board_time = None;
    exit_time = None;
}
;{
    id = 3;
    destination = top_floor;
    board_exit_duration = (Random.float 16.) +. 4.;
    call_time = None;
    board_time = None;
    exit_time = None;
}
|]
;;

let queue = Empty 0.
  |> insert (Board (people.(0), elevator, 0))
  |> insert (Board (people.(2), elevator, 2))
  |> insert (Board (people.(1), elevator, 5))
