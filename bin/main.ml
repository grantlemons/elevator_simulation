open Elevator_simulator
open Elevator_simulator.Elevator

let top_floor = 20;;
let travel_time_fn per_floor per_stop initial final = per_floor *. (Int.to_float @@ Int.abs (final - initial)) +. if (final - initial) != 0 then per_stop else 0.;;

Random.self_init ();;
let random_floor () = Random.int (top_floor + 1);;
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
  | n -> let cap = random_capacity () in
    {
      capacity = cap;
      travel_time = travel_time_fn 3. 10.;
      current_headcount = 0;
      floor = random_floor ();
      direction = random_direction ();
      local_elapsed = 0.;
    } :: create_elevators (n - 1)
;;

let rec create_calls = function
  | [] -> PriorityQueue.Empty
  | p :: rem -> PriorityQueue.insert (create_calls rem) min_int (Call (p, random_floor ()))
;;

let elevators = create_elevators 6;;
let () = run_simulation (create_calls @@ create_people 200) elevators top_floor;;
let () = print_float @@ max_elapsed elevators; print_endline "s";;
