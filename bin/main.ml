open Elevator_simulator
open Elevator_simulator.Elevator
open Elevator_simulator.Types

let top_floor = 20;;
let simulation_soft_stop_time = 600.;;
let travel_time_fn per_floor per_stop initial final = per_floor *. (Int.to_float @@ Int.abs (final - initial)) +. if (final - initial) != 0 then per_stop else 0.;;

Random.self_init ();;
let random_floor () = Random.int (top_floor + 1);;
let random_time () = Random.float simulation_soft_stop_time;;
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
    } :: create_elevators (n - 1)
;;

let rec create_calls = function
  | [] -> PriorityQueue.Empty
  | p :: rem -> PriorityQueue.insert (Call (random_time(), p, random_floor ())) (create_calls rem) 
;;

let people = create_people 200;;
let elevators = create_elevators 6;;
let () = List.iteri (fun i e -> print_string "Elevator "; print_int (i + 1); print_string ": <"; print_string @@ dir_to_string e.direction; print_string "> ("; print_int e.floor; print_endline ")") elevators
let () = run_simulation (create_calls people) elevators top_floor;;
let () = print_string "Max total wait: "; print_float @@ max_total_wait people; print_endline "s";;
let () = print_string "Max call wait: "; print_float @@ max_call_wait people; print_endline "s";;
let () = print_string "Max time in elevator: "; print_float @@ max_exit_wait people; print_endline "s";;
