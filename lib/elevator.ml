type time = float;;
type floor = int;;
type direction =
  | Up
  | Down
;;

let clock = ref 0.;;

type person = {
  call_time : time;
  destination : floor;
  board_exit_duration : time;
  mutable arrival_time : time option;
};;

type elevator = {
  capacity : int;
  mutable current_headcount: int;
  mutable floor : floor;
  mutable direction : direction;
};;

type event =
  | Call of person * floor
  | Board of person * elevator * floor
  | Exit of person * elevator * floor
;;

let rec run_simulation ?(delayed_calls=[]) queue elevators =
  let insert_event event =
    let get_elevator_priority elevator destination = match elevator.direction with
      | Up -> destination - elevator.floor
      | Down -> elevator.floor - destination
    in
  match event with
  | Board(_, elevator, floor) -> run_simulation (PriorityQueue.insert (PriorityQueue.remove_min queue) (get_elevator_priority elevator floor) event) elevators ~delayed_calls:delayed_calls
  | Exit(_, elevator, floor) -> run_simulation (PriorityQueue.insert (PriorityQueue.remove_min queue) (get_elevator_priority elevator floor) event) elevators ~delayed_calls:delayed_calls
  | _ -> run_simulation (PriorityQueue.remove_min queue) elevators ~delayed_calls:delayed_calls
  in

  let board_handler person elevator floor = if elevator.current_headcount < elevator.capacity then
    clock := !clock +. person.board_exit_duration;

    elevator.current_headcount <- elevator.current_headcount + 1;
    elevator.floor <- floor;

    insert_event (Exit (person, elevator, person.destination))
  in

  let exit_handler person elevator floor =
    clock := !clock +. person.board_exit_duration;

    elevator.current_headcount <- elevator.current_headcount - 1;
    elevator.floor <- floor;

    print_string "Person arrived";

    person.arrival_time <- Some !clock
  in

  let call_handler person floor = 
    let is_valid_call elevator floor = match elevator.direction with
      | Up when floor >= elevator.floor -> true
      | Down when floor <= elevator.floor -> true
      | _ -> false
    in
    List.filter (fun e -> e.current_headcount < e.capacity) elevators
    |> List.sort (fun e1 e2 -> Int.compare e1.current_headcount e2.current_headcount)
    |> List.find_opt (fun e -> is_valid_call e floor)
    |> function
      | Some elevator -> insert_event @@ Board (person, elevator, floor)
      | None -> run_simulation (PriorityQueue.remove_min queue) elevators ~delayed_calls:(Call (person, floor) :: delayed_calls)
  in

  let event_handler = function
    | Call(person, floor) -> call_handler person floor
    | Board(person, elevator, floor) -> board_handler person elevator floor
    | Exit(person, elevator, floor) -> exit_handler person elevator floor
  in

  match queue with
  | Empty -> ()
  | Node(_, event, _, _) -> event_handler event
;;
