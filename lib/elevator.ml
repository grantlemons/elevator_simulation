type time = float;;
type floor = int;;
type direction =
  | Up
  | Down
;;

type person = {
  id : int;
  call_time : time;
  destination : floor;
  board_exit_duration : time;
  mutable arrival_time : time option;
};;

type elevator = {
  capacity : int;
  travel_time : floor -> floor -> time;
  mutable current_headcount: int;
  mutable floor : floor;
  mutable direction : direction;
  mutable elapsed_time : time;
};;

type event =
  | Call of person * floor * direction
  | Board of person * elevator * floor
  | Exit of person * elevator * floor
  | ToggleDirection of elevator
;;

let rec run_simulation ?(delayed_calls=[]) queue elevators =
  let rem_queue = PriorityQueue.remove_min queue in

  let insert_event event =
    let get_elevator_priority elevator destination = match elevator.direction with
      | Up -> destination - elevator.floor
      | Down -> elevator.floor - destination
    in
  match event with
    | Board(_, elevator, floor) -> run_simulation (PriorityQueue.insert rem_queue (get_elevator_priority elevator floor) event) elevators ~delayed_calls:delayed_calls
    | Exit(_, elevator, floor) -> run_simulation (PriorityQueue.insert rem_queue (get_elevator_priority elevator floor) event) elevators ~delayed_calls:delayed_calls
    | _ -> ()
  in

  let board_handler person elevator floor = if elevator.current_headcount < elevator.capacity
    then
      begin
        elevator.elapsed_time <- elevator.elapsed_time +. person.board_exit_duration;
        elevator.elapsed_time <- elevator.elapsed_time +. elevator.travel_time elevator.floor floor;
        elevator.current_headcount <- elevator.current_headcount + 1;
        elevator.floor <- floor;

        insert_event (Exit (person, elevator, person.destination))
      end
    else
      (* The elevator still goes to the floor, just no one boards and no destinations are added *)
      elevator.elapsed_time <- elevator.elapsed_time +. elevator.travel_time elevator.floor floor;
      elevator.floor <- floor;
      run_simulation rem_queue elevators ~delayed_calls:(Call (person, floor, elevator.direction) :: delayed_calls)
  in

  let exit_handler person elevator floor =
    elevator.elapsed_time <- elevator.elapsed_time +. person.board_exit_duration;
    elevator.elapsed_time <- elevator.elapsed_time +. elevator.travel_time elevator.floor floor;
    elevator.current_headcount <- elevator.current_headcount - 1;
    elevator.floor <- floor;

    person.arrival_time <- Some elevator.elapsed_time;

    run_simulation rem_queue elevators ~delayed_calls:delayed_calls
  in

  let call_handler person floor direction = 
    let is_valid_call elevator floor = match (elevator.direction, direction) with
      | (Up, Up) when floor >= elevator.floor -> true
      | (Down, Down) when floor <= elevator.floor -> true
      | _ -> false
    in
    List.sort (fun e1 e2 -> Int.compare e1.current_headcount e2.current_headcount) elevators
    |> List.find_opt (fun e -> is_valid_call e floor)
    |> function
      | Some elevator -> insert_event @@ Board (person, elevator, floor)
      | None -> run_simulation rem_queue elevators ~delayed_calls:(Call (person, floor, direction) :: delayed_calls)
  in

  let toggle_handler elevator = match elevator.direction with
    | Up -> elevator.direction <- Down
    | Down -> elevator.direction <- Up;

    run_simulation (List.fold_left (fun q call -> PriorityQueue.insert q 0 call) rem_queue delayed_calls) elevators
  in

  let event_handler = function
    | Call(person, floor, direction) -> call_handler person floor direction
    | Board(person, elevator, floor) -> board_handler person elevator floor
    | Exit(person, elevator, floor) -> exit_handler person elevator floor
    | ToggleDirection(elevator) -> toggle_handler elevator
  in

  match queue with
  | Empty -> ()
  | Node(_, event, _, _) -> event_handler event
;;

let elapsed_time elevators = List.fold_left (fun acc x -> max acc x.elapsed_time) 0. elevators;;
