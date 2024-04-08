type time = float;;
type floor = int;;
type direction =
  | Up
  | Down
;;

type person = {
  id : int;
  destination : floor;
  board_exit_duration : time;
  mutable call_time : time option;
  mutable board_time : time option;
  mutable exit_time : time option;
};;

type elevator = {
  capacity : int;
  travel_time : floor -> floor -> time;
  mutable local_elapsed : time;
  mutable current_headcount : int;
  mutable floor : floor;
  mutable direction : direction;
};;

type event =
  | Call of person * floor
  | Board of person * elevator * floor
  | Exit of person * elevator * floor
;;

let max_elapsed elevators = List.fold_left (fun acc x -> max acc x.local_elapsed) 0. elevators;;
let direction s e = if e > s then Up else Down;;
let dir_to_string = function
  | Up -> "Up"
  | Down -> "Down"
;;
let distance_from_destination direction floor destination = match direction with
  | Up -> destination - floor
  | Down -> floor - destination
;;

let rec run_simulation ?(delayed_calls=[]) ?(elapsed_time=0.) ?(index=0) queue elevators top_floor =
  let rem_queue = PriorityQueue.remove_min queue in
  let elapsed = ref elapsed_time in

  let insert_event event =
  match event with
    | Board(_, elevator, floor) -> run_simulation (PriorityQueue.insert rem_queue (distance_from_destination elevator.direction elevator.floor floor) event) elevators top_floor ~delayed_calls:delayed_calls ~elapsed_time:!elapsed ~index:(index + 1)
    | Exit(_, elevator, floor) -> run_simulation (PriorityQueue.insert rem_queue (distance_from_destination elevator.direction elevator.floor floor) event) elevators top_floor ~delayed_calls:delayed_calls ~elapsed_time:!elapsed ~index:(index + 1)
    | _ -> ()
  in

  let insert_delayed_call call = run_simulation rem_queue elevators top_floor ~delayed_calls:(call :: delayed_calls) ~elapsed_time:!elapsed ~index:(index + 1) in

  let goto_floor elevator floor = 
    elevator.local_elapsed <- elevator.local_elapsed +. elevator.travel_time elevator.floor floor;
    elevator.floor <- floor
  in

  let board_handler person elevator floor = if elevator.current_headcount < elevator.capacity
    then
      begin
        goto_floor elevator floor;
        elevator.local_elapsed <- elevator.local_elapsed +. person.board_exit_duration;
        elevator.current_headcount <- elevator.current_headcount + 1;
        elapsed := max_elapsed elevators;

        person.board_time <- Some !elapsed;

        insert_event (Exit (person, elevator, person.destination))
      end
    else
      begin
        (* The elevator still goes to the floor, just no one boards and no destinations are added *)
        goto_floor elevator floor;
        elapsed := max_elapsed elevators;

        insert_delayed_call @@ Call (person, floor)
      end
  in

  let exit_handler person elevator floor =
    goto_floor elevator floor;
    elevator.local_elapsed <- elevator.local_elapsed +. person.board_exit_duration;
    elevator.current_headcount <- elevator.current_headcount - 1;
    elapsed := max_elapsed elevators;

    person.exit_time <- Some !elapsed;

    run_simulation rem_queue elevators top_floor ~delayed_calls:delayed_calls ~elapsed_time:!elapsed ~index:(index + 1)
  in

  let call_handler person floor = 
    let is_valid_call elevator floor = match (elevator.direction, direction floor person.destination) with
      | (Up, Up) when floor >= elevator.floor -> true
      | (Down, Down) when floor <= elevator.floor -> true
      | _ -> false
    in
    person.call_time <- Some !elapsed;
    List.sort (fun e1 e2 -> Int.compare e1.current_headcount e2.current_headcount) elevators
    |> List.find_opt (fun e -> is_valid_call e floor)
    |> function
      | Some elevator -> insert_event @@ Board (person, elevator, floor)
      | None -> insert_delayed_call @@ Call (person, floor)

  in

  let toggle_handler () =
    let toggle_direction elevator = match elevator.direction with
      | Up -> elevator.direction <- Down; goto_floor elevator top_floor
      | Down -> elevator.direction <- Up; goto_floor elevator 0
    in

    List.iter toggle_direction elevators;
    run_simulation (List.fold_left (fun q call -> PriorityQueue.insert q min_int call) PriorityQueue.Empty delayed_calls) elevators top_floor ~elapsed_time:!elapsed ~index:(index + 1)
  in

  let event_handler = function
    | Call(person, floor) -> call_handler person floor
    | Board(person, elevator, floor) -> board_handler person elevator floor
    | Exit(person, elevator, floor) -> exit_handler person elevator floor
  in

  match (queue, delayed_calls) with
  | (Empty, []) -> ()
  | (Empty, _) -> toggle_handler ()
  | (Node(_, event, _, _), _) -> event_handler event
;;
