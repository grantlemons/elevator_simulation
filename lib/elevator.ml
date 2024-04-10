open PriorityQueue

let max_call_wait people = List.fold_left (fun acc x -> max acc (Option.get x.board_time -. Option.get x.call_time)) 0. people;;
let max_exit_wait people = List.fold_left (fun acc x -> max acc (Option.get x.exit_time -. Option.get x.board_time)) 0. people;;
let max_total_wait people = List.fold_left (fun acc x -> max acc (Option.get x.exit_time -. Option.get x.call_time)) 0. people;;
let direction s e = if e > s then Up else Down;;
let dir_to_string = function
  | Up -> "Up"
  | Down -> "Down"
;;
let distance_from_destination direction floor destination = match direction with
  | Up -> destination - floor
  | Down -> floor - destination
;;

let rec run_simulation ?(delayed_calls=[]) queue elevators top_floor =
  let (time, rem_queue) = PriorityQueue.remove_min queue in

  let insert_event = function
    | Board _ as event -> run_simulation (PriorityQueue.insert rem_queue event) elevators top_floor ~delayed_calls:delayed_calls
    | Exit _ as event -> run_simulation (PriorityQueue.insert rem_queue event) elevators top_floor ~delayed_calls:delayed_calls
    | _ -> ()
  in

  let insert_delayed_call call = run_simulation rem_queue elevators top_floor ~delayed_calls:(call :: delayed_calls) in

  let goto_floor elevator floor = 
    elevator.floor <- floor
  in

  let board_handler person elevator floor = if elevator.current_headcount < elevator.capacity
    then
      begin
        goto_floor elevator floor;
        elevator.current_headcount <- elevator.current_headcount + 1;

        person.board_time <- Some time;

        insert_event (Exit (person, elevator, person.destination))
      end
    else
      begin
        (* The elevator still goes to the floor, just no one boards and no destinations are added *)
        goto_floor elevator floor;

        insert_delayed_call @@ Call (time, person, floor)
      end
  in

  let exit_handler person elevator floor =
    goto_floor elevator floor;
    elevator.current_headcount <- elevator.current_headcount - 1;

    person.exit_time <- Some time;

    run_simulation rem_queue elevators top_floor ~delayed_calls:delayed_calls
  in

  let call_handler person floor = 
    let is_valid_call elevator floor = match (elevator.direction, direction floor person.destination) with
      | (Up, Up) when floor >= elevator.floor -> true
      | (Down, Down) when floor <= elevator.floor -> true
      | _ -> false
    in

    person.call_time <- Some time;

    List.sort (fun e1 e2 -> Int.compare e1.current_headcount e2.current_headcount) elevators
    |> List.find_opt (fun e -> is_valid_call e floor)
    |> function
      | Some elevator -> insert_event @@ Board (person, elevator, floor)
      | None -> insert_delayed_call @@ Call (time, person, floor)

  in

  let toggle_handler () =
    let toggle_direction elevator = match elevator.direction with
      | Up -> elevator.direction <- Down; goto_floor elevator top_floor
      | Down -> elevator.direction <- Up; goto_floor elevator 0
    in

    List.iter toggle_direction elevators;
    run_simulation (List.fold_left (fun q call -> PriorityQueue.insert q call) queue delayed_calls) elevators top_floor
  in

  let event_handler = function
    | Call(_, person, floor) -> call_handler person floor
    | Board(person, elevator, floor) -> board_handler person elevator floor
    | Exit(person, elevator, floor) -> exit_handler person elevator floor
  in

  match (queue, delayed_calls) with
  | (Empty _, []) -> ()
  | (Empty _, _) -> toggle_handler ()
  | (Node(_, event, _, _, _), _) -> event_handler event
;;
