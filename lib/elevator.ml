open Types
open PriorityQueue

let max_call_wait people = List.fold_left (fun acc x -> max acc (Option.get x.board_time -. Option.get x.call_time)) 0. people;;
let max_exit_wait people = List.fold_left (fun acc x -> max acc (Option.get x.exit_time -. Option.get x.board_time)) 0. people;;
let max_total_wait people = List.fold_left (fun acc x -> max acc (Option.get x.exit_time -. Option.get x.call_time)) 0. people;;

let rec run_simulation ?(delayed_calls=[]) ?(time=0.) queue elevators top_floor =
  let time = match queue with
    | Node (p, _, _, _, _) -> p
    | Empty -> time
  in
  let rem_queue = PriorityQueue.remove_min queue in

  let insert_event = function
    | Board _ as event -> run_simulation (PriorityQueue.insert ~backup:time event rem_queue) elevators top_floor ~delayed_calls:delayed_calls ~time:time
    | Exit _ as event -> run_simulation (PriorityQueue.insert ~backup:time event rem_queue) elevators top_floor ~delayed_calls:delayed_calls ~time:time
    | _ -> ()
  in

  let insert_delayed_call call = run_simulation (List.fold_left (fun acc elevator -> PriorityQueue.insert ~backup:time (ChangeDirection (elevator, dir_opposite elevator.direction, dir_eol top_floor elevator.direction)) acc) rem_queue elevators) elevators top_floor ~delayed_calls:(call :: delayed_calls) ~time:time in

  let goto_floor elevator floor = 
    elevator.floor <- floor
  in

  let board_handler person elevator floor = if elevator.current_headcount < elevator.capacity
    then
      begin
        goto_floor elevator floor;
        elevator.current_headcount <- elevator.current_headcount + 1;

        print_float time;
        print_string " ";
        print_int person.id;
        print_string " Boarded! (";
        print_int floor;
        print_endline ")";
        person.board_time <- Some time;

        insert_event (Exit (person, elevator, person.destination))
      end
    else
      begin
        (* The elevator still goes to the floor, just no one boards and no destinations are added *)
        goto_floor elevator floor;

        print_float time;
        print_string " ";
        print_int person.id;
        print_endline " Delayed! (Capacity)";

        insert_delayed_call @@ Call (time, person, floor)
      end
  in

  let exit_handler person elevator floor =
    goto_floor elevator floor;
    elevator.current_headcount <- elevator.current_headcount - 1;

    print_float time;
    print_string " ";
    print_int person.id;
    print_string " Exited! (";
    print_int floor;
    print_endline ")";
    person.exit_time <- Some time;

    run_simulation rem_queue elevators top_floor ~delayed_calls:delayed_calls ~time:time
  in

  let call_handler person floor = 
    let is_valid_call elevator floor = match (elevator.direction, direction floor person.destination) with
      | (Up, Up) when floor >= elevator.floor -> true
      | (Down, Down) when floor <= elevator.floor -> true
      | _ -> false
    in

    print_float time;
    print_string " ";
    print_int person.id;
    print_string " Called (";
    print_int floor;
    print_string ") <";
    print_string @@ dir_to_string @@ direction floor person.destination;
    print_endline ">";

    person.call_time <- Some time;

    List.sort (fun e1 e2 -> Int.compare e1.current_headcount e2.current_headcount) elevators
    |> List.find_opt (fun e -> is_valid_call e floor)
    |> function
      | Some elevator -> insert_event @@ Board (person, elevator, floor)
      | None -> begin
        print_float time;
        print_string " ";
        print_int person.id;
        print_endline " Delayed! (Directional)";
        insert_delayed_call @@ Call (time, person, floor)
      end

  in

  let change_dir_handler elevator direction floor = if direction != elevator.direction then
    begin
      print_float time;
      print_string " Turned Around! <";
      print_string @@ dir_to_string elevator.direction;
      print_string "> <";
      elevator.direction <- direction;
      print_string @@ dir_to_string elevator.direction;
      print_endline ">";

      goto_floor elevator floor;
      run_simulation (List.fold_left (fun q call -> PriorityQueue.insert call q ~prio_override:time) rem_queue delayed_calls) elevators top_floor ~time:time
    end else
      run_simulation rem_queue elevators top_floor
  in

  let event_handler = function
    | Call (_, person, floor) -> call_handler person floor
    | Board (person, elevator, floor) -> board_handler person elevator floor
    | Exit (person, elevator, floor) -> exit_handler person elevator floor
    | ChangeDirection (elevator, direction, floor) -> change_dir_handler elevator direction floor
  in

  match (queue, delayed_calls) with
  | (Empty, []) -> ()
  | (Empty, _) -> ()
  | (Node(_, event, _, _, _), _) -> event_handler event
;;
