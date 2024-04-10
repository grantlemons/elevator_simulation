type time = float
type priority = time
type floor = int
type direction =
  | Up
  | Down

type person = {
  id : int;
  destination : floor;
  board_exit_duration : time;
  mutable call_time : time option;
  mutable board_time : time option;
  mutable exit_time : time option;
}

type elevator = {
  capacity : int;
  travel_time : floor -> floor -> time;
  mutable current_headcount : int;
  mutable floor : floor;
  mutable direction : direction;
}

type event =
  | Call of time * person * floor
  | Board of person * elevator * floor
  | Exit of person * elevator * floor

type pqueue =
  | Empty of time
  | Node of priority * event * pqueue * pqueue * bool ref

let extract_person = function
  | Call (_, person, _) -> person
  | Board (person, _, _) -> person
  | Exit (person, _, _) -> person

let extract_elevator = function
  | Call _ -> None
  | Board (_, elevator, _) -> Some elevator
  | Exit (_, elevator, _) -> Some elevator

let extract_floor = function
  | Call (_, _, floor) -> floor
  | Board (_, _, floor) -> floor
  | Exit (_, _, floor) -> floor

let extract_time = function
  | Call (time, _, _) -> Some time
  | Board (_, _, _) -> None
  | Exit (_, _, _) -> None

let rec count = function
  | Node (_, _, left, right, { contents = true }) -> 1 + count left + count right
  | Node (_, _, left, right, _) -> 0 + count left + count right
  | _ -> 0

let leaf_valid = function
  | Node (_, _, l, r, { contents = true }) when count l + count r == 0 -> true
  | _ -> false

let rec count = function
  | Node (_, _, left, right, { contents = true }) -> 1 + count left + count right
  | Node (_, _, left, right, _) -> 0 + count left + count right
  | _ -> 0

let max_pair pair1 pair2 = match (pair1, pair2) with
  | ((_, Some _), (_, None)) -> pair1
  | ((_, None), (_, Some _)) -> pair2
  | ((p1, _), (p2, _)) when p1 > p2 -> pair1
  | ((p1, _), (p2, _)) when p2 > p1 -> pair2
  | _ -> pair1

let rec get_prev ?(prev) elevator floor queue = match (queue, prev) with
  | (Empty t, _) -> (t, None)
  | (Node (_, e, _, _, { contents = true }), Some prev)
    when Option.equal (==) (extract_elevator e) (Some elevator) && (extract_floor e) > floor -> prev
  | (Node (p, e, l, r, { contents = true }), _)
    when Option.equal (==) (extract_elevator e) (Some elevator)
    -> max_pair (get_prev elevator floor l ~prev:(p, Some e)) (get_prev elevator floor r ~prev:(p, Some e))
  | (Node (_, _, l, r, _), Some prev) -> max_pair (get_prev elevator floor l ~prev:prev) (get_prev elevator floor r ~prev:prev)
  | (Node (_, _, l, r, _), None) -> max_pair (get_prev elevator floor l) (get_prev elevator floor r)

let rec remove_min =
  let rec remove = function
    | Empty _ as e -> e
    | Node (_, _, left, Empty _, _) -> left
    | Node (_, _, Empty _, right, _) -> right
    | Node (_, _, (Node (lprio, left_element, _, _, _) as left),
                 (Node (rprio, right_element, _, _, _) as right), _) ->
      if lprio <= rprio
      then Node (lprio, left_element, remove left, right, ref true)
      else Node (rprio, right_element, left, remove right, ref true)
  in
  function
    | Empty t as e -> (t, e)
    | Node (_, _, _, _, { contents = false }) as node -> remove_min @@ remove node
    | Node (p, _, _, _, { contents = true }) as node -> (p, remove node)

let calc_priority element queue = match element with
  | Call (time, _, _) -> time
  | _ -> begin
    let elevator = Option.get @@ extract_elevator element in
    match get_prev elevator (extract_floor element) queue with
      | (p, Some e) -> p +. elevator.travel_time (extract_floor e) (extract_floor element)
      | (p, None) -> p +. elevator.travel_time elevator.floor (extract_floor element)
  end

let rec insert ?(priority) element queue = 
  let prio = Option.value priority ~default:(calc_priority element queue) in

  let insert_value = match queue with
    | Empty _ -> Node (prio, element, Empty prio, Empty prio, ref true)
    | Node (p, e, left, right, _) ->
      if prio <= p
      then Node (prio, element, insert e right ~priority:p, left, ref true)
      else Node (p, e, insert element right ~priority:prio, left, ref true)
  in
  let new_queue = ref insert_value in
  let elevator = extract_elevator element in

  let reinsert_value event = new_queue := insert event !new_queue in
  let rec invalidate_low_priority = function
    | Empty _ -> ()
    | Node (p, e, l, r, value) when p > prio && Option.equal (==) (extract_elevator e) elevator -> value := false; reinsert_value e; invalidate_low_priority l; invalidate_low_priority r
    | Node (_, _, l, r, _) -> invalidate_low_priority l; invalidate_low_priority r
  in
  if Option.is_some elevator then invalidate_low_priority !new_queue;
  !new_queue
