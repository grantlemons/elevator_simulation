open Types

type priority = time
type pqueue =
  | Empty
  | Node of priority * event * pqueue * pqueue * bool ref

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

let floor_occurs_sooner direction floor other = match direction with
  | Up -> floor < other
  | Down -> floor > other
;;

let get_prev elevator floor backup queue = 
  let elevator_match event = Option.equal (==) (extract_elevator event) (Some elevator) in
  let valid_node event = elevator_match event && floor_occurs_sooner elevator.direction (extract_floor event) floor in
  let rec internal = function
      | Node (p, e, l, r, { contents = true }) when valid_node e -> max_pair (p, Some e) @@ max_pair (internal l) (internal r)
      | Node (_, _, l, r, _) -> max_pair (internal l) (internal r)
      | Empty -> (backup, None)
  in
  internal queue

let rec remove_min queue =
  let rec remove = function
    | Empty as e -> e
    | Node (_, _, left, Empty, _) -> left
    | Node (_, _, Empty, right, _) -> right
    | Node (_, _, (Node (lprio, left_element, _, _, lvalid) as left),
                 (Node (rprio, right_element, _, _, rvalid) as right), _) ->
      if lprio <= rprio
      then Node (lprio, left_element, remove left, right, lvalid)
      else Node (rprio, right_element, left, remove right, rvalid)
  in
  match remove queue with
    | Empty as e -> e
    | Node (_, _, _, _, { contents = false }) as node -> remove_min node
    | Node (_, _, _, _, { contents = true }) as node -> node

let calc_priority element backup queue = match element with
  | Call (time, _, _) -> time
  | _ -> begin
    let elevator = Option.get @@ extract_elevator element in
    match get_prev elevator (extract_floor element) backup queue with
      | (p, Some e) -> p +. elevator.travel_time (extract_floor e) (extract_floor element)
      | (p, None) -> p +. elevator.travel_time elevator.floor (extract_floor element)
  end

let insert ?(prio_override) ?(backup=0.) element queue = 
  let prio = Option.value prio_override ~default:(calc_priority element backup queue) in

  let rec insert_value ?(valid_node=ref true) prio element = function
    | Empty -> Node (prio, element, Empty, Empty, valid_node)
    | Node (p, e, left, right, valid) ->
      if prio <= p
      then Node (prio, element, insert_value p e right ~valid_node:valid, left, valid_node)
      else Node (p, e, insert_value prio element right ~valid_node:valid_node, left, valid)
  in
  let new_queue = ref @@ insert_value prio element queue in

  let invalidate_old_events elevator queue =
    let invalid time event = Option.equal (==) (extract_elevator event) (Some elevator) && time > prio in
    let reinsertion_queue = ref [] in
    let rec internal = function
      | Node (p, e, l, r, ({ contents = true } as valid)) when invalid p e -> valid := false; reinsertion_queue := (e :: !reinsertion_queue); internal l; internal r
      | Node (_, _, l, r, _) -> internal l; internal r
      | Empty -> ()
    in
    internal queue;
    List.fold_left (fun acc event -> insert_value (calc_priority event backup acc) event acc) queue !reinsertion_queue
  in
  match extract_elevator element with
    | Some elevator -> invalidate_old_events elevator !new_queue
    | _ -> !new_queue
