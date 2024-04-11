type time = float
type floor = int
type direction =
  | Up
  | Down

let direction s e = if e > s then Up else Down;;
let dir_eol top_floor = function
  | Up -> top_floor
  | Down -> 0
let dir_to_string = function
  | Up -> "Up"
  | Down -> "Down"
let dir_opposite = function
  | Up -> Down
  | Down -> Up

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
  | ChangeDirection of elevator * direction * floor

let extract_person = function
  | Call (_, person, _) -> Some person
  | Board (person, _, _) -> Some person
  | Exit (person, _, _) -> Some person
  | ChangeDirection _ -> None

let extract_elevator = function
  | Call _ -> None
  | Board (_, elevator, _) -> Some elevator
  | Exit (_, elevator, _) -> Some elevator
  | ChangeDirection (elevator, _, _) -> Some elevator

let extract_floor = function
  | Call (_, _, floor) -> floor
  | Board (_, _, floor) -> floor
  | Exit (_, _, floor) -> floor
  | ChangeDirection (_, _, floor) -> floor

let extract_time = function
  | Call (time, _, _) -> Some time
  | Board _ -> None
  | Exit _ -> None
  | ChangeDirection _ -> None
