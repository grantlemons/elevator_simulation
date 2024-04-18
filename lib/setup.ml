open Types

let top_floor = 20
let people = [|{
  id = 1;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 2;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 3;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 4;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 5;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 5;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
;{
  id = 7;
  destination = top_floor;
  board_exit_duration = 4.;
  call_time = None;
  board_time = None;
  exit_time = None;
}
|]

let travel_time_fn per_floor per_stop initial final = per_floor *. (Int.to_float @@ Int.abs (final - initial)) +. if final != initial then per_stop else 0.;;
let elevator = {
  capacity = 10;
  travel_time = travel_time_fn 3. 10.;
  current_headcount = 0;
  floor = 0;
  direction = Up;
}
