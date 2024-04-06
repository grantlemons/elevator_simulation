open Elevator_simulator
open Elevator_simulator.Elevator

let people = [|{
  call_time = 0.;
  destination = 0;
  board_exit_duration = 4.;
  arrival_time = None;
}
;{
  call_time = 0.;
  destination = 3;
  board_exit_duration = 4.;
  arrival_time = None;
}|];;

let queue = PriorityQueue.insert PriorityQueue.Empty 0 (Call (people.(0), 10));;

let elevators = [{
  capacity = 3;
  current_headcount = 0;
  floor = 20;
  direction = Down;
}];;

run_simulation queue elevators;;
