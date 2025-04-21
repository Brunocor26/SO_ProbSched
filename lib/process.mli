type process_state = Ready | Running | Waiting | Terminated
type t = {
  id : int;
  name : string;
  arrival_time : int;
  burst_time : int;
  priority : int;
  mutable remaining_burst_time : int;
  mutable state : process_state;
  mutable completion_time : int option;
  mutable waiting_time : int;
  mutable turnaround_time : int option;
  period : int option;
  deadline : int option;
}
val string_of_state : process_state -> string
val create :
  id:int ->
  ?name:string ->
  arrival_time:int ->
  burst_time:int ->
  priority:int ->
  ?initial_state:process_state -> ?period:int -> ?deadline:int -> unit -> t
val is_terminated : t -> bool
val get_turnaround_time : t -> int option
val string_of_process : t -> string
