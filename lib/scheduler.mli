type timeline_event = {
  time : int;
  process_id : int;
  new_state : Process.process_state;
}
val get_option_value : 'a option -> string -> 'a
val log_event :
  timeline_event list ref -> int -> int -> Process.process_state -> unit
val reset_process : Process.t -> unit
val sort_by_arrival : Process.t list -> Process.t list
val fcfs : Process.t list -> int * timeline_event list
val sjf : Process.t list -> int * timeline_event list
val priority_non_preemptive : Process.t list -> int * timeline_event list
val priority_preemptive : Process.t list -> int * timeline_event list
val round_robin : Process.t list -> quantum:int -> int * timeline_event list
