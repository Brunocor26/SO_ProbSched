open Process

type timeline_event = {
  time : int;
  process_id : int;
  new_state : process_state;
  instance_id : int option;
}

val all_instances : t list ref
val completed_instances : t list ref
val next_instance_id : int ref

val get_completed_instances : unit -> t list
val get_all_instances : unit -> t list

val get_option_value : 'a option -> string -> 'a
val get_next_instance_id : unit -> int
val get_structured_instance_id : int -> int -> int

val gerar_instancias_periodicas : t list -> int -> t list

val log_event : timeline_event list ref -> int -> int -> process_state -> unit
val log_event_with_instance : timeline_event list ref -> int -> int -> process_state -> int -> unit

val reset_process : t -> unit

val sort_by_arrival : t list -> t list

val fcfs : t list -> int * timeline_event list
val sjf : t list -> int * timeline_event list
val priority_non_preemptive : t list -> int * timeline_event list
val priority_preemptive : t list -> int * timeline_event list
val round_robin : t list -> quantum:int -> int * timeline_event list
val edf : ?tempo_max:int -> t list -> int * timeline_event list
val rate_monotonic : ?tempo_max:int -> t list -> int * timeline_event list
