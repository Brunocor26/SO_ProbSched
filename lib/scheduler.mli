open Process

type timeline_event = {
  time : int;
  process_id : int;  (* -1 para CPU livre *)
  new_state : process_state;
}

val get_option_value : 'a option -> string -> 'a

val gerar_instancias_periodicas : t list -> int -> t list
val log_event : timeline_event list ref -> int -> int -> process_state -> unit
val reset_process : t -> unit
val sort_by_arrival : t list -> t list

val fcfs : t list -> int * timeline_event list
val sjf : t list -> int * timeline_event list
val priority_non_preemptive : t list -> int * timeline_event list
val priority_preemptive : t list -> int * timeline_event list
val round_robin : t list -> quantum:int -> int * timeline_event list

val rate_monotonic : t list -> tempo_max:int -> int * timeline_event list
val edf : t list -> tempo_max:int -> int * timeline_event list