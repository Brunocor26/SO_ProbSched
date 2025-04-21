val algo_ref : string ref
val file_ref : string ref
val quantum_ref : int option ref
val usage_msg : string
val speclist : (string * Arg.spec * string) list
val stats_to_json :
  Prob_sched_lib.Statistics.simulation_stats -> Yojson.Basic.t
val format_timeline_string :
  Prob_sched_lib.Scheduler.timeline_event list -> int -> string
val run_and_output : unit -> unit
