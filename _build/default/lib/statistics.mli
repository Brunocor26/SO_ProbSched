type simulation_stats = {
  total_simulation_time : int;
  total_processes_completed : int;
  avg_waiting_time : float;
  avg_turnaround_time : float;
  cpu_utilization : float;
  throughput : float;
  deadline_misses : int;
}

val calculate_statistics :
  Process.t list -> int -> Scheduler.timeline_event list -> simulation_stats
