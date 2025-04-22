type simulation_stats = {
  avg_waiting_time : float;
  avg_turnaround_time : float;
  cpu_utilization : float;
  throughput : float;
  total_simulation_time : int;
  total_processes_completed : int;
  deadline_misses : int;
}
val calculate_statistics : Process.t list -> int -> simulation_stats
