open Process
open Scheduler

type simulation_stats = {
  total_simulation_time : int;
  total_processes_completed : int;
  avg_waiting_time : float;
  avg_turnaround_time : float;
  cpu_utilization : float;
  throughput : float;
  deadline_misses : int;
}

(* Helper function to identify if a process is from a real-time algorithm *)
let is_realtime_instance p =
  p.id >= 1000  (* Real-time instances have IDs >= 1000 *)

(* Helper function to get completed processes based on algorithm type *)
let get_completed_processes processes =
  let completed_instances = Scheduler.get_completed_instances () in
  let is_realtime = List.exists is_realtime_instance processes in
  if is_realtime && completed_instances <> [] then
    completed_instances
  else
    List.filter (fun p -> 
      match p.completion_time with 
      | Some _ -> true 
      | None -> false
    ) processes

let calculate_statistics (processos : t list) (tempo_final : int) (log : timeline_event list) : simulation_stats =
  (* Get completed processes based on whether it's a real-time algorithm or not *)
  let completed = get_completed_processes processos in
  let valid_for_stats = List.filter (fun p -> 
    match p.completion_time, p.turnaround_time with
    | Some _, Some _ -> true
    | _ -> false
  ) completed in
  let total_completed = List.length valid_for_stats in
  let total_waiting_time =
    List.fold_left (fun acc p -> acc + p.waiting_time) 0 valid_for_stats
  in
  let total_turnaround_time =
    List.fold_left (fun acc p ->
      match p.turnaround_time with
      | Some tat -> acc + tat
      | None -> acc
    ) 0 valid_for_stats
  in
  let total_time = float_of_int tempo_final in
  let idle_time = List.fold_left (fun acc ev -> if ev.process_id = -1 then acc + 1 else acc) 0 log in
  let cpu_utilization =
    if tempo_final > 0 then (1.0 -. (float_of_int idle_time /. total_time)) *. 100.0 else 0.0
  in
  let avg_waiting_time =
    if total_completed > 0 then (float_of_int total_waiting_time) /. (float_of_int total_completed) else 0.0
  in
  let avg_turnaround_time =
    if total_completed > 0 then (float_of_int total_turnaround_time) /. (float_of_int total_completed) else 0.0
  in
  let throughput =
    if tempo_final > 0 then (float_of_int total_completed) /. (float_of_int tempo_final) else 0.0
  in
  let deadline_misses =
    let missed_set = Hashtbl.create 10 in
    List.iter (fun event ->
      match event.new_state with
      | Process.Terminated -> (
          let pid = event.process_id in
          let instance_id = event.instance_id in
          let find_process =
            match instance_id with
            | Some iid -> 
                if !Scheduler.all_instances <> [] then
                  List.find_opt (fun p -> p.id = iid) !Scheduler.all_instances
                else
                  List.find_opt (fun p -> p.id = pid) processos
            | None -> 
                List.find_opt (fun p -> p.id = pid) processos
          in
          match find_process with
          | Some p -> (
              match p.deadline, p.completion_time with
              | Some deadline, Some ct ->
                  let abs_deadline = 
                    if is_realtime_instance p then
                      match p.deadline with
                      | Some d -> d  (* For real-time instances, deadline is absolute *)
                      | None -> p.arrival_time + deadline
                    else
                      p.arrival_time + deadline
                  in
                  if ct > abs_deadline then
                    let key = match instance_id with
                              | Some iid -> (iid, abs_deadline)
                              | None -> (pid, abs_deadline)
                    in
                    Hashtbl.replace missed_set key true
              | Some deadline, None ->
                  let abs_deadline = 
                    if is_realtime_instance p then
                      match p.deadline with
                      | Some d -> d
                      | None -> p.arrival_time + deadline
                    else 
                      p.arrival_time + deadline
                  in
                  if tempo_final > abs_deadline then
                    let key = match instance_id with
                              | Some iid -> (iid, abs_deadline)
                              | None -> (pid, abs_deadline)
                    in
                    Hashtbl.replace missed_set key true
              | _ -> ()
            )
          | None -> ()
        )
      | _ -> ()
    ) log;
    Hashtbl.length missed_set
  in
  {
    total_simulation_time = tempo_final;
    total_processes_completed = total_completed;
    avg_waiting_time;
    avg_turnaround_time;
    cpu_utilization;
    throughput;
    deadline_misses;
  }
