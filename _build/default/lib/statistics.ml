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

let calculate_statistics (processos : t list) (tempo_final : int) (log : timeline_event list) : simulation_stats =
  let total_completed = List.length processos in

  let total_waiting_time =
    List.fold_left (fun acc p -> acc + p.waiting_time) 0 processos
  in
  let total_turnaround_time =
    List.fold_left (fun acc p ->
      match p.turnaround_time with
      | Some tat -> acc + tat
      | None -> acc
    ) 0 processos
  in

  (* Utilização do CPU: percentagem de tempo em que process_id <> -1 *)
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

  (* Deadlines falhadas: conta processos terminados depois do deadline *)
  let deadline_misses =
    let missed_set = Hashtbl.create 10 in
    List.iter (fun event ->
      match event.new_state with
      | Process.Terminated -> (
          let pid = event.process_id in
          match List.find_opt (fun p -> p.id = pid) processos with
          | Some p -> (
              match p.deadline, p.completion_time with
              | Some deadline, Some ct ->
                  let abs_deadline = p.arrival_time + deadline in
                  if ct > abs_deadline then
                    Hashtbl.replace missed_set (pid, abs_deadline) true
              | Some deadline, None ->
                  let abs_deadline = p.arrival_time + deadline in
                  if tempo_final > abs_deadline then
                    Hashtbl.replace missed_set (pid, abs_deadline) true
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
