(* Colocar num ficheiro stats.ml ou similar *)

(* Estatísticas calculadas após a simulação *)
type simulation_stats = {
  avg_waiting_time : float;
  avg_turnaround_time : float;
  cpu_utilization : float; (* Percentagem 0.0-100.0 *)
  throughput : float; (* Processos por unidade de tempo *)
  total_simulation_time : int;
  total_processes_completed : int;  
  deadline_misses : int; (* Número de processos que falharam o deadline *)
}

(* Função para calcular estatísticas (a implementar depois) *)
let calculate_statistics (completed_processes : Process.t list) (total_time : int) : simulation_stats =
  let num_processes = List.length completed_processes in
  if num_processes = 0 || total_time = 0 then
    (* Evitar divisão por zero e retornar stats vazias/default *)
    {
      avg_waiting_time = 0.0;
      avg_turnaround_time = 0.0;
      cpu_utilization = 0.0;
      throughput = 0.0;
      total_simulation_time = total_time;
      total_processes_completed = 0;
      deadline_misses = 0;
    }
  else
    let total_waiting = ref 0 in
    let total_turnaround = ref 0 in
    let total_burst = ref 0 in
    let deadline_misses_count = ref 0 in

    List.iter (fun p ->
      (* Assumimos que waiting_time foi calculado e guardado no processo pelo escalonador *)
      total_waiting := !total_waiting + p.Process.waiting_time;

      (* Calculamos turnaround a partir do completion_time e arrival_time *)
      let tt = match p.Process.completion_time with
              | None -> 0 (* Processo não completou? Deveria ter completado nesta lista *)
              | Some ct -> ct - p.Process.arrival_time
      in
      total_turnaround := !total_turnaround + tt;
      p.Process.turnaround_time <- Some tt; (* Atualiza o campo no processo *)

      total_burst := !total_burst + p.Process.burst_time; (* Soma dos tempos de burst originais *)

      (* Verifica deadline misses *)
      match p.Process.deadline, p.Process.completion_time with
      | Some dl, Some ct when ct > (p.Process.arrival_time + dl) -> (* Assumindo deadline relativo à chegada *)
          incr deadline_misses_count
      | _ -> ()
    ) completed_processes;

    let n = float_of_int num_processes in
    let tot_time = float_of_int total_time in
    let busy_time = float_of_int !total_burst in

    {
      avg_waiting_time = float_of_int !total_waiting /. n;
      avg_turnaround_time = float_of_int !total_turnaround /. n;
      cpu_utilization = (busy_time /. tot_time) *. 100.0;
      throughput = n /. tot_time;
      total_simulation_time = total_time;
      total_processes_completed = num_processes;
      deadline_misses = !deadline_misses_count;
    }