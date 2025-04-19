(* Ficheiro: stats.ml *)

(* Não precisa de 'open Process' se os tipos relevantes estiverem em process_run_data *)

(** Dados necessários para cada processo concluído *)
type process_run_data = {
  id : int;
  arrival_time : float;
  burst_time : float;       (* Burst original total *)
  completion_time : float;  (* Tempo de conclusão (obtido do scheduler.result) *)
  deadline : float option;  (* Deadline absoluto, se houver *)
}

(** Estrutura para os resultados finais *)
type simulation_results = {
  total_processes_completed : int;
  avg_waiting_time : float;
  avg_turnaround_time : float;
  cpu_utilization : float;         (* Percentagem 0.0-100.0 *)
  throughput : float;              (* Processos / unidade de tempo *)
  total_deadline_processes : int;  (* Processos que tinham deadline *)
  deadline_misses : int;           (* Quantos falharam o deadline *)
}

(** Valor zero/inicial *)
let zero : simulation_results = {
  total_processes_completed = 0; avg_waiting_time = 0.0; avg_turnaround_time = 0.0;
  cpu_utilization = 0.0; throughput = 0.0; total_deadline_processes = 0; deadline_misses = 0;
}

(** Divisão segura *)
let safe_division num den = if den = 0.0 || den = 0. then 0.0 else num /. den

(** Função principal de cálculo *)
let calculate_statistics
    ~(run_data: process_run_data list) (* Renomeado para clareza *)
    ~total_simulation_time
    ~total_cpu_busy_time
    =
  let n_completed = List.length run_data in
  if n_completed = 0 || total_simulation_time <= 0.0 then zero
  else
    let total_waiting = ref 0.0 in
    let total_turnaround = ref 0.0 in
    let deadline_processes = ref 0 in
    let misses = ref 0 in

    List.iter (fun data ->
      let turnaround = data.completion_time -. data.arrival_time in
      let waiting = turnaround -. data.burst_time in
      (* Adicionar verificações de sanidade se quiser (waiting/turnaround >= 0) *)
      total_turnaround := !total_turnaround +. max 0.0 turnaround; (* Evita acumular negativos se houver imprecisão *)
      total_waiting := !total_waiting +. max 0.0 waiting;       (* Evita acumular negativos *)

      match data.deadline with
      | Some dl ->
          deadline_processes := !deadline_processes + 1;
          (* Usar tolerância pequena para comparação float *)
          if data.completion_time > dl +. 1e-9 then misses := !misses + 1
      | None -> ()
    ) run_data;

    let avg_waiting = safe_division !total_waiting (float_of_int n_completed) in
    let avg_turnaround = safe_division !total_turnaround (float_of_int n_completed) in
    let cpu_util = safe_division (total_cpu_busy_time *. 100.0) total_simulation_time in
    let throughput = safe_division (float_of_int n_completed) total_simulation_time in

    { total_processes_completed = n_completed; avg_waiting_time = avg_waiting;
      avg_turnaround_time = avg_turnaround; cpu_utilization = min 100.0 (max 0.0 cpu_util); (* Garante 0-100% *)
      throughput = throughput; total_deadline_processes = !deadline_processes;
      deadline_misses = !misses;
    }

(** Função para formatar os resultados *)
let format_results results =
   let deadline_miss_percentage =
     safe_division (float_of_int results.deadline_misses *. 100.0) (float_of_int results.total_deadline_processes)
   in
   let deadline_str =
     if results.total_deadline_processes > 0 then
       Printf.sprintf "Deadline Misses:            %d / %d (%.2f%%)"
         results.deadline_misses results.total_deadline_processes deadline_miss_percentage
     else
       "Deadline Misses:            N/A (No deadlines defined)"
   in
   Printf.sprintf
     "--- Simulation Results ---\n\
      Total Processes Completed:  %d\n\
      Average Waiting Time:     %.4f\n\
      Average Turnaround Time:  %.4f\n\
      CPU Utilization:          %.2f%%\n\
      Throughput:               %.4f processes/unit time\n\
      %s\n\
      --------------------------"
     results.total_processes_completed
     results.avg_waiting_time
     results.avg_turnaround_time
     results.cpu_utilization
     results.throughput
     deadline_str