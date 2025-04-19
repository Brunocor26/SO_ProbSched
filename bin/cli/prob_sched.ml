(* Importar os módulos *)
open Prob_sched
open Process
open Help
open Scheduler

let next_pid = ref 0

let generate_pid () =
  let pid = !next_pid in
  next_pid := !next_pid + 1;
  pid

let () =
  Printf.printf "--- Criar Processo Manualmente ---\n";
  let nome = ler_string "Nome do processo" in
  let arrival_time = float_of_int (ler_int "Tempo de chegada") in
  let burst_time = float_of_int (ler_int "Burst time (tempo de execução)") in
  let priority = ler_int "Prioridade (quanto menor, maior prioridade)" in

  let pid = generate_pid () in
  let processo = create_process pid nome arrival_time burst_time priority Ready in

  Printf.printf "\n--- Processo Criado ---\n";
  Printf.printf "PID: %d\n" processo.pid;
  Printf.printf "Nome: %s\n" processo.name;
  Printf.printf "Arrival: %.1f\n" processo.arrival_time;
  Printf.printf "Burst: %.1f\n" processo.burst_time;
  Printf.printf "Priority: %d\n" processo.priority;
  Printf.printf "Completion: %s\n"
    (match processo.completion_time with Some t -> Printf.sprintf "%.1f" t | None -> "N/A");

  let algoritmo = escolher_algoritmo () in

  let result =
    match algoritmo with
    | "FCFS" -> fcfs [processo]
    (* | "SJF" -> sjf [processo] *)
    (* | "Priority_NP" -> priority_non_preemptive [processo] *)
    (* | "Priority_P" -> priority_preemptive [processo] *)
    (* | "RR" -> round_robin [processo] ~quantum:1.0 *)
    | _ -> failwith "Algoritmo não implementado"
  in

  Printf.printf "\n--- Resultado %s ---\n" algoritmo;
  List.iter (fun (start_time, pid) ->
    Printf.printf "Início: %.1f | PID: %d\n" start_time pid
  ) result.schedule;
  List.iter (fun (proc, completion_time) ->
    Printf.printf "PID: %d | Waiting: %.1f | Turnaround: %.1f | Completion: %.1f\n"
      proc.pid
      (completion_time -. proc.arrival_time -. proc.burst_time)
      (completion_time -. proc.arrival_time)
      completion_time
  ) result.completed_processes;

