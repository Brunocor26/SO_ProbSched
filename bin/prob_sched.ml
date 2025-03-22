(* Importar os módulos *)
open Prob_sched
open Generator
open Scheduler
open Stats

(* Função principal *)
let main () =
  (* Parâmetros de simulação *)
  let num_processes = 10 in  (* Número de processos a serem gerados *)
  let lambda = 2.0 in        (* Parâmetro lambda para a distribuição de Poisson *)

  (* Gera processos dinamicamente *)
  let processes = Generator.generate_processes num_processes lambda in

  (* Executa o algoritmo FCFS *)
  let fcfs_result = Scheduler.fcfs_scheduler processes in
  Printf.printf "=== FCFS Execution Order ===\n";
  Scheduler.print_execution_order fcfs_result;
  let fcfs_avg_waiting_time = Stats.calculate_average_waiting_time fcfs_result in
  Printf.printf "Average Waiting Time (FCFS): %.2f\n" fcfs_avg_waiting_time;

  (* Executa o algoritmo SJF *)
  let sjf_result = Scheduler.sjf_scheduler processes in
  Printf.printf "\n=== SJF Execution Order ===\n";
  Scheduler.print_execution_order sjf_result;
  let sjf_avg_waiting_time = Stats.calculate_average_waiting_time sjf_result in
  Printf.printf "Average Waiting Time (SJF): %.2f\n" sjf_avg_waiting_time;

  (* Aqui você pode descomentar futuramente quando implementar *)
  (*
  let quantum = 2 in
  let rr_result = Scheduler.round_robin_scheduler processes quantum in
  Printf.printf "\n=== Round Robin Execution Order ===\n";
  Scheduler.print_execution_order rr_result;
  let rr_avg_waiting_time = Stats.calculate_average_waiting_time rr_result in
  Printf.printf "Average Waiting Time (Round Robin): %.2f\n" rr_avg_waiting_time;

  let priority_result = Scheduler.priority_scheduler_preemptive processes in
  Printf.printf "\n=== Priority Scheduling (Preemptive) Execution Order ===\n";
  Scheduler.print_execution_order priority_result;
  let priority_avg_waiting_time = Stats.calculate_average_waiting_time priority_result in
  Printf.printf "Average Waiting Time (Priority Scheduling): %.2f\n" priority_avg_waiting_time;
  *)
  ()

(* Ponto de entrada do programa *)
let () = main ()
