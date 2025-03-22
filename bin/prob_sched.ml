(* Import necessary modules *)
open Prob_sched
open Generator
open Scheduler
open Stats

(* main function *)
let main () =
  (* Simulation parameters *)
  let num_processes = 10 in  (* number of processes being generated *)
  let lambda = 2.0 in        (* lambda for Poisson distribution *)

  (* generates processes dinamically *)
  let processes = Generator.generate_processes num_processes lambda in

  (* Executes the FCFS algorithm *)
  let fcfs_result = Scheduler.fcfs_scheduler processes in
  Printf.printf "=== FCFS Execution Order ===\n";
  Scheduler.print_execution_order fcfs_result;
  let fcfs_avg_waiting_time = Stats.calculate_average_waiting_time fcfs_result in
  Printf.printf "Average Waiting Time (FCFS): %.2f\n" fcfs_avg_waiting_time;

  (* Executes the SJF algorithm *)
  let sjf_result = Scheduler.sjf_scheduler processes in
  Printf.printf "\n=== SJF Execution Order ===\n";
  Scheduler.print_execution_order sjf_result;
  let sjf_avg_waiting_time = Stats.calculate_average_waiting_time sjf_result in
  Printf.printf "Average Waiting Time (SJF): %.2f\n" sjf_avg_waiting_time;

(* Ponto de entrada do programa *)
let () = main ()
