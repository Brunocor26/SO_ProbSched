open Process

(* Módulo Scheduler - Algoritmos de escalonamento *)
module Scheduler = struct
  open Process

  let print_execution_order processes =
    List.iter (fun p -> Printf.printf "Processo %d executado\n" p.pid) processes

  (* FCFS - First-Come, First-Served *)
  let fcfs_scheduler processes =
    List.sort (fun p1 p2 -> compare p1.arrival_time p2.arrival_time) processes

  (* SJF - Shortest Job First *)
  let sjf_scheduler processes =
    List.sort (fun p1 p2 -> compare p1.burst_time p2.burst_time) processes
end
