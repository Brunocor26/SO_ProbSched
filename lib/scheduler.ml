(* Colocar num ficheiro scheduler.ml ou similar *)

(* Helper para ordenar *)
let sort_by_field f lst = List.sort (fun a b -> compare (f a) (f b)) lst

(* --- FCFS Adaptado --- *)
let fcfs (processes : Process.t list) : int = (* Retorna o tempo total *)
  let time = ref 0 in (* Usa int *)
  let completed_processes = ref [] in
  (* Ordena a cópia da lista por tempo de chegada *)
  let sorted_procs = sort_by_field (fun p -> p.Process.arrival_time) processes in

  List.iter (fun p ->
    (* Avança o tempo se CPU estiver ociosa *)
    if !time < p.Process.arrival_time then
      time := p.Process.arrival_time;

    (* Atualiza estado e tempos do processo p *)
    p.Process.state <- Process.Running; (* Simplificação: Assume que corre até ao fim *)
    let wait = !time - p.Process.arrival_time in
    let completion = !time + p.Process.burst_time in
    let turnaround = completion - p.Process.arrival_time in

    p.Process.waiting_time <- wait;
    p.Process.completion_time <- Some completion;
    p.Process.turnaround_time <- Some turnaround;
    p.Process.remaining_burst_time <- 0; (* Terminou *)
    p.Process.state <- Process.Terminated;

    (* Avança o tempo de simulação *)
    time := completion;
    completed_processes := p :: !completed_processes;
  ) sorted_procs;

  !time (* Retorna o tempo de conclusão do último processo *)