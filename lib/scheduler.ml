(* Dentro de Scheduler.ml *)

(* Tipo para representar um evento da timeline *)
type timeline_event = {
  time : int;
  process_id : int; (* Usar -1 para CPU Ociosa, por exemplo *)
  new_state : Process.process_state; (* Running, Terminated, Ready, Waiting *)
}

(* FCFS Adaptado para retornar log *)
let fcfs (processes : Process.t list) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let sorted_procs = List.sort (fun p1 p2 -> compare p1.Process.arrival_time p2.Process.arrival_time) processes in

  (* Função auxiliar para adicionar evento ao log *)
  let log_event t pid state =
    schedule_log := { time = t; process_id = pid; new_state = state } :: !schedule_log
  in

  List.iter (fun p ->
    (* Tempo Ocioso *)
    if !time < p.Process.arrival_time then begin
      log_event !time (-1) Process.Waiting; (* Marca início do idle *)
      time := p.Process.arrival_time;
      log_event !time (-1) Process.Ready; (* Marca fim do idle, -1 pode não ser o melhor aqui *)
                                        (* Talvez logar início/fim de execução seja melhor *)
    end;

    (* Início da Execução *)
    log_event !time p.Process.id Process.Running;

    (* Atualiza estado e tempos do processo p *)
    p.Process.state <- Process.Running; (* Já definido no log *)
    let wait = !time - p.Process.arrival_time in
    let completion = !time + p.Process.burst_time in
    let turnaround = completion - p.Process.arrival_time in

    p.Process.waiting_time <- wait;
    p.Process.completion_time <- Some completion;
    p.Process.turnaround_time <- Some turnaround;
    p.Process.remaining_burst_time <- 0;

    (* Fim da Execução *)
    time := completion;
    log_event !time p.Process.id Process.Terminated;
    p.Process.state <- Process.Terminated; (* Já definido no log *)

  ) sorted_procs;

  (!time, List.rev !schedule_log) (* Retorna tempo final e o log revertido (ordem cronológica) *)

(* NOTA: Para algoritmos preemptivos, terias de logar quando um processo passa
   de Running para Ready (preempção) e quando volta a Running. *)