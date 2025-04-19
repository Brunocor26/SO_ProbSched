(* Ficheiro: scheduler.ml *)
open Process
(* Como 'process.ml' já não tem 'module Process', não precisamos de 'open Process'
   se ambos forem compilados juntos (ex: na mesma biblioteca Dune).
   Os tipos 'process' e 'process_state' estarão diretamente visíveis. *)

(** Tipo para estatísticas individuais (pode ser movido para stats.ml se preferir) *)
type stats = {
  waiting_time : float;
  turnaround_time : float;
}

(** Tipo para o resultado retornado por cada algoritmo de escalonamento *)
type result = {
  schedule : (float * int) list;           (** Histórico: (tempo_inicio_slice, pid) *)
  completed_processes : (process * float) list; (** Lista: (processo_concluido, tempo_de_conclusao) *)
}

(** Função auxiliar de ordenação *)
let sort_by f lst = List.sort (fun a b -> compare (f a) (f b)) lst

(* --- Implementações dos Algoritmos --- *)

(** First-Come, First-Served (Não Preemptivo) *)
let fcfs (processes : process list) : result =
  let time = ref 0. in
  let schedule = ref [] in
  let completed = ref [] in
  let sorted = sort_by (fun p -> p.arrival_time) processes in
  List.iter (fun p ->
    if !time < p.arrival_time then time := p.arrival_time;
    schedule := List.rev ((!time, p.pid) :: (List.rev !schedule)); (* Correção: usar rev+cons+rev é mais eficiente que @ *)
    let completion = !time +. p.burst_time in
    time := completion;
    p.completion_time <- Some completion; (* Atualiza campo mutável *)
    (* Não precisa mais setar remaining_time = 0.0 aqui *)
    completed := (p, completion) :: !completed;
  ) sorted;
  { schedule = !schedule; completed_processes = List.rev !completed }


(** Shortest Job First (Não Preemptivo)
let sjf (processes : process list) : result =
  (* ... (Implementação completa como vista antes, mas adaptada para: *)
  (* - Usar 'process' type corrigido (com remaining_time mutável, deadline opcional) *)
  (* - Popular 'completed_processes' com (processo, tempo_conclusao) *)
  (* - Atualizar p.completion_time <- Some !time quando terminar *)
  (* - Usar List.rev+cons+rev para 'schedule' se quiser otimizar) *)
  (* ... *)
  failwith "Implementação SJF necessária aqui" (* Placeholder *)


(** Priority Scheduling (Não Preemptivo) *)
let priority_non_preemptive (processes : process list) : result =
  (* ... (Implementação completa como vista antes, adaptada como SJF) ... *)
  failwith "Implementação Priority NP necessária aqui" (* Placeholder *)


(** Priority Scheduling (Preemptivo) *)
let priority_preemptive (processes : process list) : result =
  (* ... (Implementação completa como vista antes, adaptada para: *)
  (* - Usar 'process' type corrigido *)
  (* - Decrementar p.remaining_time <- p.remaining_time -. 1.0 *)
  (* - Ao completar (remaining_time <= epsilon): *)
  (* - Atualizar p.completion_time <- Some !time *)
  (* - Adicionar (p, !time) à lista 'completed' *)
  (* ... *)
  failwith "Implementação Priority P necessária aqui" (* Placeholder *)


(** Round Robin (Preemptivo) *)
let round_robin (processes : process list) ~(quantum:float) : result =
  (* ... (Implementação completa como vista antes, adaptada para: *)
  (* - Usar 'process' type corrigido *)
  (* - Decrementar p.remaining_time <- p.remaining_time -. run_time *)
  (* - Ao completar (remaining_time <= epsilon): *)
  (* - Atualizar p.completion_time <- Some !time *)
  (* - Adicionar (p, !time) à lista 'completed' (ou tabela hash e depois converter) *)
  (* ... *)
   failwith "Implementação Round Robin necessária aqui" (* Placeholder *)


(** Rate Monotonic (Preemptivo) *)
let rate_monotonic (processes : process list) : result =
  (* ... (Implementação completa como vista antes, adaptada para: *)
  (* - Usar 'process' type corrigido (depende de 'deadline') *)
  (* - Filtrar e ordenar por 'deadline' (assumido como período) *)
  (* - Atribuir prioridades *)
  (* - Chamar 'priority_preemptive' (que deve estar adaptada) *)
  (* ... *)
   failwith "Implementação Rate Monotonic necessária aqui" (* Placeholder *)


(** Earliest Deadline First (Preemptivo) *)
let edf (processes : process list) : result =
  (* ... (Implementação completa como vista antes, adaptada para: *)
  (* - Usar 'process' type corrigido (depende de 'deadline') *)
  (* - Ordenar 'ready' por 'deadline' *)
  (* - Decrementar p.remaining_time <- p.remaining_time -. 1.0 *)
  (* - Ao completar (remaining_time <= epsilon): *)
  (* - Atualizar p.completion_time <- Some !time *)
  (* - Adicionar (p, !time) à lista 'completed' *)
  (* ... *)
   failwith "Implementação EDF necessária aqui" (* Placeholder *) *)