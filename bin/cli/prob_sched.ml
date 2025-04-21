open Prob_sched_lib
open Help

(* --- Função de Aplicação do Algoritmo --- *)

(* Aplica o algoritmo escolhido e devolve o tempo final e o log, ou None *)
let aplicar_algoritmo (lista : Process.t list) (opcao : string) : (int * Scheduler.timeline_event list) option =
  Printf.printf "A aplicar algoritmo: %s\n" opcao;
  try (* try-with para capturar erros nos schedulers, se necessário *)
    match opcao with
    | "fcfs" -> Some (Scheduler.fcfs lista)
    | "sjf" -> Some (Scheduler.sjf lista) 
    | "priority_np" -> Some (Scheduler.priority_non_preemptive lista)
    | "priority_preemp" -> Some (Scheduler.priority_preemptive lista)
    | "rr" ->
        let quantum = read_int "Inserir time quantum:\n" in (* pedir ao user o time quantum desejado *)
        Printf.printf "  (Quantum = %d para Round Robin)\n" quantum;
        Some (Scheduler.round_robin ~quantum lista) (*chamar o algoritmo*)
    (*| "rm" -> Some (Scheduler.rate_monotonic lista) (*chamar o algoritmo*)
    | "edf" -> Some (Scheduler.edf lista) (*chamar o algoritmo*)*)
    | _ ->
        Printf.eprintf "Erro: Algoritmo '%s' não reconhecido.\n" opcao; (* nao deve ocorrer *)
        None
  with
  | ex -> (* Captura exceções que possam ocorrer dentro dos algoritmos *)
      Printf.eprintf "Erro durante a execução do algoritmo '%s': %s\n" opcao (Printexc.to_string ex);
      None

(* --- Função de Impressão da Timeline --- *)

(* Imprime a timeline passo a passo *)
let imprimir_timeline (log : Scheduler.timeline_event list) (tempo_final : int) : unit =
  print_endline "\n--- Timeline ---";
  flush Stdlib.stdout;

  if tempo_final <= 0 then begin
    print_endline "[Simulação sem duração ou vazia]"; flush Stdlib.stdout; ()
  end else
    let sorted_log = List.sort (fun e1 e2 -> compare e1.Scheduler.time e2.Scheduler.time) log in
    let log_ref = ref sorted_log in (* Usar referência para consumir a lista *)
    let running_pid = ref (-1) in

    (* Itera por cada unidade de tempo *)
    for t = 0 to tempo_final - 1 do
      (* Processa todos os eventos que ocorrem exatamente no instante 't' *)
      let rec process_events_at_t current_t =
        match !log_ref with
        | event :: rest when event.Scheduler.time = current_t ->
            (* Atualiza o PID em execução com base no evento *)
            (match event.Scheduler.new_state with
             | Process.Running -> running_pid := event.Scheduler.process_id
             | Process.Terminated when event.Scheduler.process_id = !running_pid -> running_pid := -1
             | Process.Ready when event.Scheduler.process_id = !running_pid -> running_pid := -1 (* Preempção *)
             | Process.Waiting when event.Scheduler.process_id = -1 -> running_pid := -1 (* Idle *)
             | _ -> () (* Outros estados não afetam quem está a correr agora *)
            );
            log_ref := rest; (* Consome o evento da lista *)
            process_events_at_t current_t (* Chama recursivamente para processar outros eventos no mesmo 't' *)
        | _ -> () (* Sem mais eventos neste instante ou lista vazia *)
      in
      process_events_at_t t;

      (* Imprime o símbolo para o intervalo [t, t+1) *)
      let symbol = if !running_pid = -1 then "[IDLE]" else Printf.sprintf "[ P%d ]" !running_pid in
      Printf.printf "%s" symbol;
      flush Stdlib.stdout;
      
    done;

    print_endline "\n-----------------------------------";
    flush Stdlib.stdout

(* --- Funções Auxiliares para Main --- *)

(* Pergunta ao utilizador e devolve o nome do algoritmo escolhido *)
let prompt_algoritmo () : string =
  let menu = "1->FCFS\n2->SJF (NP)\n3->Priority (NP)\n4->Priority (P)\n5->Round Robin\n6->Rate Monotonic (RT)\n7->EDF (RT)\nEscolha o algoritmo (1-7):\n" in
  let escolha_num = read_int_range menu 1 7 in (* Corrigido para 7 opções *)
  (* Assume que Help.escolha_algoritmo mapeia o número para a string correta *)
  Help.escolha_algoritmo escolha_num

(* Obtém a lista de processos, seja de ficheiro ou gerada *)
let obter_lista_processos (algoritmo : string) : Process.t list option =
  let is_realtime = match String.lowercase_ascii algoritmo with "rm" | "edf" -> true | _ -> false in
  let origem_processos = read_int_range "Obter processos de:\n1-> Ficheiro\n2-> Gerar (Não implementado)\nEscolha (1-2): " 1 2 in

  match origem_processos with
  | 1 -> (* Ler de ficheiro *)
      let nome_ficheiro = read_non_empty_string "Nome do ficheiro de processos: " in
      Printf.printf "A ler ficheiro '%s'...\n" nome_ficheiro;
      (try (* Tenta ler e criar os processos *)
         if is_realtime then
           let dados_rt = Help.ler_ficheiro_dados_processos_rt nome_ficheiro in
           if dados_rt = [] && Sys.file_exists nome_ficheiro then Printf.eprintf "Aviso: Ficheiro lido, mas nenhuma tarefa RT válida encontrada.\n";
           Some (List.map (fun (id, inicio, burst, periodo) ->
                     Process.create ~id ~arrival_time:inicio ~burst_time:burst
                       ~priority:periodo (* Exemplo: Usar período como prioridade base para RM, ignorado por EDF? Ou -1? *)
                       ?period:(Some periodo)
                       ?deadline:(Some (inicio + periodo)) (* Exemplo: Deadline relativo = período *)
                       ()
                 ) dados_rt)
         else
           let dados = Help.ler_ficheiro_dados_processos nome_ficheiro in
            if dados = [] && Sys.file_exists nome_ficheiro then Printf.eprintf "Aviso: Ficheiro lido, mas nenhuma tarefa válida encontrada.\n";
           Some (List.map (fun (id, inicio, burst, prioridade) ->
                     Process.create ~id ~arrival_time:inicio ~burst_time:burst ~priority:prioridade ()
                 ) dados)
       with
       | Sys_error msg -> Printf.eprintf "Erro ao ler ficheiro '%s': %s\n" nome_ficheiro msg; None
       | End_of_file -> Printf.eprintf "Erro: Fim de ficheiro inesperado ao ler '%s'.\n" nome_ficheiro; None
       | Failure msg -> Printf.eprintf "Erro ao processar dados do ficheiro '%s': %s\n" nome_ficheiro msg; None
       | ex -> Printf.eprintf "Erro desconhecido ao obter processos de '%s': %s\n" nome_ficheiro (Printexc.to_string ex); None
      )

  | 2 -> (* Gerar processos *)
      Printf.eprintf "Geração de processos não implementada.\n";
      None (* devolve None porque não foi implementado *)

  | _ -> (* Nunca deve acontecer com read_int_range *)
       None


(* Mostra os resultados finais: estatísticas e timeline *)
let mostrar_resultados (lista_processos_final : Process.t list) (tempo_final : int) (log_eventos : Scheduler.timeline_event list) : unit =
  (* 1. Calcular e Imprimir Estatísticas *)
  Printf.printf "\n--- Estatísticas Finais ---\n";
  if tempo_final > 0 && List.length lista_processos_final > 0 then
    try
      let stats = Statistics.calculate_statistics lista_processos_final tempo_final in
      Printf.printf "Tempo Total Simulação: %d\n" stats.total_simulation_time;
      Printf.printf "Processos Completados: %d\n" stats.total_processes_completed;
      Printf.printf "Avg WT: %.2f\n" stats.avg_waiting_time;
      Printf.printf "Avg TT: %.2f\n" stats.avg_turnaround_time;
      Printf.printf "CPU Util: %.2f%%\n" stats.cpu_utilization;
      Printf.printf "Throughput: %.4f proc/ut\n" stats.throughput;
      Printf.printf "Deadline Misses: %d\n" stats.deadline_misses;
    with ex -> Printf.eprintf "Erro ao calcular estatísticas: %s\n" (Printexc.to_string ex)
  else
    Printf.printf "Não é possível calcular estatísticas (tempo final = %d, processos = %d).\n" tempo_final (List.length lista_processos_final);

  (* 2. Imprimir Timeline *)
  imprimir_timeline log_eventos tempo_final


(* --- Função Principal Refatorada --- *)

let main () =
  (* 1. Obter escolha do algoritmo *)
  let algoritmo_escolhido = prompt_algoritmo () in
  Printf.printf "Algoritmo escolhido: %s\n" algoritmo_escolhido;

  (* 2. Obter lista de processos *)
  match obter_lista_processos algoritmo_escolhido with
  | None ->
      Printf.eprintf "Falha ao obter lista de processos. A terminar.\n"
  | Some processos_iniciais ->
      if processos_iniciais = [] then
         Printf.eprintf "Lista de processos está vazia. Nada para simular.\n"
      else begin
          Printf.printf "%d processos obtidos com sucesso.\n" (List.length processos_iniciais);
          (* NOTA: aplicar_algoritmo modifica a lista devido aos campos mutáveis *)
          (* Não precisamos de cópia explícita se aceitarmos esta modificação. *)

          (* 3. Executar simulação *)
          match aplicar_algoritmo processos_iniciais algoritmo_escolhido with
          | None ->
              Printf.eprintf "Falha ao executar a simulação para '%s'.\n" algoritmo_escolhido
          | Some (tempo_final, log_eventos) ->
              Printf.printf "Simulação '%s' concluída.\n" algoritmo_escolhido;

              (* 4. Mostrar Resultados *)
              mostrar_resultados processos_iniciais tempo_final log_eventos
          end

(* Ponto de entrada do programa *)
let () = main ()