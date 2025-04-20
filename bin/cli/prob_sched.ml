open Prob_sched_lib
open Help


(* Adaptar aplicar_algoritmo para retornar também o log *)
let aplicar_algoritmo (lista : Process.t list) (opcao : string) : (int * Scheduler.timeline_event list) option =
  let opcao_lower = String.lowercase_ascii opcao in
  Printf.printf "A tentar aplicar o algoritmo: %s\n" opcao_lower;
  match opcao_lower with
  | "fcfs" ->
      let tempo_total, log = Scheduler.fcfs lista in
      Some (tempo_total, log)
  (* ... outros casos, chamando as versões adaptadas dos schedulers ... *)
  | _ ->
      Printf.eprintf "Erro: Algoritmo '%s' não reconhecido ou não adaptado para timeline.\n" opcao;
      None


(* Função para imprimir a timeline (exemplo simples) *)
let imprimir_timeline_simples (log : Scheduler.timeline_event list) (tempo_final : int) : unit =
  print_endline "\n--- Timeline (Gráfico de Gantt Texto Simples) ---";
  if log = [] then begin
    print_endline "[Vazio]";
    flush stdout;
    ()
  end else
    (* Ordenar por tempo, caso não esteja garantido *)
    let sorted_log = List.sort (fun e1 e2 -> compare e1.Scheduler.time e2.Scheduler.time) log in

    (* Itera pelos eventos para construir a representação *)
    let last_time = ref 0 in
    let current_pid = ref (-1) in (* Começa Ocioso *)

    List.iter (fun event ->
      let time_interval = event.Scheduler.time - !last_time in
      if time_interval > 0 then (
        (* Imprime o que estava a correr no intervalo anterior *)
        let symbol = if !current_pid = -1 then "[IDLE]" else Printf.sprintf "[ P%d ]" !current_pid in
        for _ = 1 to time_interval do
          Printf.printf "%s" symbol;
        done;
        flush stdout; (* Garante a impressão imediata *)
      );

      (* Atualiza o estado com base no evento atual *)
      (* Esta lógica simplista assume que Running é o único estado ativo *)
      (* Uma lógica mais robusta consideraria o estado exato do evento *)
       (match event.Scheduler.new_state with
         | Process.Running -> current_pid := event.Scheduler.process_id
         | Process.Terminated when event.Scheduler.process_id = !current_pid -> current_pid := -1 (* Fica Idle *)
         | Process.Ready when event.Scheduler.process_id = !current_pid -> current_pid := -1 (* Preempted, fica Idle temporariamente até próximo evento *)
         | Process.Waiting when event.Scheduler.process_id = -1 -> current_pid := -1 (* Mantém Idle *)
         | _ -> () (* Outros estados não mudam quem está a correr imediatamente *)
        );

      last_time := event.Scheduler.time;

    ) sorted_log;

     (* Imprime o último estado até ao tempo final, se necessário *)
     let time_interval = tempo_final - !last_time in
      if time_interval > 0 then (
        let symbol = if !current_pid = -1 then "[IDLE]" else Printf.sprintf "[ P%d ]" !current_pid in
        for _ = 1 to time_interval do
          Printf.printf "%s" symbol;
        done;
          flush stdout;
      );

  print_endline "\n-------------------------------------------------";
  flush stdout


let main () =
  let algoritmos_disp : string = "1->FCFS\n2->SJ\n3->Priority Scheduling\n4->Round Robin\n5->Rate Monotonic (Tempo Real)\n6->EDF (Tempo Real)\nEscolha o algoritmo a ser usado:\n" in
  let opcao_alg : int = read_int_range algoritmos_disp 1 6 in (* Adjusted range to include 1-6 *)
  
  let algoritmo_string : string = escolha_algoritmo opcao_alg in

  (* pedidos adicionais de certos algoritmos *)
  match algoritmo_string with
  | "fcfs" | "sj" | "priority" -> (* Non-realtime algorithms *)
      let origem_processos : int = read_int_range "Processos através de:\n1->Lista estática\n2->Gerados\n" 1 2 in
      (match origem_processos with
      | 1 -> 
          let nome_ficheiro : string = read_non_empty_string "Inserir nome do ficheiro a ser lido:\n" in
          let atributos_processos : (int * int * int * int) list = ler_ficheiro_dados_processos nome_ficheiro in
          let processos_extraidos : Process.t list =
            List.map (fun (id, inicio, burst, prioridade) ->
              Process.create
                ~id:id
                ~arrival_time:inicio
                ~burst_time:burst
                ~priority:prioridade
                ()
            ) atributos_processos
          in
          print_endline "Lista de processos lida com sucesso";

          (match aplicar_algoritmo processos_extraidos algoritmo_string with
          | Some (tempo_final, log_eventos) ->
              Printf.printf "Simulação '%s' concluída em %d unidades de tempo.\n" algoritmo_string tempo_final;
        
              (* Calcular estatísticas usando a lista e o tempo_final *)
              let stats = Statistics.calculate_statistics processos_extraidos tempo_final in
        
              (* Imprimir as estatísticas... *)
              Printf.printf "Avg WT: %.2f, Avg TT: %.2f, CPU Util: %.2f%%, Throughput: %.4f, Deadline Misses: %d\n"
                stats.avg_waiting_time
                stats.avg_turnaround_time
                stats.cpu_utilization
                stats.throughput
                stats.deadline_misses;
                
              (* Imprimir a timeline *)
              imprimir_timeline_simples log_eventos tempo_final

          | None ->
              Printf.eprintf "Não foi possível executar a simulação para o algoritmo '%s'.\n" algoritmo_string
          )
      | 2 -> print_endline "Por implementar\n"
      | _ -> (* This should never happen due to read_int_range, but compiler needs it *)
          Printf.eprintf "Valor inválido para origem dos processos: %d\n" origem_processos
      )
  
  | "rr" -> print_endline "Por implementar\n"
  
  | "rm" | "edf" -> (* Realtime algorithms *)
      let origem_processos : int = read_int_range "Processos através de:\n1->Lista estática\n2->Gerados\n" 1 2 in
      (match origem_processos with
      | 1 -> print_endline "Depois meter aqui a funcao do read_file.ml\n"
      | 2 -> print_endline "Por implementar\n"
      | _ -> (* This should never happen due to read_int_range, but compiler needs it *)
          Printf.eprintf "Valor inválido para origem dos processos: %d\n" origem_processos
      )

  
  | _ -> print_endline "Algoritmo não reconhecido ou não implementado"

let () = main ()