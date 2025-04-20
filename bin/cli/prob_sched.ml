open Prob_sched_lib
open Help


let aplicar_algoritmo (lista : Process.t list) (opcao : string) : int option =
  (* Converte a opção para minúsculas para ser case-insensitive *)

  Printf.printf "A tentar aplicar o algoritmo: %s\n" opcao; (* Feedback *)

  match opcao with
  | "fcfs" ->
      let tempo_total = Scheduler.fcfs lista in
      Some tempo_total

  | _ ->
      (* Caso a opção não corresponda a nenhum algoritmo conhecido *)
      Printf.eprintf "Erro: Algoritmo de escalonamento '%s' não reconhecido.\n" opcao;
      Printf.eprintf "Opções válidas (exemplo): fcfs, sjf, priority_np, priority_p, rr, rm, edf\n";
      None (* Retorna None para indicar que o algoritmo não foi aplicado *)



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
          | Some tempo_final ->
              Printf.printf "Simulação '%s' concluída em %d unidades de tempo.\n" algoritmo_string tempo_final;
        
              (* Calcular estatísticas usando a lista e o tempo_final *)
              let stats = Statistics.calculate_statistics processos_extraidos tempo_final in
        
              (* Imprimir as estatísticas... *)
              Printf.printf "Avg WT: %.2f, Avg TT: %.2f, CPU Util: %.2f%%, Throughput: %.4f, Deadline Misses: %d\n"
                stats.avg_waiting_time
                stats.avg_turnaround_time
                stats.cpu_utilization
                stats.throughput
                stats.deadline_misses
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