(* bin/main.ml *)
open Prob_sched_lib
open Yojson.Basic

(* --- Referências para guardar os argumentos da linha de comando --- *)
let algo_ref = ref ""
let file_ref = ref ""
let quantum_ref = ref (None : int option)
let max_time_ref = ref (None : int option)
let num_ref = ref None
let human_ref = ref false  (* Indica se o output deve ser legível para humanos *)

(* --- Especificação dos Argumentos --- *)
let usage_msg =
  "Usage: " ^ Sys.argv.(0) ^
  " --algo <name> [--file <path> | --gen <num>] [--quantum <int>] [--max <int>] [--human]\n" ^
  "  --file <path>   : Caminho para o ficheiro CSV de processos\n" ^
  "  --gen <num>     : Gerar <num> processos aleatórios (alternativa a --file)\n" ^
  "  --algo <name>   : Algoritmo (fcfs, sjf, priority_np, priority_preemp, rr, rm, edf)\n" ^
  "  --quantum <int> : Quantum para RR\n" ^
  "  --max <int>     : Tempo máximo de simulação (obrigatório para rm/edf)\n" ^
  "  --human         : Output legível para humanos\n"

let speclist = [
  ("--algo", Arg.Set_string algo_ref, " Algoritmo de escalonamento (fcfs, sjf, priority_np, priority_preemp, rr, rm, edf)");
  ("--file", Arg.Set_string file_ref, " Caminho para o ficheiro de processos (alternativa a --gen)");
  ("--gen", Arg.Int (fun n -> num_ref := Some n), "Número de processos a gerar aleatoriamente (alternativa a --file)");
  ("--quantum", Arg.Int (fun q -> quantum_ref := Some q), " Quantum para Round Robin (obrigatório se --algo rr)");
  ("--max", Arg.Int (fun m -> max_time_ref := Some m), " Tempo máximo de simulação (obrigatório para rm/edf)");
  ("--human", Arg.Set human_ref, " Output legível para humanos");
]

(* --- Função de Saída JSON --- *)
let stats_to_json (stats : Statistics.simulation_stats) : t =
  `Assoc [
    ("total_simulation_time", `Int stats.total_simulation_time);
    ("total_processes_completed", `Int stats.total_processes_completed);
    ("avg_waiting_time", `Float stats.avg_waiting_time);
    ("avg_turnaround_time", `Float stats.avg_turnaround_time);
    ("cpu_utilization", `Float stats.cpu_utilization);
    ("throughput", `Float stats.throughput);
    ("deadline_misses", `Int stats.deadline_misses);
  ]

(* --- Função para formatar a timeline da simulação --- *)
let format_timeline_string (log : Scheduler.timeline_event list) (tempo_final : int) : string =
  if tempo_final <= 0 || log = [] then "[No simulation trace]"
  else
    let buffer = Buffer.create (tempo_final * 6) in
    let sorted_log = List.sort (fun e1 e2 -> compare e1.Scheduler.time e2.Scheduler.time) log in
    let log_ref = ref sorted_log in
    let running_pid = ref (-1) in
    
    for t = 0 to tempo_final - 1 do
      let rec process_events_at_t current_t =
        match !log_ref with
        | event :: rest when event.Scheduler.time = current_t ->
            (match event.Scheduler.new_state with
             | Process.Running -> 
                 running_pid := event.Scheduler.process_id
             | Process.Terminated when event.Scheduler.process_id = !running_pid -> 
                 running_pid := -1
             | Process.Ready when event.Scheduler.process_id = !running_pid -> 
                 running_pid := -1
             | Process.Waiting when event.Scheduler.process_id = -1 -> 
                 running_pid := -1
             | _ -> ());
            log_ref := rest;
            process_events_at_t current_t
        | _ -> ()
      in
      process_events_at_t t;
      let symbol = 
        if !running_pid = -1 then "[-]"
        else 
          (* Extrai o ID original do processo dividindo por 1000, se for uma instância periódica *)
          let original_id = 
            if !running_pid >= 1000 then
              !running_pid / 1000  (* Por exemplo, instância 1001 -> processo 1 *)
            else
              !running_pid
          in
          Printf.sprintf "[P%d]" original_id
      in
      Buffer.add_string buffer symbol
    done;
    Buffer.contents buffer

(* --- Função para imprimir resultados em formato legível para humanos --- *)
let print_human_readable algo filename tempo_final stats timeline_str processos_tuplos =
  Printf.printf "Algoritmo: %s\n" algo;
  if filename <> "" then Printf.printf "Ficheiro: %s\n" filename;
  Printf.printf "Tempo final: %d\n" tempo_final;
  Printf.printf "\n--- Estatísticas ---\n";
  Printf.printf "Total de processos terminados: %d\n" stats.Statistics.total_processes_completed;
  Printf.printf "Tempo médio de espera: %.2f\n" stats.Statistics.avg_waiting_time;
  Printf.printf "Tempo médio de turnaround: %.2f\n" stats.Statistics.avg_turnaround_time;
  Printf.printf "Utilização da CPU: %.2f%%\n" stats.Statistics.cpu_utilization;
  Printf.printf "Throughput: %.2f\n" stats.Statistics.throughput;
  Printf.printf "Deadline misses: %d\n" stats.Statistics.deadline_misses;
  Printf.printf "\n--- Timeline ---\n%s\n" timeline_str;
  Printf.printf "\n--- Processos Gerados ---\n";
  List.iter (fun (id, arrival_time, burst_time, priority) ->
    Printf.printf "ID: %d | Chegada: %d | Burst: %d | Prioridade/Período: %d\n"
      id arrival_time burst_time priority
  ) processos_tuplos

(* --- Função principal que executa a simulação e imprime o resultado em JSON ou formato humano --- *)
let run_and_output () =
  try
    (* Validação dos argumentos obrigatórios *)
    if !algo_ref = "" then failwith "Algorithm (--algo) is required.";
    if !num_ref = None && !file_ref = "" then failwith "É necessário --file ou --gen.";
    if !algo_ref = "rr" && !quantum_ref = None then failwith "Quantum (--quantum) is required for Round Robin (rr).";
    let algo = !algo_ref in
    let filename = !file_ref in
    let quantum = !quantum_ref in
    let max_time = !max_time_ref in

    let is_realtime = match algo with "rm" | "edf" -> true | _ -> false in
    if is_realtime && max_time = None then failwith "Max simulation time (--max) is required for rm/edf.";

    (* Geração ou leitura dos processos *)
    let (processos_tuplos, processos_iniciais) =
      match !num_ref with
      | Some n ->
          let arrival_lambda = 1.0 in
          let burst_mu = 5.0 in
          let burst_sigma = 2.0 in
          if is_realtime then
            let period_mu = 8.0 in
            let period_sigma = 3.0 in
            let tuplos = Process_generator.generate_processes_rt
              ~n
              ~arrival_lambda
              ~burst_mu
              ~burst_sigma
              ~period_mu
              ~period_sigma
            in
            let processos =
              List.map (fun (id, arrival_time, burst_time, period) ->
                Process.create
                  ~id
                  ~arrival_time
                  ~burst_time
                  ~priority:id  (* Guarda o ID original no campo priority *)
                  ?period:(Some period)
                  ?deadline:(Some (arrival_time + period))
                  ()
              ) tuplos
            in
            (tuplos, processos)
          else
            let tuplos = Process_generator.generate_processes
              ~n
              ~arrival_lambda
              ~burst_mu
              ~burst_sigma
            in
            let processos =
              List.map (fun (id, arrival_time, burst_time, priority) ->
                Process.create
                  ~id
                  ~arrival_time
                  ~burst_time
                  ~priority
                  ()
              ) tuplos
            in
            (tuplos, processos)
      | None ->
          let tuplos = [] in
          let processos =
            if !file_ref = "" then failwith "É necessário --file ou --num."
            else
              if is_realtime then
                Help.ler_ficheiro_dados_processos_rt filename
                |> List.map (fun (id, inicio, burst, periodo) ->
                     Process.create ~id ~arrival_time:inicio ~burst_time:burst ~priority:id
                       ?period:(Some periodo) ?deadline:(Some (inicio + periodo)) ())
              else
                Help.ler_ficheiro_dados_processos filename
                |> List.map (fun (id, inicio, burst, prioridade) ->
                     Process.create ~id ~arrival_time:inicio ~burst_time:burst ~priority:prioridade ())
          in
          (tuplos, processos)
    in

    if processos_iniciais = [] then failwith ("No valid processes loaded from file '" ^ filename ^ "'.");

    (* Execução da simulação consoante o algoritmo escolhido *)
    let simulation_result, processos_para_stats =
      try
        match algo with
        | "fcfs" -> (Some (Scheduler.fcfs processos_iniciais), processos_iniciais)
        | "sjf" -> (Some (Scheduler.sjf processos_iniciais), processos_iniciais)
        | "priority_np" -> (Some (Scheduler.priority_non_preemptive processos_iniciais), processos_iniciais)
        | "priority_preemp" -> (Some (Scheduler.priority_preemptive processos_iniciais), processos_iniciais)
        | "rr" -> (match quantum with
                  | Some q -> (Some (Scheduler.round_robin ~quantum:q processos_iniciais), processos_iniciais)
                  | None -> failwith "Internal error: Quantum missing for RR")
        | "rm" -> (match max_time with
                  | Some m ->
                      let insts = Scheduler.gerar_instancias_periodicas processos_iniciais m in
                      (Some (Scheduler.rate_monotonic ~tempo_max:m insts), insts)
                  | None -> failwith "Internal error: Max time missing for RM")
        | "edf"-> (match max_time with
                  | Some m ->
                      let insts = Scheduler.gerar_instancias_periodicas processos_iniciais m in
                      (Some (Scheduler.edf ~tempo_max:m insts), insts)
                  | None -> failwith "Internal error: Max time missing for EDF")
        | _ -> failwith ("Algorithm '" ^ algo ^ "' not recognized or implemented."), processos_iniciais
      with ex -> failwith ("Error during simulation for algorithm '" ^ algo ^ "': " ^ Printexc.to_string ex), processos_iniciais
    in

    (* Impressão do resultado no formato escolhido *)
    match simulation_result with
    | None -> failwith "Simulation failed to produce results."
    | Some (tempo_final, log_eventos) ->
        let stats = Statistics.calculate_statistics processos_para_stats tempo_final log_eventos in
        let timeline_str = format_timeline_string log_eventos tempo_final in

        if !human_ref then
          print_human_readable algo filename tempo_final stats timeline_str processos_tuplos
        else
          let json_output =
            `Assoc [
              ("success", `Bool true);
              ("results", `Assoc [
                  ("algorithm", `String algo);
                  ("file", `String filename);
                  ("final_time", `Int tempo_final);
                  ("stats", stats_to_json stats);
                  ("timeline_string", `String timeline_str);
                  ("processes_generated",
                    `List (List.map (fun (id, arrival_time, burst_time, priority) ->
                      `List [
                        `Int id;
                        `Int arrival_time;
                        `Int burst_time;
                        `Int priority
                      ]) processos_tuplos)
                  )
                ])
            ]
          in
          print_endline (Yojson.Basic.to_string json_output)

  with
  | Failure msg ->
      let json_error = `Assoc [("success", `Bool false); ("error", `String msg)] in
      print_endline (Yojson.Basic.to_string json_error);
      exit 1
  | ex ->
      let err_msg = "Erro inesperado: " ^ Printexc.to_string ex in
      let json_error = `Assoc [("success", `Bool false); ("error", `String err_msg)] in
      print_endline (Yojson.Basic.to_string json_error);
      exit 1

(* --- Ponto de Entrada Principal --- *)
let () =
  Arg.parse speclist (fun anon_arg -> raise (Arg.Bad ("Argumento inesperado: " ^ anon_arg))) usage_msg;
  run_and_output ()
