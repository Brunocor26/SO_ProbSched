(* bin/main.ml *)
open Prob_sched_lib
open Yojson.Basic

(* --- Referências para guardar os argumentos da linha de comando --- *)
let algo_ref = ref ""
let file_ref = ref ""
let quantum_ref = ref (None : int option)
let max_time_ref = ref (None : int option)  (* NOVO *)

(* --- Especificação dos Argumentos --- *)
let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " --algo <name> --file <path> [--quantum <int>] [--max <int>]"
let speclist = [
  ("--algo", Arg.Set_string algo_ref, " Scheduling algorithm (fcfs, sjf, priority_np, priority_preemp, rr, rm, edf)");
  ("--file", Arg.Set_string file_ref, " Path to the process definition file");
  ("--quantum", Arg.Int (fun q -> quantum_ref := Some q), " Time quantum for Round Robin (required if --algo rr)");
  ("--max", Arg.Int (fun m -> max_time_ref := Some m), " Max simulation time (required for rm/edf)");  (* NOVO *)
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

(* --- Função para Formatar Timeline --- *)
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
             | Process.Running -> running_pid := event.Scheduler.process_id
             | Process.Terminated when event.Scheduler.process_id = !running_pid -> running_pid := -1
             | Process.Ready when event.Scheduler.process_id = !running_pid -> running_pid := -1
             | Process.Waiting when event.Scheduler.process_id = -1 -> running_pid := -1
             | _ -> ());
            log_ref := rest;
            process_events_at_t current_t
        | _ -> ()
      in
      process_events_at_t t;
      let symbol = if !running_pid = -1 then "[-]" else Printf.sprintf "[P%d]" !running_pid in
      Buffer.add_string buffer symbol
    done;
    Buffer.contents buffer

(* --- Função principal que executa e imprime JSON --- *)
let run_and_output () =
  try
    if !algo_ref = "" || !file_ref = "" then failwith "Algorithm (--algo) and file (--file) are required.";
    if !algo_ref = "rr" && !quantum_ref = None then failwith "Quantum (--quantum) is required for Round Robin (rr).";
    let algo = !algo_ref in
    let filename = !file_ref in
    let quantum = !quantum_ref in
    let max_time = !max_time_ref in

    let is_realtime = match algo with "rm" | "edf" -> true | _ -> false in
    if is_realtime && max_time = None then failwith "Max simulation time (--max) is required for rm/edf.";

    let processos_iniciais =
      try
        if is_realtime then
          Help.ler_ficheiro_dados_processos_rt filename
          |> List.map (fun (id, inicio, burst, periodo) ->
               Process.create ~id ~arrival_time:inicio ~burst_time:burst ~priority:periodo
                 ?period:(Some periodo) ?deadline:(Some (inicio + periodo)) ())
        else
          Help.ler_ficheiro_dados_processos filename
          |> List.map (fun (id, inicio, burst, prioridade) ->
               Process.create ~id ~arrival_time:inicio ~burst_time:burst ~priority:prioridade ())
      with
      | Sys_error msg -> failwith ("Error accessing file '" ^ filename ^ "': " ^ msg)
      | End_of_file -> failwith ("Unexpected end of file while reading '" ^ filename ^ "'.")
      | ex -> failwith ("Error reading process file '" ^ filename ^ "': " ^ Printexc.to_string ex)
    in

    if processos_iniciais = [] then failwith ("No valid processes loaded from file '" ^ filename ^ "'.");

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
                      (Some (Scheduler.rate_monotonic ~tempo_max:m processos_iniciais), insts)
                  | None -> failwith "Internal error: Max time missing for RM")
        | "edf"-> (match max_time with
                  | Some m ->
                      let insts = Scheduler.gerar_instancias_periodicas processos_iniciais m in
                      (Some (Scheduler.edf ~tempo_max:m processos_iniciais), insts)
                  | None -> failwith "Internal error: Max time missing for EDF")
        | _ -> failwith ("Algorithm '" ^ algo ^ "' not recognized or implemented."), processos_iniciais
      with ex -> failwith ("Error during simulation for algorithm '" ^ algo ^ "': " ^ Printexc.to_string ex), processos_iniciais
    in

    match simulation_result with
    | None -> failwith "Simulation failed to produce results."
    | Some (tempo_final, log_eventos) ->
        let stats = Statistics.calculate_statistics processos_para_stats tempo_final log_eventos in
        let timeline_str = format_timeline_string log_eventos tempo_final in

        let json_output =
          `Assoc [
            ("success", `Bool true);
            ("results", `Assoc [
                ("algorithm", `String algo);
                ("file", `String filename);
                ("final_time", `Int tempo_final);
                ("stats", stats_to_json stats);
                ("timeline_string", `String timeline_str)
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
      let err_msg = "Unexpected error: " ^ Printexc.to_string ex in
      let json_error = `Assoc [("success", `Bool false); ("error", `String err_msg)] in
      print_endline (Yojson.Basic.to_string json_error);
      exit 1

(* --- Ponto de Entrada Principal --- *)
let () =
  Arg.parse speclist (fun anon_arg -> raise (Arg.Bad ("Unexpected argument: " ^ anon_arg))) usage_msg;
  run_and_output ()
