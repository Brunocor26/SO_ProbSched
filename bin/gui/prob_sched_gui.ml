(* Abrir a biblioteca principal, se houver uma *)
open Prob_sched

(* --- Módulos --- *)
open Tk
open Process     (* Assume float times, remaining_time, deadline, completion_time option *)
open Scheduler   (* Assumir que as funções retornam { schedule; completed_processes } *)
open Stats       (* Assumir que existe e funciona como definido *)

(* --- Configuração Inicial --- *)
let next_pid = ref 1 (* Começar PID em 1 *)
let generate_pid () =
  let pid = !next_pid in
  next_pid := !next_pid + 1;
  pid

(* --- Elementos da GUI --- *)
let top = openTk ()
let () = Wm.title_set top "Criador Processo & Simulador (Combinado)"

(* Variáveis (OCaml refs) para armazenar dados e estado da GUI *)
let display_output = ref "Preencha os dados do processo (serão ignorados pela simulação fixa)\ne clique 'Criar Processo' para executar a simulação."

(* --- Widgets da Interface --- *)

(* Frame para Inputs do Processo (como antes) *)
let input_frame = Frame.create top ~borderwidth:2 ~relief:`Groove
let () = pack [coe input_frame] ~pady:5 ~padx:5 ~fill:`X
let name_label = Label.create input_frame ~text:"Nome:"
let name_entry = Entry.create input_frame
(* ... outros labels e entries para arrival, burst, priority ... *)
let arrival_label = Label.create input_frame ~text:"Chegada (float):"
let arrival_entry = Entry.create input_frame
let burst_label = Label.create input_frame ~text:"Burst (float):"
let burst_entry = Entry.create input_frame
let priority_label = Label.create input_frame ~text:"Prioridade (1-5):"
let priority_entry = Entry.create input_frame
(* ... (grid layout como antes) ... *)
let () = grid [coe name_label] ~row:0 ~column:0 ~sticky:"w" ~padx:2 ~pady:2
let () = grid [coe name_entry] ~row:0 ~column:1 ~sticky:"we" ~padx:2 ~pady:2
let () = grid [coe arrival_label] ~row:1 ~column:0 ~sticky:"w" ~padx:2 ~pady:2
let () = grid [coe arrival_entry] ~row:1 ~column:1 ~sticky:"we" ~padx:2 ~pady:2
let () = grid [coe burst_label] ~row:2 ~column:0 ~sticky:"w" ~padx:2 ~pady:2
let () = grid [coe burst_entry] ~row:2 ~column:1 ~sticky:"we" ~padx:2 ~pady:2
let () = grid [coe priority_label] ~row:3 ~column:0 ~sticky:"w" ~padx:2 ~pady:2
let () = grid [coe priority_entry] ~row:3 ~column:1 ~sticky:"we" ~padx:2 ~pady:2
let () = Grid.column_configure input_frame 1 ~weight:1


(* --- Frame de Simulação (Seleção de Algoritmo e Quantum) --- *)
let sim_frame = Frame.create top ~borderwidth:2 ~relief:`Groove
let () = pack [coe sim_frame] ~pady:5 ~padx:5 ~fill:`X

(* Seleção de Algoritmo (Usando OptionMenu) *)
let nomes_algoritmos = ["FCFS"; "SJF"; "Priority_NP"; "Priority_P"; "RR"; "RM"; "EDF"]
let var_algoritmo_selecionado = Textvariable.create ()
let valor_inicial = List.hd nomes_algoritmos
let () = Textvariable.set var_algoritmo_selecionado valor_inicial
let label_selecao_algoritmo = Label.create sim_frame ~text:"Algoritmo:"
let menu_selecao_algoritmo, _ = Optionmenu.create ~parent:sim_frame ~variable:var_algoritmo_selecionado nomes_algoritmos
let () = pack [coe label_selecao_algoritmo] ~side:`Left ~padx:5
let () = pack [coe menu_selecao_algoritmo] ~side:`Left ~padx:5 ~fill:`X ~expand:true

(* Input para Quantum (necessário para RR) *)
let quantum_label = Label.create sim_frame ~text:"Quantum (RR):"
let quantum_entry = Entry.create sim_frame ~width:5
let () = Entry.insert quantum_entry ~index:(`Num 0) ~text:"1.0" (* Default quantum *)
let () = pack [coe quantum_label] ~side:`Left ~padx:5
let () = pack [coe quantum_entry] ~side:`Left ~padx:5


(* --- Botão para Criar Processo (AGORA EXECUTA SIMULAÇÃO) --- *)
let create_run_button = Button.create top ~text:"Criar Processo / Executar Simulação Fixa" (* Comando definido abaixo *)
let () = pack [coe create_run_button] ~pady:10

(* --- Frame para Exibir Saída (Resultados da Simulação) --- *)
let display_frame = Frame.create top ~borderwidth:2 ~relief:`Groove
let display_label = Label.create display_frame ~text:!display_output ~justify:`Left ~anchor:`Nw
let () = pack [coe display_frame] ~pady:5 ~padx:5 ~fill:`Both ~expand:true


(* --- Lógica do Botão (AGORA EXECUTA SIMULAÇÃO) --- *)
let create_and_run_action () =
  (* Mensagem de espera *)
  display_output := "A executar simulação...";
  Label.configure display_label ~text:!display_output;
  update (); (* Força atualização da GUI *)

  try
    (* !! VALIDAÇÃO DOS INPUTS (opcional, pois não serão usados na simulação fixa) !! *)
    let _name = Entry.get name_entry in (* Lê mas ignora *)
    let _arrival_str = Entry.get arrival_entry in
    let _burst_str = Entry.get burst_entry in
    let _priority_str = Entry.get priority_entry in
    (* Poderia adicionar a validação aqui para dar feedback ao user, mesmo que não use os dados *)
    (* Ex: if _name = "" then raise (Failure "Nome não pode ser vazio"); *)


    (* !! DEFINIR LISTA DE PROCESSOS FIXA PARA A SIMULAÇÃO !! *)
    (* Certifique-se que create_process está correto em process.ml *)
    let simulation_input_list : Process.process list = [
      create_process 1 "P1" 0.0 5.0 2 Ready;
      create_process 2 "P2" 1.0 3.0 1 Ready;
      create_process 3 "P3" 2.0 8.0 3 Ready;
      create_process 4 "P4" 3.0 6.0 2 Ready;
      (* create_process 5 "P5_RT" 4.0 2.0 1 Ready (Some 10.0); *) (* Exemplo RT *)
    ] in
    (* IMPORTANTE: Resetar remaining_time e completion_time antes de cada simulação! *)
    let current_processes = List.map (fun p ->
      { p with remaining_time = p.burst_time; completion_time = None }
    ) simulation_input_list in

    (* Obter algoritmo selecionado *)
    let nome_algo_escolhido = Textvariable.get var_algoritmo_selecionado in

    (* Executar a simulação *)
    let result : Scheduler.result =
      match nome_algo_escolhido with
      | "FCFS"        -> Scheduler.fcfs current_processes
      (*| "SJF"         -> Scheduler.sjf current_processes
      | "Priority_NP" -> Scheduler.priority_non_preemptive current_processes
      | "Priority_P"  -> Scheduler.priority_preemptive current_processes
      | "RR" ->
          let q_str = Entry.get quantum_entry in
          let quantum = try float_of_string q_str with Failure _ -> 1.0 in
          let valid_quantum = if quantum <= 0. then (Printf.eprintf "Quantum inválido, usando 1.0\n"; 1.0) else quantum in
          Scheduler.round_robin current_processes ~quantum:valid_quantum
      | "RM"          -> Scheduler.rate_monotonic current_processes
      | "EDF"         -> Scheduler.edf current_processes
      *)
      | s             -> failwith ("Algoritmo não implementado ou desconhecido: " ^ s)
    in

    (* Processar resultados e calcular estatísticas *)
    let run_data_for_stats : Stats.process_run_data list =
      List.map (fun (proc, comp_time) ->
        { Stats.id = proc.pid;
          Stats.arrival_time = proc.arrival_time;
          Stats.burst_time = proc.burst_time; (* Original *)
          Stats.completion_time = comp_time;
          Stats.deadline = proc.deadline;
        }
      ) result.completed_processes
    in

    (* Calcular tempos totais (simplificado) *)
    let total_simulation_time =
      match List.rev result.completed_processes with
      | [] -> 0.0
      | (_, last_completion_time) :: _ -> last_completion_time
    in
    let total_cpu_busy_time =
      List.fold_left (fun acc (proc, _) -> acc +. proc.burst_time) 0.0 result.completed_processes
    in

    (* Calcular estatísticas finais *)
    let final_stats = Stats.calculate_statistics
      ~completed_processes:run_data_for_stats
      ~total_simulation_time:total_simulation_time
      ~total_cpu_busy_time:total_cpu_busy_time
    in
    let stats_output = Stats.format_results final_stats in

    (* Formatar Schedule (simples) *)
    let schedule_output =
      "--- Schedule ---\n" ^
      (String.concat "\n" (List.map (fun (t, pid) -> Printf.sprintf " %.1f: PID %d" t pid) result.schedule)) ^
      "\n--- Fim Schedule ---"
    in

    (* Atualizar label de saída com OS RESULTADOS DA SIMULAÇÃO *)
    display_output := "--- Simulação Concluída ---\n\n" ^ schedule_output ^ "\n\n" ^ stats_output;
    Label.configure display_label ~text:!display_output

  with
  | Failure msg ->
      display_output := "Erro durante a simulação:\n" ^ msg;
      Label.configure display_label ~text:!display_output;
      Printf.eprintf "Erro: %s\n%!" msg
  | e -> (* Captura outras excepções *)
      display_output := "Erro inesperado na simulação:\n" ^ (Printexc.to_string e);
      Label.configure display_label ~text:!display_output;
      Printf.eprintf "Erro Inesperado: %s\n" (Printexc.to_string e)

(* Associa a ação ao botão *)
let () = Button.configure create_run_button ~command:create_and_run_action

(* --- Loop Principal da GUI --- *)
let () =
  try
    print_endline "A iniciar a interface gráfica...";
    mainLoop ()
  with e ->
    Printf.eprintf "Erro fatal na aplicação GUI: %s\n" (Printexc.to_string e);
    exit 1