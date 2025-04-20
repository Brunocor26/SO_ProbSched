(* usamos o pid=-1 para representar a cpu e o seu estado  *)

(* Tipo para representar um evento da timeline a cada instante *)
type timeline_event = {
  time : int;       (*instante do evento *)
  process_id : int; (* Usar -1 para CPU livre *)
  new_state : Process.process_state; (* Running, Terminated, Ready, Waiting *)
}

(* FCFS
 Recebe:  processes : Process.t list  - lista de processos 
 Devolve:  (int * timeline_event list)  um tuplo com: tempo total (int) e uma lista de eventos por ordem *)
let fcfs (processes : Process.t list) : int * timeline_event list =
  let time = ref 0 in (* inicializa um "relógio" *)
  let schedule_log = ref [] in (* lista que vai guardar os eventos *)

  (*ordena a lista com base na sua ordem de chegada *)
  let sorted_procs = List.sort (fun p1 p2 -> compare p1.Process.arrival_time p2.Process.arrival_time) processes in

  (* Função auxiliar para adicionar um evento ao log *)
  let log_event t pid state =
    schedule_log := { time = t; process_id = pid; new_state = state } :: !schedule_log
  in

  (* itera sobre cada processo da lista já ordenada *)
  List.iter (fun p ->
    (* verifica se a cpu esteve idle (sem fazer nada) *) (*! para aceder ao valor do ref*)
    if !time < p.Process.arrival_time then begin
      log_event !time (-1) Process.Waiting; (* Marca início do idle, dizemos que ha um evento onde a cpu fica waiting *)
      time := p.Process.arrival_time;       (* := modifica o valor do ref, esta linha basicamente mete o cpu ocupado *)
      log_event !time (-1) Process.Ready;   (* CPU fica ocupada *)
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


(*Shortest Job First (não preemptivo)*)
(* --- Ficheiro: scheduler.ml --- *)

(* As definições de Process.t, Process.process_state e timeline_event são necessárias *)
(* Exemplo: *)
(* type timeline_event = { time: int; process_id: int; new_state: Process.process_state } *)


(* --- Algoritmo SJF (Shortest Job First) Não-Preemptivo --- *)
(*
 * Executa o escalonamento SJF Não-Preemptivo.
 * Quando a CPU fica livre, escolhe o processo da fila de prontos
 * (que já chegaram) com o menor tempo de burst (burst_time).
 * O processo escolhido executa até ao fim sem interrupção.
 * Argumentos:
 * processes: Process.t list - A lista inicial de processos.
 * Retorna:
 * (int * timeline_event list) - Tupla com tempo final e log da timeline.
 * Efeitos Colaterais: Modifica os processos na lista de entrada.
 *)
 let sjf (processes : Process.t list) : int * timeline_event list =
  (* --- Inicialização --- *)
  let time = ref 0 in                  (* Relógio da simulação *)
  let schedule_log = ref [] in         (* Log de eventos da timeline *)
  let completed_count = ref 0 in       (* Contador de processos terminados *)
  let num_processes = List.length processes in (* Número total de processos *)

  (* Lista de processos que ainda não chegaram, ordenada por tempo de chegada *)
  let incoming_processes = ref (List.sort (fun p1 p2 -> compare p1.Process.arrival_time p2.Process.arrival_time) processes) in
  (* Lista de processos que já chegaram mas estão à espera da CPU (fila de prontos) *)
  let ready_queue = ref [] in

  (* Inicializa campos mutáveis dos processos (necessário para cálculo de stats depois) *)
  List.iter (fun p ->
      p.Process.remaining_burst_time <- p.Process.burst_time;
      p.Process.state <- Process.Ready; (* Assume Ready como estado inicial conceptual *)
      p.Process.waiting_time <- 0;      (* Será calculado *)
      p.Process.completion_time <- None;
      p.Process.turnaround_time <- None;
  ) processes;

  (* Função auxiliar para logar eventos *)
  let log_event t pid state =
    schedule_log := { time = t; process_id = pid; new_state = state } :: !schedule_log
  in

  (* --- Loop Principal da Simulação --- *)
  (* Continua enquanto houver processos por terminar *)
  while !completed_count < num_processes do

    (* 1. Adicionar processos que chegaram à fila de prontos *)
    (* Filtra os processos em 'incoming' cujo tempo de chegada é <= tempo atual *)
    let arrived_now = List.filter (fun p -> p.Process.arrival_time <= !time) !incoming_processes in
    if arrived_now <> [] then (
      (* Adiciona os recém-chegados à fila de prontos *)
      ready_queue := List.rev_append arrived_now !ready_queue;
      (* Remove os recém-chegados da lista 'incoming' *)
      incoming_processes := List.filter (fun p -> p.Process.arrival_time > !time) !incoming_processes
    );

    (* 2. Decisão de Escalonamento: Ocorre quando a CPU está livre *)
    (* Neste modelo simples, consideramos a CPU livre no início de cada iteração *)
    (* se não houver um processo a ser "continuado". Como é não-preemptivo, *)
    (* a decisão só é feita quando um processo termina ou no início se time=0. *)
    (* A forma como o loop está, ele efetivamente reavalia a cada passo de tempo implícito *)
    (* (quando um processo termina ou quando o tempo avança por ociosidade). *)

    if !ready_queue <> [] then begin
      (* --- Caso 1: Há processos prontos --- *)

      (* Ordena a fila de prontos pelo menor burst_time *)
      ready_queue := List.sort (fun p1 p2 -> compare p1.Process.burst_time p2.Process.burst_time) !ready_queue;

      (* Seleciona o processo com menor burst_time *)
      let current_process = List.hd !ready_queue in
      (* Remove-o da fila de prontos *)
      ready_queue := List.tl !ready_queue;

      (* Verifica se houve tempo ocioso antes deste processo começar *)
      (* (Isto pode não ser estritamente necessário se o tempo só avança quando algo termina ou chega) *)
      (* No entanto, garante que o tempo está correto caso um processo chegue mais tarde *)
       if !time < current_process.Process.arrival_time then begin
          log_event !time (-1) Process.Waiting; (* Log inicio idle *)
          time := current_process.Process.arrival_time;
          log_event !time (-1) Process.Ready; (* Log fim idle *)
        end;

      (* Inicia a execução do processo selecionado *)
      log_event !time current_process.Process.id Process.Running;
      current_process.Process.state <- Process.Running;

      (* Calcula tempos *)
      let completion = !time + current_process.Process.burst_time in
      let wait = !time - current_process.Process.arrival_time in
      let turnaround = completion - current_process.Process.arrival_time in

      (* Atualiza os dados do processo *)
      current_process.Process.waiting_time <- wait;
      current_process.Process.completion_time <- Some completion;
      current_process.Process.turnaround_time <- Some turnaround;
      current_process.Process.remaining_burst_time <- 0; (* Terminou *)

      (* Loga o fim da execução *)
      log_event completion current_process.Process.id Process.Terminated;
      current_process.Process.state <- Process.Terminated;

      (* Avança o tempo para o fim da execução deste processo *)
      time := completion;
      (* Incrementa o contador de processos terminados *)
      incr completed_count;

    end else if !incoming_processes <> [] then begin
      (* --- Caso 2: Fila de prontos vazia, mas há processos para chegar --- *)
      (* A CPU fica ociosa até à chegada do próximo processo *)

      (* Encontra o tempo de chegada do próximo processo *)
      let next_arrival_time = (List.hd !incoming_processes).Process.arrival_time in
      (* Verifica se realmente precisamos avançar o tempo (evita logs de idle de duração 0) *)
      if next_arrival_time > !time then begin
        (* Loga o início do período ocioso *)
        log_event !time (-1) Process.Waiting;
        (* Avança o tempo para a chegada do próximo *)
        time := next_arrival_time;
        (* Loga o fim do período ocioso *)
        log_event !time (-1) Process.Ready;
       end else begin
          (* Próximo processo chegou exatamente agora, não há tempo ocioso. *)
          (* O loop vai tratar da chegada na próxima iteração (ou já tratou). *)
          (* Poderia até haver um `continue` implícito aqui. *)
          ()
       end

    end else begin
      (* --- Caso 3: Fila de prontos vazia E não há mais processos para chegar --- *)
      (* Se completed_count < num_processes, algo está errado. *)
      (* Mas se completed_count = num_processes, o loop while vai terminar. *)
      if !completed_count < num_processes then
         (* Esta situação não deveria ocorrer num cenário normal. Pode indicar um bug. *)
         failwith "Erro na simulação SJF: Loop ativo sem processos prontos ou futuros."
         (* Se completed_count = num_processes, o loop termina naturalmente. *)
    end

  done; (* Fim do while *)

  (* Retorna o tempo final e o log cronológico *)
  (!time, List.rev !schedule_log)

  (* --- Algoritmo de Prioridade Não-Preemptivo --- *)
(*
 * Executa o escalonamento por Prioridade Não-Preemptivo.
 * Quando a CPU fica livre, escolhe o processo da fila de prontos
 * (que já chegaram) com a maior prioridade (menor valor numérico de prioridade).
 * O processo escolhido executa até ao fim sem interrupção.
 * Utiliza o módulo Priority_queue fornecido para gerir a fila de prontos.
 * Argumentos:
 * processes: Process.t list - A lista inicial de processos.
 * Retorna:
 * (int * timeline_event list) - Tupla com tempo final e log da timeline.
 * Efeitos Colaterais: Modifica os processos na lista de entrada.
 *)
 let priority_non_preemptive (processes : Process.t list) : int * timeline_event list =
  (* --- Inicialização --- *)
  let time = ref 0 in
  let schedule_log = ref [] in
  let completed_count = ref 0 in
  let num_processes = List.length processes in

  (* Lista de processos que ainda não chegaram, ordenada por tempo de chegada *)
  let incoming_processes = ref (List.sort (fun p1 p2 -> compare p1.Process.arrival_time p2.Process.arrival_time) processes) in

  (* Cria a fila de prioridade (ready queue) usando o módulo fornecido. *)
  (* A função de prioridade extrai o campo 'priority' do registo Process.t. *)
  let ready_queue = Priority_queue.create (fun p -> p.Process.priority) in

  (* Inicializa campos mutáveis dos processos *)
  List.iter (fun p ->
      p.Process.remaining_burst_time <- p.Process.burst_time;
      p.Process.state <- Process.Ready;
      p.Process.waiting_time <- 0;
      p.Process.completion_time <- None;
      p.Process.turnaround_time <- None;
  ) processes;

  (* Função auxiliar para logar eventos *)
  let log_event t pid state =
    schedule_log := { time = t; process_id = pid; new_state = state } :: !schedule_log
  in

  (* --- Loop Principal da Simulação --- *)
  while !completed_count < num_processes do

    (* 1. Adicionar processos que chegaram à fila de prioridade *)
    let arrived_now = List.filter (fun p -> p.Process.arrival_time <= !time) !incoming_processes in
    if arrived_now <> [] then (
      (* Adiciona cada processo recém-chegado à fila de prioridade. *)
      (* A função 'add' do módulo Priority_queue mantém a ordem correta. *)
      List.iter (fun p -> Priority_queue.add ready_queue p) arrived_now;
      (* Remove os recém-chegados da lista 'incoming' *)
      incoming_processes := List.filter (fun p -> p.Process.arrival_time > !time) !incoming_processes
    );

    (* 2. Decisão de Escalonamento: Escolher o próximo processo a executar *)
    (* Verifica se a fila de prontos (priority queue) não está vazia *)
    if not (Priority_queue.is_empty ready_queue) then begin
      (* --- Caso 1: Há processos prontos --- *)

      (* Extrai (remove e retorna) o processo com maior prioridade (menor valor numérico) *)
      (* A função 'take' do módulo Priority_queue faz isso. *)
      let current_process = Priority_queue.take ready_queue in

      (* Verifica e trata possível tempo ocioso antes deste processo começar *)
       if !time < current_process.Process.arrival_time then begin
          log_event !time (-1) Process.Waiting; (* Log inicio idle *)
          time := current_process.Process.arrival_time;
          log_event !time (-1) Process.Ready; (* Log fim idle *)
        end;

      (* Inicia a execução do processo selecionado *)
      log_event !time current_process.Process.id Process.Running;
      current_process.Process.state <- Process.Running;

      (* Calcula tempos (como é não-preemptivo, executa todo o burst_time) *)
      let completion = !time + current_process.Process.burst_time in
      let wait = !time - current_process.Process.arrival_time in
      let turnaround = completion - current_process.Process.arrival_time in

      (* Atualiza os dados do processo no registo original (mutável) *)
      current_process.Process.waiting_time <- wait;
      current_process.Process.completion_time <- Some completion;
      current_process.Process.turnaround_time <- Some turnaround;
      current_process.Process.remaining_burst_time <- 0; (* Terminou *)

      (* Loga o fim da execução *)
      log_event completion current_process.Process.id Process.Terminated;
      current_process.Process.state <- Process.Terminated;

      (* Avança o tempo para o fim da execução deste processo *)
      time := completion;
      (* Incrementa o contador de processos terminados *)
      incr completed_count;

    end else if !incoming_processes <> [] then begin
      (* --- Caso 2: Fila de prontos vazia, mas há processos para chegar --- *)
      (* A CPU fica ociosa até à chegada do próximo processo *)

      let next_arrival_time = (List.hd !incoming_processes).Process.arrival_time in
      (* Só avança o tempo e loga se realmente houver um intervalo idle *)
      if next_arrival_time > !time then begin
        log_event !time (-1) Process.Waiting; (* Loga início do idle *)
        time := next_arrival_time;          (* Avança o tempo *)
        log_event !time (-1) Process.Ready;   (* Loga fim do idle *)
       end
      (* Se next_arrival_time <= !time, significa que chegou agora ou já tinha chegado,*)
      (* o loop tratará disso na próxima iteração ao verificar chegadas. *)

    end else if !completed_count < num_processes then
      (* --- Caso 3: Fila de prontos vazia E não há mais processos para chegar --- *)
      (* Se ainda não terminámos todos, algo está errado *)
      failwith "Erro na simulação Prioridade NP: Loop ativo sem processos prontos ou futuros."
      (* Caso contrário, o loop termina porque !completed_count = num_processes *)

  done; (* Fim do while *)

  (* Retorna o tempo final e o log cronológico *)
  (!time, List.rev !schedule_log)