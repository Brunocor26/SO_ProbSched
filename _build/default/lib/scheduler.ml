open Process

(* Global variables for instance tracking *)
let all_instances = ref []
let completed_instances = ref []
let next_instance_id = ref 1000

(* Function to get completed instances - for use in statistics *)
let get_completed_instances () = !completed_instances

(* Function to get all instances - for use in statistics *)
let get_all_instances () = !all_instances

type timeline_event = {
  time : int;
  process_id : int; (* -1 para CPU livre *)
  new_state : process_state;
  instance_id : int option; (* Para identificar instâncias de processos periódicos *)
}

(* Função auxiliar para obter o valor de uma opção ou lançar erro *)
let get_option_value opt err_msg =
  match opt with
  | Some v -> v
  | None -> failwith err_msg

(* Function to generate next instance ID *)
let get_next_instance_id () =
  let current = !next_instance_id in
  incr next_instance_id;
  current

(* Função auxiliar para gerar ID estruturado para instância de processo periódico *)
let get_structured_instance_id process_id instance_count =
  (process_id * 1000) + instance_count

let gerar_instancias_periodicas processos tempo_max =
  let instancias = ref [] in
  List.iter (fun p ->
    match p.period, p.deadline with
    | Some per, Some dl ->
        let rec gera t instance_count =
          if t < tempo_max then (
            let instance_id = get_structured_instance_id p.id instance_count in
            let nova_inst = { p with
              id = instance_id;
              name = p.name ^ "_inst" ^ string_of_int instance_count;
              arrival_time = t;
              deadline = Some (t + dl);
              remaining_burst_time = p.burst_time;
              state = Ready;
              completion_time = None;
              turnaround_time = None;
              waiting_time = 0;
              priority = p.priority;  (* Só para visualização *)
              period = p.period;      (* Fundamental para RM! *)
            } in
            instancias := nova_inst :: !instancias;
            gera (t + per) (instance_count + 1)
          )
        in
        gera p.arrival_time 1
    | _ -> instancias := p :: !instancias
  ) processos;
  !instancias

let log_event schedule_log t pid state =
  schedule_log := { time = t; process_id = pid; new_state = state; instance_id = None } :: !schedule_log

let log_event_with_instance schedule_log t pid state instance_id =
  let display_id = 
    if instance_id >= 1000 then
      (* For real-time instances, use the original process ID stored in priority *)
      (match List.find_opt (fun p -> p.id = instance_id) !all_instances with
       | Some p -> p.priority  (* priority contains original process ID *)
       | None -> pid)
    else 
      pid
  in
  schedule_log := { 
    time = t; 
    process_id = display_id;  (* Use display_id instead of raw process_id *)
    new_state = state; 
    instance_id = Some instance_id 
  } :: !schedule_log

let reset_process p =
  p.state <- Ready;
  p.waiting_time <- 0;
  p.completion_time <- None;
  p.turnaround_time <- None

let sort_by_arrival = List.sort (fun p1 p2 -> compare p1.arrival_time p2.arrival_time)

let fcfs (processes : t list) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let sorted_procs = sort_by_arrival processes in
  List.iter reset_process processes;
  List.iter (fun p ->
    if !time < p.arrival_time then (
      log_event schedule_log !time (-1) Waiting;
      time := p.arrival_time;
      log_event schedule_log !time (-1) Ready;
    );
    log_event schedule_log !time p.id Running;
    p.state <- Running;
    let wait = !time - p.arrival_time in
    let completion = !time + p.burst_time in
    let turnaround = completion - p.arrival_time in
    p.waiting_time <- wait;
    p.completion_time <- Some completion;
    p.turnaround_time <- Some turnaround;
    p.remaining_burst_time <- 0;
    time := completion;
    log_event schedule_log !time p.id Terminated;
    p.state <- Terminated;
  ) sorted_procs;
  (!time, List.rev !schedule_log)

let sjf (processes : t list) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let completed_count = ref 0 in
  let num_processes = List.length processes in
  let incoming = ref (sort_by_arrival processes) in
  let ready_queue = ref [] in
  List.iter reset_process processes;
  while !completed_count < num_processes do
    let arrived_now = List.filter (fun p -> p.arrival_time <= !time) !incoming in
    if arrived_now <> [] then (
      ready_queue := List.rev_append arrived_now !ready_queue;
      incoming := List.filter (fun p -> p.arrival_time > !time) !incoming
    );
    if !ready_queue <> [] then begin
      ready_queue := List.sort (fun p1 p2 -> compare p1.burst_time p2.burst_time) !ready_queue;
      let p = List.hd !ready_queue in
      ready_queue := List.tl !ready_queue;
      if !time < p.arrival_time then (
        log_event schedule_log !time (-1) Waiting;
        time := p.arrival_time;
        log_event schedule_log !time (-1) Ready;
      );
      log_event schedule_log !time p.id Running;
      p.state <- Running;
      let completion = !time + p.burst_time in
      let wait = !time - p.arrival_time in
      let turnaround = completion - p.arrival_time in
      p.waiting_time <- wait;
      p.completion_time <- Some completion;
      p.turnaround_time <- Some turnaround;
      p.remaining_burst_time <- 0;
      log_event schedule_log completion p.id Terminated;
      p.state <- Terminated;
      time := completion;
      incr completed_count;
    end else if !incoming <> [] then (
      let next_arrival = (List.hd !incoming).arrival_time in
      if next_arrival > !time then (
        log_event schedule_log !time (-1) Waiting;
        time := next_arrival;
        log_event schedule_log !time (-1) Ready;
      )
    ) else if !completed_count < num_processes then
      failwith "Erro na simulação SJF: Loop ativo sem processos prontos ou futuros."
  done;
  (!time, List.rev !schedule_log)

let priority_non_preemptive (processes : t list) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let completed_count = ref 0 in
  let num_processes = List.length processes in
  let incoming = ref (sort_by_arrival processes) in
  let ready_queue = Priority_queue.create (fun p -> p.priority) in
  List.iter reset_process processes;
  while !completed_count < num_processes do
    let arrived_now = List.filter (fun p -> p.arrival_time <= !time) !incoming in
    if arrived_now <> [] then (
      List.iter (fun p -> Priority_queue.add ready_queue p) arrived_now;
      incoming := List.filter (fun p -> p.arrival_time > !time) !incoming
    );
    if not (Priority_queue.is_empty ready_queue) then begin
      let p = Priority_queue.take ready_queue in
      if !time < p.arrival_time then (
        log_event schedule_log !time (-1) Waiting;
        time := p.arrival_time;
        log_event schedule_log !time (-1) Ready;
      );
      log_event schedule_log !time p.id Running;
      p.state <- Running;
      let completion = !time + p.burst_time in
      let wait = !time - p.arrival_time in
      let turnaround = completion - p.arrival_time in
      p.waiting_time <- wait;
      p.completion_time <- Some completion;
      p.turnaround_time <- Some turnaround;
      p.remaining_burst_time <- 0;
      log_event schedule_log completion p.id Terminated;
      p.state <- Terminated;
      time := completion;
      incr completed_count;
    end else if !incoming <> [] then (
      let next_arrival = (List.hd !incoming).arrival_time in
      if next_arrival > !time then (
        log_event schedule_log !time (-1) Waiting;
        time := next_arrival;
        log_event schedule_log !time (-1) Ready;
      )
    ) else if !completed_count < num_processes then
      failwith "Erro na simulação Prioridade NP: Loop ativo sem processos prontos ou futuros."
  done;
  (!time, List.rev !schedule_log)

let priority_preemptive (processes : t list) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let completed_count = ref 0 in
  let num_processes = List.length processes in
  let incoming = ref (sort_by_arrival processes) in
  let ready_queue = ref [] in
  List.iter reset_process processes;
  let running_process = ref None in
  while !completed_count < num_processes do
    let arrived_now = List.filter (fun p -> p.arrival_time = !time) !incoming in
    if arrived_now <> [] then (
      ready_queue := List.rev_append arrived_now !ready_queue;
      incoming := List.filter (fun p -> p.arrival_time > !time) !incoming
    );
    let preempt_needed =
      match !running_process, !ready_queue with
      | Some rp, rq when rq <> [] ->
          let best_ready = List.fold_left (fun acc p -> if p.priority < acc.priority then p else acc) (List.hd rq) (List.tl rq) in
          best_ready.priority < rp.priority
      | None, rq when rq <> [] -> true
      | _ -> false
    in
    if preempt_needed then (
      (match !running_process with
      | Some rp ->
          rp.state <- Ready;
          ready_queue := rp :: !ready_queue;
          log_event schedule_log !time rp.id Ready
      | None -> ());
      let best_ready, rest =
        match !ready_queue with
        | [] -> failwith "ready_queue vazio na preempção"
        | hd :: tl ->
            List.fold_left
              (fun (best, others) p ->
                if p.priority < best.priority then (p, best :: others)
                else (best, p :: others)
              )
              (hd, [])
              tl
      in
      ready_queue := rest;
      best_ready.state <- Running;
      log_event schedule_log !time best_ready.id Running;
      running_process := Some best_ready;
    );
    (match !running_process with
    | Some rp ->
        rp.remaining_burst_time <- rp.remaining_burst_time - 1;
        if rp.remaining_burst_time = 0 then (
          let completion = !time + 1 in
          rp.state <- Terminated;
          rp.completion_time <- Some completion;
          rp.turnaround_time <- Some (completion - rp.arrival_time);
          rp.waiting_time <- completion - rp.arrival_time - rp.burst_time;
          log_event schedule_log completion rp.id Terminated;
          running_process := None;
          incr completed_count;
        )
    | None ->
        if !ready_queue = [] && !incoming <> [] then (
          log_event schedule_log !time (-1) Waiting
        )
    );
    incr time;
  done;
  (!time, List.rev !schedule_log)

let round_robin (processes : t list) ~(quantum : int) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let completed_count = ref 0 in
  let num_processes = List.length processes in
  let incoming = ref (sort_by_arrival processes) in
  let ready_queue = ref [] in
  List.iter reset_process processes;
  while !completed_count < num_processes do
    let arrived_now = List.filter (fun p -> p.arrival_time <= !time) !incoming in
    if arrived_now <> [] then (
      ready_queue := !ready_queue @ arrived_now;
      incoming := List.filter (fun p -> p.arrival_time > !time) !incoming
    );
    if !ready_queue <> [] then begin
      let p = List.hd !ready_queue in
      ready_queue := List.tl !ready_queue;
      if !time < p.arrival_time then (
        log_event schedule_log !time (-1) Waiting;
        time := p.arrival_time;
        log_event schedule_log !time (-1) Ready;
      );
      log_event schedule_log !time p.id Running;
      p.state <- Running;
      let exec_time = min quantum p.remaining_burst_time in
      p.remaining_burst_time <- p.remaining_burst_time - exec_time;
      time := !time + exec_time;
      let arrived_during = List.filter (fun p -> p.arrival_time > (!time - exec_time) && p.arrival_time <= !time) !incoming in
      if arrived_during <> [] then (
        ready_queue := !ready_queue @ arrived_during;
        incoming := List.filter (fun p -> not (List.memq p arrived_during)) !incoming
      );
      if p.remaining_burst_time = 0 then begin
        p.state <- Terminated;
        p.completion_time <- Some !time;
        p.turnaround_time <- Some (!time - p.arrival_time);
        p.waiting_time <- !time - p.arrival_time - p.burst_time;
        log_event schedule_log !time p.id Terminated;
        incr completed_count;
      end else begin
        p.state <- Ready;
        log_event schedule_log !time p.id Ready;
        ready_queue := !ready_queue @ [p];
      end
    end else if !incoming <> [] then (
      let next_arrival = (List.hd !incoming).arrival_time in
      if next_arrival > !time then (
        log_event schedule_log !time (-1) Waiting;
        time := next_arrival;
        log_event schedule_log !time (-1) Ready;
      )
    )
  done;
  (!time, List.rev !schedule_log)

let edf ?(tempo_max=max_int) (processes : t list) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let completed_count = ref 0 in
  let num_processes = List.length processes in
  let incoming = ref (sort_by_arrival processes) in
  let ready_queue = ref [] in
  List.iter reset_process processes;
  while !completed_count < num_processes && !time < tempo_max do
    let arrived_now = List.filter (fun p -> p.arrival_time <= !time) !incoming in
    if arrived_now <> [] then (
      ready_queue := !ready_queue @ arrived_now;
      incoming := List.filter (fun p -> p.arrival_time > !time) !incoming
    );
    if !ready_queue <> [] then begin
      ready_queue := List.sort (fun p1 p2 ->
        match p1.deadline, p2.deadline with
        | Some d1, Some d2 -> compare d1 d2
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0
      ) !ready_queue;
      let p = List.hd !ready_queue in
      ready_queue := List.tl !ready_queue;
      if !time < p.arrival_time then (
        log_event schedule_log !time (-1) Waiting;
        time := p.arrival_time;
        log_event schedule_log !time (-1) Ready;
      );
      log_event schedule_log !time p.id Running;
      p.state <- Running;
      let exec_time = 1 in
      p.remaining_burst_time <- p.remaining_burst_time - exec_time;
      time := !time + exec_time;
      if p.remaining_burst_time = 0 then begin
        p.state <- Terminated;
        p.completion_time <- Some !time;
        p.turnaround_time <- Some (!time - p.arrival_time);
        p.waiting_time <- !time - p.arrival_time - p.burst_time;
        log_event schedule_log !time p.id Terminated;
        incr completed_count;
      end else begin
        p.state <- Ready;
        log_event schedule_log !time p.id Ready;
        ready_queue := !ready_queue @ [p];
      end
    end else if !incoming <> [] then (
      let next_arrival = (List.hd !incoming).arrival_time in
      if next_arrival > !time then (
        log_event schedule_log !time (-1) Waiting;
        time := next_arrival;
        log_event schedule_log !time (-1) Ready;
      )
    )
  done;
  (!time, List.rev !schedule_log)

let rate_monotonic ?(tempo_max=max_int) (processes : t list) : int * timeline_event list =
  let time = ref 0 in
  let schedule_log = ref [] in
  let completed_count = ref 0 in
  let num_processes = List.length processes in
  let incoming = ref (sort_by_arrival processes) in
  let ready_queue = ref [] in
  List.iter reset_process processes;
  while !completed_count < num_processes && !time < tempo_max do
    let arrived_now = List.filter (fun p -> p.arrival_time <= !time) !incoming in
    if arrived_now <> [] then (
      ready_queue := !ready_queue @ arrived_now;
      incoming := List.filter (fun p -> p.arrival_time > !time) !incoming
    );
    if !ready_queue <> [] then begin
      ready_queue := List.sort (fun p1 p2 ->
        match p1.period, p2.period with
        | Some per1, Some per2 -> compare per1 per2
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0
      ) !ready_queue;
      let p = List.hd !ready_queue in
      ready_queue := List.tl !ready_queue;
      if !time < p.arrival_time then (
        log_event schedule_log !time (-1) Waiting;
        time := p.arrival_time;
        log_event schedule_log !time (-1) Ready;
      );
      log_event schedule_log !time p.id Running;
      p.state <- Running;
      let exec_time = 1 in
      p.remaining_burst_time <- p.remaining_burst_time - exec_time;
      time := !time + exec_time;
      if p.remaining_burst_time = 0 then begin
        p.state <- Terminated;
        p.completion_time <- Some !time;
        p.turnaround_time <- Some (!time - p.arrival_time);
        p.waiting_time <- !time - p.arrival_time - p.burst_time;
        log_event schedule_log !time p.id Terminated;
        incr completed_count;
      end else begin
        p.state <- Ready;
        log_event schedule_log !time p.id Ready;
        ready_queue := !ready_queue @ [p];
      end
    end else if !incoming <> [] then (
      let next_arrival = (List.hd !incoming).arrival_time in
      if next_arrival > !time then (
        log_event schedule_log !time (-1) Waiting;
        time := next_arrival;
        log_event schedule_log !time (-1) Ready;
      )
    )
  done;
  (!time, List.rev !schedule_log)



