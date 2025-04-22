type process_state = Ready | Running | Waiting | Terminated

type t = {
  id : int;   (*identificador *)
  name : string; (*nome*)
  arrival_time : int; (*tempo de chegada*)
  burst_time : int; (*tempo que demora a executar*)
  priority : int; (*nivel de prioridade (serve para alguns algoritmos)*)
  mutable remaining_burst_time : int; (*tempo de execucao que lhe resta*)
  mutable state : process_state; (*estado do processo *)
  mutable completion_time : int option; (*instante em que acaba*)
  mutable waiting_time : int; (*quanto tempo o processo ficou a espera (para estatisticas)*)
  mutable turnaround_time : int option; (*tempo desde que o processo aparece até acabar *)
  period : int option; (* para o rate monotonic (tempo real)*)
  deadline : int option; (*tempo limite onde precisa ser executado (usado no EDF e para a estatistica deadlines missed)*)
}

let string_of_state (s : process_state) : string =
  match s with
  | Ready -> "Ready"
  | Running -> "Running"
  | Waiting -> "Waiting"
  | Terminated -> "Terminated"

let create  (*~ significa argumento obrigatorio e ? significa opcional (e podes definir o valor por omissao)*)
    ~id
    ?(name = "Proc_" ^ string_of_int id)
    ~arrival_time
    ~burst_time
    ~priority
    ?(initial_state = Ready) (* Default é Ready *)
    ?period
    ?deadline
    () =
  {
    id;
    name;
    arrival_time;
    burst_time;
    priority;
    state = initial_state;
    remaining_burst_time = burst_time;
    completion_time = None;
    waiting_time = 0;
    turnaround_time = None;
    period;
    deadline;
  }

(* Funções auxiliares *)

(*vê se o processo ja terminou*)
let is_terminated (p : t) : bool =
  p.state = Terminated

(*devole o turnaround time do processo*)
let get_turnaround_time (p : t) : int option =
  match p.completion_time with
  | None -> None (*se ainda nao acabou, None*)
  | Some ct -> Some (ct - p.arrival_time) (* quando há um valor no completion time, faz esse valor - tempo de chegada*)

(* Converte um processo para uma representação de string legível *)
let string_of_process (p : t) : string =
  let optional_to_string prefix = function
    | None -> prefix ^ ": N/A"
    | Some value -> prefix ^ ": " ^ string_of_int value
  in
  
  let state_str = string_of_state p.state in
  
  Printf.sprintf
    "Process %d (%s)\n\
     State: %s\n\
     Arrival time: %d\n\
     Burst time: %d / %d (remaining / total)\n\
     Priority: %d\n\
     Waiting time: %d\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\n"
    p.id
    p.name
    state_str
    p.arrival_time
    p.remaining_burst_time p.burst_time
    p.priority
    p.waiting_time
    (optional_to_string "Completion time" p.completion_time)
    (optional_to_string "Turnaround time" p.turnaround_time)
    (optional_to_string "Period" p.period)
    (optional_to_string "Deadline" p.deadline)