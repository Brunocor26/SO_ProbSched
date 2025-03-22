(* Módulo Process *)
module Process = struct
  type process_state =
    | Ready
    | Running
    | Waiting
    | Terminated

  type process = {
    pid: int;               (* ID do processo *)
    name: string;           (* Nome do processo *)
    arrival_time: int;      (* Tempo de chegada *)
    burst_time: int;        (* Tempo de execução (burst time) *)
    priority: int;          (* Prioridade do processo *)
    state: process_state;   (* Estado atual do processo *)
    completion_time: int;   (* Tempo de término do processo *)
  }

  (* Cria um novo processo *)
  let create_process pid name arrival_time burst_time priority state completion_time =
    { pid; name; arrival_time; burst_time; priority; state; completion_time }
end