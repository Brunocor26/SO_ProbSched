(* Módulo Process *)
module Process = struct
  type process_state =
    | Ready
    | Running
    | Waiting
    | Terminated

  type process = {
    pid: int;               
    name: string;         
    arrival_time: int;     
    burst_time: int;     
    priority: int;       
    state: process_state;  
    completion_time: int;  
  }

  let create_process pid name arrival_time burst_time priority state completion_time =
    { pid; name; arrival_time; burst_time; priority; state; completion_time }

   (* === ADICIONAR ESTA FUNÇÃO === *)
   let string_of_state (s : process_state) : string =
    match s with
    | Ready -> "Ready"
    | Running -> "Running"
    | Waiting -> "Waiting"
    | Terminated -> "Terminated"
  (* === FIM DA FUNÇÃO ADICIONADA === *)
  
end