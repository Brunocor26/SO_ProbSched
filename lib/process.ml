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
end