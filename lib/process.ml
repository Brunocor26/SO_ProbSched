
  type process_state =
    | Ready
    | Running
    | Waiting
    | Terminated

  type process = {
    pid: int;
    name: string;
    arrival_time: float;
    burst_time: float;
    priority: int;
    state: process_state;
    mutable completion_time: float option;
    remaining_time: float;
  }

  let create_process pid name arrival_time burst_time priority state =
    { pid; name; arrival_time; burst_time; priority; state; completion_time = None; remaining_time=burst_time }

  let string_of_state (s : process_state) : string =
    match s with
    | Ready -> "Ready"
    | Running -> "Running"
    | Waiting -> "Waiting"
    | Terminated -> "Terminated"
