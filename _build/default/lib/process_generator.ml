(* receber do user um numero de processos (tamanho da lista)*)
(* com as funcoes do random_distributions fazer uma lista de processos *)

open Random_distributions

let prioridade_ponderada () =
  let r = Random.float 1.0 in
  if r < 0.4 then 1
  else if r < 0.7 then 2
  else if r < 0.9 then 3
  else 4

  (* Parâmetros fixos *)
let default_arrival_lambda = 1.0
let default_burst_mu = 5.0
let default_burst_sigma = 2.0

(* real time*)
let default_period_mu = 8.0
let default_period_sigma = 3.0


let generate_processes n =
  let rec gen acc i last_arrival =
    if i > n then List.rev acc
    else
      let inter_arrival = exponential default_arrival_lambda in
      let arrival_time = last_arrival +. inter_arrival in
      let burst_time_f = normal ~mu:default_burst_mu ~sigma:default_burst_sigma in
      let burst_time = max 1 (int_of_float (abs_float burst_time_f)) in
      let priority = prioridade_ponderada () in
      let proc_tuple =
        (i, int_of_float arrival_time, burst_time, priority)
      in
      gen (proc_tuple :: acc) (i + 1) arrival_time
  in
  gen [] 1 0.0


  let generate_processes_rt n =
    let rec gen acc i last_arrival =
      if i > n then List.rev acc
      else
        let inter_arrival = exponential default_arrival_lambda in
        let arrival_time = last_arrival +. inter_arrival in
        let burst_time_f = normal ~mu:default_burst_mu ~sigma:default_burst_sigma in
        let burst_time = max 1 (int_of_float (abs_float burst_time_f)) in
        (* Período realista-> sempre maior que burst_time *)
        let min_period = burst_time + 1 in
        let period_f = normal ~mu:default_period_mu ~sigma:default_period_sigma in
        let period = max min_period (int_of_float (abs_float period_f)) in
        let proc_tuple =
          (i, int_of_float arrival_time, burst_time, period)
        in
        gen (proc_tuple :: acc) (i + 1) arrival_time
    in
    gen [] 1 0.0


    (* Parâmetros fixos *)
let default_arrival_lambda = 1.0
let default_burst_lambda = 0.25  (* Valor baixo para bursts curtos serem mais prováveis *)
let default_period_mu = 8.0
let default_period_sigma = 3.0

let generate_processes_exp n =
  let rec gen acc i last_arrival =
    if i > n then List.rev acc
    else
      let inter_arrival = exponential default_arrival_lambda in
      let arrival_time = last_arrival +. inter_arrival in
      let burst_time_f = exponential default_burst_lambda in
      let burst_time = max 1 (int_of_float (ceil burst_time_f)) in
      let priority = prioridade_ponderada () in
      let proc_tuple =
        (i, int_of_float arrival_time, burst_time, priority)
      in
      gen (proc_tuple :: acc) (i + 1) arrival_time
  in
  gen [] 1 0.0

let generate_processes_rt_exp n =
  let rec gen acc i last_arrival =
    if i > n then List.rev acc
    else
      let inter_arrival = exponential default_arrival_lambda in
      let arrival_time = last_arrival +. inter_arrival in
      let burst_time_f = exponential default_burst_lambda in
      let burst_time = max 1 (int_of_float (ceil burst_time_f)) in
      (* Período realista-> sempre maior que burst_time *)
      let min_period = burst_time + 1 in
      let period_f = normal ~mu:default_period_mu ~sigma:default_period_sigma in
      let period = max min_period (int_of_float (abs_float period_f)) in
      let proc_tuple =
        (i, int_of_float arrival_time, burst_time, period)
      in
      gen (proc_tuple :: acc) (i + 1) arrival_time
  in
  gen [] 1 0.0