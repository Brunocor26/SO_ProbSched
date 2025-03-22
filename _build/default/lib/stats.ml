(* Módulo Stats *)
open Process
module Stats = struct
  open Process

  (* Função para calcular o tempo de espera *)
  let calculate_waiting_time processes =

    let rec aux acc = function
      | [] -> acc
      | p :: ps ->
          match p.state with
          | Terminated ->
              (* Calcula o tempo de espera para processos terminados *)
              let waiting_time = (p.completion_time - p.arrival_time) - p.burst_time in
              aux (acc + waiting_time) ps
          | _ -> aux acc ps  (* Ignora processos que não terminaram *)
    in
    aux 0 processes

  (* Função para calcular o tempo médio de espera *)
  let calculate_average_waiting_time processes =
    let total_waiting_time = calculate_waiting_time processes in
    let num_terminated_processes = List.length (List.filter (fun p -> p.state = Terminated) processes) in
    float_of_int total_waiting_time /. float_of_int num_terminated_processes
end