(* Importar os módulos *)
open Prob_sched
open Process
open Help

(* Referência mutável para gerar pids únicos *)
let next_pid = ref 0

(* Função para gerar um novo PID *)
let generate_pid () =
  let pid = !next_pid in
  next_pid := !next_pid + 1;
  pid

  let () =
  Printf.printf "--- Criar Processo Manualmente ---\n";
  let nome = ler_string "Nome do processo" in
  let arrival_time = ler_int "Tempo de chegada" in
  let burst_time = ler_int "Burst time (tempo de execução)" in
  let priority = ler_int "Prioridade (quanto menor, maior prioridade)" in
  let state = Process.Ready in
  let completion_time = 0 in

  let pid = generate_pid () in
  let processo = Process.create_process pid nome arrival_time burst_time priority state completion_time in

  Printf.printf "\n--- Processo Criado ---\n";
  Printf.printf "PID: %d\n" processo.pid;
  Printf.printf "Nome: %s\n" processo.name;
  Printf.printf "Arrival: %d\n" processo.arrival_time;
  Printf.printf "Burst: %d\n" processo.burst_time;
  Printf.printf "Priority: %d\n" processo.priority;
  Printf.printf "Completion: %d\n" processo.completion_time;

  let algoritmo = escolher_algoritmo () in
  Printf.printf "\nAlgoritmo escolhido: %s\n" algoritmo

  