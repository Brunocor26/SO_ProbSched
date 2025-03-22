(* Módulo Generator - Geração de processos com distribuições probabilísticas *)
open Process

module Generator = struct
  (* Função para gerar tempos de chegada usando distribuição de Poisson *)
  let poisson lambda =
    let l = exp (-.lambda) in
    let rec aux k p =
      if p <= l then k
      else aux (k + 1) (p *. (Random.float 1.0))
    in
    aux 0 1.0

  (* Função para gerar processos dinamicamente *)
  let generate_processes num_processes lambda =
    let rec aux n processes =
      if n = 0 then processes
      else
        let arrival_time = poisson lambda in
        let burst_time = Random.int 10 + 1 in
        let priority = Random.int 5 in
        let name = Printf.sprintf "Process_%d" n in
        let completion_time = 0 in
        let process = Process.create_process n name arrival_time burst_time priority Process.Ready completion_time in
        aux (n - 1) (process :: processes)
    in
    aux num_processes []
end
