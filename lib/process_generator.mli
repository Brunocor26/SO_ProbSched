val prioridade_ponderada : unit -> int
val generate_processes :
  n:int ->
  arrival_lambda:float ->
  burst_mu:float -> burst_sigma:float -> (int * int * int * int) list
val generate_processes_rt :
  n:int ->
  arrival_lambda:float ->
  burst_mu:float ->
  burst_sigma:float ->
  period_mu:float -> period_sigma:float -> (int * int * int * int) list
