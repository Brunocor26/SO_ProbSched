val prioridade_ponderada : unit -> int
val default_arrival_lambda : float
val default_burst_mu : float
val default_burst_sigma : float
val default_period_mu : float
val default_period_sigma : float
val generate_processes : int -> (int * int * int * int) list
val generate_processes_rt : int -> (int * int * int * int) list
