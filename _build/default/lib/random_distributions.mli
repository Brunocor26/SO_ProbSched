(** Gera um valor exponencial com parâmetro lambda *)
val exponential : float -> float

(** Gera um valor Poisson com parâmetro lambda *)
val poisson : float -> int

(** Gera um valor normal (Gaussiano) com média [mu] e desvio padrão [sigma] *)
val normal : mu:float -> sigma:float -> float

(** Gera um valor inteiro uniforme entre [min] e [max] (inclusive) *)
val uniforme : int -> int -> int

(** Gera uma prioridade ponderada (exemplo: 70% chance de ser 1, 30% de ser 2) *)
val prioridade_ponderada : unit -> int