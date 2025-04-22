(* Inicializa o gerador de aleatórios com uma semente *)
let () = Random.self_init ()

(* Exponencial: usa inversa da função de distribuição acumulada (CDF) *)
let exponential lambda =
  let u = Random.float 1.0 in
  -. (log (1.0 -. u)) /. lambda

(* Poisson: usa método de Knuth *)
let poisson lambda =
  let l = exp (-.lambda) in
  let rec loop k p =
    if p <= l then k - 1
    else loop (k + 1) (p *. Random.float 1.0)
  in
  loop 0 1.0

(* Normal: método Box-Muller *)
let normal ~mu ~sigma =
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  let z0 = sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2) in
  mu +. sigma *. z0

(* Uniforme: entre min e max *)
let uniforme min max =
  min + Random.int (max - min + 1)

(* Prioridade ponderada: por exemplo, 70% chance de ser prioridade 1, 30% de ser prioridade 2 *)
let prioridade_ponderada () =
  let r = Random.float 1.0 in
  if r < 0.7 then 1 else 2