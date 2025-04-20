(* priority_queue.ml *)

(* Implementação baseada em lista ordenada por prioridade. *)

type 'a t = {
  mutable items : 'a list;        (* Lista ordenada de elementos *)
  priority_func : 'a -> int;      (* Função para obter o valor da prioridade *)
}

(* Cria uma fila de prioridade vazia com a função de prioridade *)
let create priority_func = { items = []; priority_func }

(* Verifica se a fila está vazia *)
let is_empty pq = List.length pq.items = 0

(* Insere um elemento mantendo a ordem (menor prioridade primeiro) *)
let add pq item =
  let priority = pq.priority_func item in
  let rec insert acc lst =
    match lst with
    | [] -> List.rev (item :: acc)  (* Caso base: inserir no final *)
    | h :: t ->
        if priority <= pq.priority_func h then 
          List.rev_append acc (item :: h :: t)  (* elemento a ser mudado (menor prioridade) *)
        else 
          insert (h :: acc) t  (* Continua a procurar *)
  in
  pq.items <- insert [] pq.items

(* Remove e devolve o elemento de maior prioridade (menor valor) 
   Lança Not_found se a fila estiver vazia *)
let take pq =
  match pq.items with
  | [] -> raise Not_found
  | h :: t ->
      pq.items <- t;
      h

(* devolve o elemento de maior prioridade sem removê-lo
    Lança Not_found se a fila estiver vazia *)
let peek pq =
  match pq.items with [] -> raise Not_found | h :: _ -> h

(* devolve o número de elementos na fila *)
let size pq = List.length pq.items