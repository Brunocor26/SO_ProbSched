(* Implementação de Fila de Prioridade usando uma Lista Ordenada. *)

(* ('a t) é o tipo da fila de prioridade para elementos do tipo 'a *)
type 'a t = {
  mutable items : 'a list;      (* Lista ordenada (menor valor de prioridade primeiro) *)
  priority_func : 'a -> int;  (* Função que extrai o valor da prioridade de um elemento *)
}

(* Cria uma fila de prioridade vazia.
   Requer uma função que determine a prioridade (int) de um elemento ('a). *)
let create (priority_func : 'a -> int) : 'a t =
  { items = []; priority_func }

(* Verifica se a fila está vazia. *)
let is_empty (pq : 'a t) : bool =
  pq.items = [] (* Mais idiomático e ligeiramente mais eficiente que List.length pq.items = 0 *)

(* Adiciona um item à fila, mantendo a ordem de prioridade.
   Itens com menor valor numérico de prioridade ficam mais perto do início da lista.
   Complexidade: O(n), onde n é o número de items na fila, pois pode percorrer a lista. *)
let add (pq : 'a t) (item : 'a) : unit =
  let priority = pq.priority_func item in
  let rec insert_sorted current_list =
    match current_list with
    | [] -> [item] (* Se a lista está vazia, o novo item é a lista *)
    | head :: tail ->
        if priority <= pq.priority_func head then
          item :: head :: tail (* Encontrou a posição: insere antes do 'head' *)
        else
          head :: insert_sorted tail (* Continua a procurar na cauda ('tail') *)
  in
  pq.items <- insert_sorted pq.items (* Atualiza a lista interna mutável *)

(* Remove e devolve o elemento com maior prioridade (menor valor numérico).
   Lança Not_found se a fila estiver vazia.
   Complexidade: O(1), pois o elemento está sempre na cabeça da lista. *)
let take (pq : 'a t) : 'a =
  match pq.items with
  | [] -> raise Not_found (* Ou poderia devolver 'a option *)
  | head :: tail ->
      pq.items <- tail; (* Atualiza a lista removendo a cabeça *)
      head

(* Devolve (sem remover) o elemento com maior prioridade (menor valor numérico).
   Lança Not_found se a fila estiver vazia.
   Complexidade: O(1). *)
let peek (pq : 'a t) : 'a =
  match pq.items with
  | [] -> raise Not_found (* Ou poderia devolver 'a option *)
  | head :: _ -> head

(* Devolve (sem remover) o elemento com maior prioridade (menor valor numérico) como opção.
   Devolve None se a fila estiver vazia.
   Complexidade: O(1). *)
let peek_opt (pq : 'a t) : 'a option =
  match pq.items with
  | [] -> None
  | h :: _ -> Some h

(* Devolve o número de elementos na fila.
   Complexidade: O(n) para List.length. Se for chamado frequentemente,
   poderia manter-se um campo 'size' mutável para ter O(1). *)
let size (pq : 'a t) : int =
  List.length pq.items

let exists (pq : 'a t) (pred : 'a -> bool) : bool =
  List.exists pred pq.items