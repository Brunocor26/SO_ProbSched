(* Mostrar menu de algoritmos e retornar escolha *)
let escolher_algoritmo () =
  Printf.printf "\nEscolha o algoritmo de escalonamento:\n";
  Printf.printf "1. FCFS (First-Come, First-Served)\n";
  Printf.printf "2. SJF (Shortest Job First)\n";
  Printf.printf "3. Priority (Non-preemptive)\n";
  Printf.printf "4. Priority (Preemptive)\n";
  Printf.printf "5. Round Robin\n";
  Printf.printf "6. Rate Monotonic (RT)\n";
  Printf.printf "7. EDF (Earliest Deadline First)\n";
  Printf.printf "Opção: ";
  let opcao = read_int () in
  match opcao with
  | 1 -> "FCFS"
  | 2 -> "SJF"
  | 3 -> "Priority (NP)"
  | 4 -> "Priority (P)"
  | 5 -> "Round Robin"
  | 6 -> "Rate Monotonic"
  | 7 -> "EDF"
  | _ -> "Inválido"

(* Função para ler um inteiro com prompt *)
let ler_int prompt =
  Printf.printf "%s: " prompt;
  read_int ()

(* Função para ler uma string com prompt *)
let ler_string prompt =
  Printf.printf "%s: " prompt;
  read_line ()

