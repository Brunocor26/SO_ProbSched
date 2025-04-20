(* Funções auxiliares para leitura e validação de inputs *)

(*converter numero para o algoritmo a ser usado*)

let escolha_algoritmo n =
  match n with
  |1-> "fcfs"
  |2-> "sj"
  |3-> "priority"
  |4-> "rr"
  |5-> "rm"
  |6-> "edf"
  |_-> "POR IMPLEMENTAR"

(* Funções para leitura de strings *)

(* Lê uma string com um prompt *)
let read_string prompt =
  print_string prompt;
  flush stdout;
  read_line ()

(* Lê uma string não vazia com um prompt *)
let rec read_non_empty_string prompt =
  let input = read_string prompt in
  if String.trim input = "" then (
    print_endline "Entrada inválida. Não pode ser vazia.";
    read_non_empty_string prompt
  ) else
    String.trim input

(* Funções para leitura de inteiros *)

(* Tenta converter uma string para inteiro *)
let parse_int s =
  try Some (int_of_string (String.trim s))
  with Failure _ -> None

(* Lê um inteiro com um prompt *)
let rec read_int prompt =
  let input = read_string prompt in
  match parse_int input with
  | Some n -> n
  | None ->
      print_endline "Entrada inválida. Por favor, insira um número inteiro.";
      read_int prompt

(* Lê um inteiro com um prompt e validação de intervalo *)
let rec read_int_range prompt min max =
  let n = read_int prompt in
  if n >= min && n <= max then n
  else (
    Printf.printf "Entrada inválida. Por favor, insira um número entre %d e %d.\n" min max;
    read_int_range prompt min max
  )

(* Funções para leitura de floats *)

(* Tenta converter uma string para float *)
let parse_float s =
  try Some (float_of_string (String.trim s))
  with Failure _ -> None

(* Lê um float com um prompt *)
let rec read_float prompt =
  let input = read_string prompt in
  match parse_float input with
  | Some x -> x
  | None ->
      print_endline "Entrada inválida. Por favor, insira um número.";
      read_float prompt

(* Lê um float com um prompt e validação de intervalo *)
let rec read_float_range prompt min max =
  let x = read_float prompt in
  if x >= min && x <= max then x
  else (
    Printf.printf "Entrada inválida. Por favor, insira um número entre %g e %g.\n" min max;
    read_float_range prompt min max
  )

(* Funções para ler dados de processos de um ficheiro. *)

(*
 * processa_linha_dados: Tenta extrair os dados (id, inicio, burst, prioridade) de uma linha.
 * Argumentos:
 * linha_num: int - O número da linha atual (para mensagens de erro).
 * str_linha: string - O conteúdo da linha.
 * Retorna:
 * (int * int * int * int) option - Uma tupla com os 4 inteiros se sucesso, None se erro.
 * Formato esperado da linha: "id,arrival_time,burst_time,priority"
 *)
 let processa_linha_dados (linha_num : int) (str_linha : string) : (int * int * int * int) option =
  try
    match String.split_on_char ',' str_linha with
    | [id_str; inicio_str; burst_str; priority_str] ->
        let id_val = int_of_string (String.trim id_str) in
        let inicio_val = int_of_string (String.trim inicio_str) in
        let burst_val = int_of_string (String.trim burst_str) in
        let priority_val = int_of_string (String.trim priority_str) in

        (* Retorna a tupla com os dados extraídos, dentro de Some *)
        Some (id_val, inicio_val, burst_val, priority_val)

    | _ ->
        Printf.eprintf
          "Aviso: Linha %d ignorada - formato inválido (esperava 4 campos: id,inicio,burst,prioridade): %s\n"
          linha_num str_linha;
        None
  with
  | Failure msg -> (* Erro na conversão int_of_string *)
      Printf.eprintf
        "Aviso: Linha %d ignorada - erro na conversão de número (%s): %s\n"
        linha_num msg str_linha;
      None
  | ex -> (* Outros erros *)
      Printf.eprintf
        "Aviso: Linha %d ignorada - erro inesperado (%s): %s\n"
        linha_num (Printexc.to_string ex) str_linha;
      None

(*
 * ler_ficheiro_dados_processos: Lê um ficheiro linha a linha e retorna uma lista
 * de tuplas, cada uma contendo os dados (id, inicio, burst, prioridade) de uma linha válida.
 * Argumentos:
 * nome_ficheiro: string - O caminho para o ficheiro a ser lido.
 * Retorna:
 * (int * int * int * int) list - Lista de tuplas com os dados lidos.
 * Retorna lista vazia se o ficheiro não puder ser aberto ou não contiver linhas válidas.
 *)
let ler_ficheiro_dados_processos (nome_ficheiro : string) : (int * int * int * int) list =
  let lista_dados = ref [] in (* Acumulador para as tuplas de dados *)
  let linha_num = ref 0 in
  try
    let ic = open_in nome_ficheiro in
    try
      while true do
        let linha = input_line ic in
        incr linha_num;
        (* Chama a função que processa a linha e retorna a tupla de dados ou None *)
        match processa_linha_dados !linha_num linha with
        | Some dados_tupla -> lista_dados := dados_tupla :: !lista_dados (* Adiciona a tupla à lista *)
        | None -> () (* Ignora linhas inválidas *)
      done;
      !lista_dados (* Unreachable code *)
    with End_of_file ->
      close_in ic;
      List.rev !lista_dados (* Inverte para manter a ordem original *)
  with Sys_error msg ->
    Printf.eprintf "Erro ao abrir ou ler o ficheiro '%s': %s\n" nome_ficheiro msg;
    [] (* Retorna lista vazia em caso de erro *)

    (*
 * processa_linha_dados_rt: Tenta extrair os dados (id, inicio, burst, periodo) de uma linha.
 * Argumentos:
 * linha_num: int - O número da linha atual (para mensagens de erro).
 * str_linha: string - O conteúdo da linha.
 * Retorna:
 * (int * int * int * int) option - Uma tupla com id, inicio, burst, periodo se sucesso, None se erro.
 * Formato esperado da linha: "id,arrival_time,burst_time,period"
 *)
let processa_linha_dados_rt (linha_num : int) (str_linha : string) : (int * int * int * int) option =
  try
    match String.split_on_char ',' str_linha with
    (* Atualizado para refletir que o 4º campo é periodo_str *)
    | [id_str; inicio_str; burst_str; periodo_str] ->
        let id_val = int_of_string (String.trim id_str) in
        let inicio_val = int_of_string (String.trim inicio_str) in
        let burst_val = int_of_string (String.trim burst_str) in
        (* Converte o quarto campo como periodo_val *)
        let periodo_val = int_of_string (String.trim periodo_str) in

        (* Retorna a tupla com os dados extraídos (incluindo o período), dentro de Some *)
        Some (id_val, inicio_val, burst_val, periodo_val)

    | _ ->
        Printf.eprintf
          (* Mensagem de erro atualizada para mencionar 'periodo' *)
          "Aviso: Linha %d ignorada - formato inválido (esperava 4 campos: id,inicio,burst,periodo): %s\n"
          linha_num str_linha;
        None
  with
  | Failure msg -> (* Erro na conversão int_of_string *)
      Printf.eprintf
        "Aviso: Linha %d ignorada - erro na conversão de número (%s): %s\n"
        linha_num msg str_linha;
      None
  | ex -> (* Outros erros *)
      Printf.eprintf
        "Aviso: Linha %d ignorada - erro inesperado (%s): %s\n"
        linha_num (Printexc.to_string ex) str_linha;
      None

(*
 * ler_ficheiro_dados_processos_rt: Lê um ficheiro linha a linha e retorna uma lista
 * de tuplas, cada uma contendo os dados (id, inicio, burst, periodo) de uma linha válida.
 * Argumentos:
 * nome_ficheiro: string - O caminho para o ficheiro a ser lido.
 * Retorna:
 * (int * int * int * int) list - Lista de tuplas com os dados (id, inicio, burst, periodo).
 * Retorna lista vazia se o ficheiro não puder ser aberto ou não contiver linhas válidas.
 *)
let ler_ficheiro_dados_processos_rt (nome_ficheiro : string) : (int * int * int * int) list =
  let lista_dados = ref [] in (* Acumulador para as tuplas de dados *)
  let linha_num = ref 0 in
  try
    let ic = open_in nome_ficheiro in
    try
      while true do
        let linha = input_line ic in
        incr linha_num;
        (* Chama a versão _rt da função que processa a linha *)
        match processa_linha_dados_rt !linha_num linha with
        | Some dados_tupla -> lista_dados := dados_tupla :: !lista_dados (* Adiciona a tupla à lista *)
        | None -> () (* Ignora linhas inválidas *)
      done;
      !lista_dados (* Unreachable code *)
    with End_of_file ->
      close_in ic;
      List.rev !lista_dados (* Inverte para manter a ordem original *)
  with Sys_error msg ->
    Printf.eprintf "Erro ao abrir ou ler o ficheiro '%s': %s\n" nome_ficheiro msg;
    [] (* Retorna lista vazia em caso de erro *)