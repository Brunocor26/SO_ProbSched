open Prob_sched (* biblioteca com os modulos criados *)

(* --- Módulos --- *)
open Tk (* labltk pra fazer a janela *)
open Process

(* --- Configuração Inicial --- *)

let next_pid = ref 0 (* variavel global pra gerar pids unicos, começa em 0 *)
let generate_pid () =
  let pid = !next_pid in (* le o valor atual *)
  next_pid := !next_pid + 1; (* incrementa para o proximo *)
  pid (* retorna o pid gerado *)

(* --- Elementos da GUI --- *)

let top = openTk () (* cria a janela principal *)
let () = Wm.title_set top "Criador de Processo e Seletor de Algoritmo" (* titulo da janela *)

(* Variáveis (OCaml refs) para armazenar dados e estado da GUI *)
let chosen_algorithm_name = ref "Nenhum" (* nome do algo escolhido *)
let display_process_details = ref "Nenhum processo criado ainda." (* texto pra mostrar detalhes do processo *)
let display_chosen_algorithm = ref "Algoritmo: Nenhum selecionado" (* texto pra mostrar o algo *)

(* --- Widgets da Interface --- *)

(* Frame para Inputs do Processo *)
let input_frame = Frame.create top ~borderwidth:2 ~relief:`Groove (* caixa pros inputs *)
let () = pack [coe input_frame] ~pady:5 ~padx:5 ~fill:`X (* meter a caixa na janela *)

(* Nome do Processo - Row 0 *)
let name_label = Label.create input_frame ~text:"Nome do processo:" (* texto fixo *)
let name_entry = Entry.create input_frame (* caixa de texto pra escrever o nome *)
let () = grid [coe name_label] ~row:0 ~column:0 ~sticky:"w" ~padx:2 ~pady:2 (* arrumar na grelha *)
let () = grid [coe name_entry] ~row:0 ~column:1 ~sticky:"we" ~padx:2 ~pady:2 (* caixa de texto ao lado *)

(* Tempo de Chegada - Row 1 *)
let arrival_label = Label.create input_frame ~text:"Tempo de chegada:"
let arrival_entry = Entry.create input_frame
let () = grid [coe arrival_label] ~row:1 ~column:0 ~sticky:"w" ~padx:2 ~pady:2
let () = grid [coe arrival_entry] ~row:1 ~column:1 ~sticky:"we" ~padx:2 ~pady:2

(* Burst Time - Row 2 *)
let burst_label = Label.create input_frame ~text:"Burst time (execução):"
let burst_entry = Entry.create input_frame
let () = grid [coe burst_label] ~row:2 ~column:0 ~sticky:"w" ~padx:2 ~pady:2
let () = grid [coe burst_entry] ~row:2 ~column:1 ~sticky:"we" ~padx:2 ~pady:2

(* Prioridade - Row 3 *)
let priority_label = Label.create input_frame ~text:"Prioridade (menor=maior):"
let priority_entry = Entry.create input_frame
let () = grid [coe priority_label] ~row:3 ~column:0 ~sticky:"w" ~padx:2 ~pady:2
let () = grid [coe priority_entry] ~row:3 ~column:1 ~sticky:"we" ~padx:2 ~pady:2

(* Configura a coluna 1 para expandir *)
(* isto faz com que as caixas de texto estiquem se a janela aumentar *)
let () = Grid.column_configure input_frame 1 ~weight:1

(* Frame para Exibir Detalhes do Processo Criado *)
let display_frame = Frame.create top ~borderwidth:2 ~relief:`Groove (* outra caixa, pra mostrar o resultado*)
let display_label = Label.create display_frame ~text:!display_process_details ~justify:`Left (* onde o texto aparece *)
let () = pack [coe display_frame] ~pady:5 ~padx:5 ~fill:`X (* meter na janela *)
let () = pack [coe display_label] ~padx:5 ~pady:5 ~anchor:`W (* texto alinhado à esquerda *)

(* Frame para Seleção de Algoritmo *)
let algo_frame = Frame.create top ~borderwidth:2 ~relief:`Groove (* caixa pro algoritmo *)
let () = pack [coe algo_frame] ~pady:5 ~padx:5 ~fill:`X

(* Label para mostrar o algoritmo escolhido *)
let chosen_algo_label = Label.create algo_frame ~text:!display_chosen_algorithm ~justify:`Left
let () = pack [coe chosen_algo_label] ~side:`Bottom ~pady:5 ~padx:5 ~anchor:`W (* fica em baixo nesta frame *)

(* Menu Button para escolher o algoritmo *)
let algo_menubutton = Menubutton.create algo_frame ~text:"Escolher Algoritmo" ~relief:`Raised (* o botao q abre o menu *)
let algo_menu = Menu.create algo_menubutton (* o menu em si, ainda vazio *)

(* Função callback quando um algoritmo é selecionado *)
(* isto corre quando escolhes uma opção no menu *)
let set_algorithm algo_name =
  chosen_algorithm_name := algo_name; (* guarda o nome *)
  display_chosen_algorithm := "Algoritmo: " ^ algo_name; (* prepara o texto pra mostrar *)
  Label.configure chosen_algo_label ~text:!display_chosen_algorithm; (* atualiza o label na janela *)
  Printf.printf "Debug: Algoritmo '%s' selecionado.\n%!" algo_name (* mensagem pra consola *)

(* Adiciona as opções de algoritmo ao menu *)
let algorithms = ["FIFO"; "SJF"; "Prioridade"; "Round Robin"] (* lista de algos *)
let () =
  List.iter (fun algo -> (* para cada algo na lista... *)
    Menu.add_command algo_menu ~label:algo ~command:(fun () -> set_algorithm algo) (* ...adiciona uma entrada no menu que chama set_algorithm *)
  ) algorithms
let () = Menubutton.configure algo_menubutton ~menu:algo_menu (* associa o menu ao botao *)
let () = pack [coe algo_menubutton] ~pady:5 ~padx:5 ~anchor:`W (* mete o botao na frame *)

(* --- Botão para Criar o Processo --- *)

(* Função callback para o botão "Criar Processo" *)
(* isto corre quando clicas no botao 'Criar Processo' *)
let create_process_action () =
  try (* tentar fazer isto tudo, pode dar erro *)
    (* 1. Obter dados das caixas de texto *)
    let name = Entry.get name_entry in
    let arrival_str = Entry.get arrival_entry in
    let burst_str = Entry.get burst_entry in
    let priority_str = Entry.get priority_entry in

    (* 2. Validar e Converter para numeros *)
    if name = "" then raise (Failure "Nome do processo não pode estar vazio."); (* verifica nome *)
    let arrival_time = try int_of_string arrival_str with Failure _ -> raise (Failure "Tempo de chegada inválido.") in (* converte chegada *)
    let burst_time = try int_of_string burst_str with Failure _ -> raise (Failure "Burst time inválido.") in (* converte burst *)
    let priority = try int_of_string priority_str with Failure _ -> raise (Failure "Prioridade inválida.") in (* converte prio *)
    (* mais umas verificações *)
    if arrival_time < 0 then raise (Failure "Tempo de chegada não pode ser negativo.");
    if burst_time <= 0 then raise (Failure "Burst time deve ser positivo.");
    if priority < 0 then raise (Failure "Prioridade não pode ser negativa.");

    (* 3. Definir estado inicial e PID *)
    (* Usa 'Ready' diretamente, pois 'open Process' expôs 'process_state' *)
    (* !! Atenção !! A sintaxe aqui com 'and' está errada para lets separados *)
    let state = Process.Ready (* aqui devia ser só Ready? *)
    and completion_time = 0
    and pid = generate_pid () in

    (* 4. Criar o processo *)
    (* Usa 'create_process' diretamente, pois 'open Process' expôs a função *)
    (* A variável 'processo' terá o tipo Process.process implicitamente *)
     (* aqui tb devia ser só create_process, nao? *)
    let processo = Process.create_process pid name arrival_time burst_time priority state completion_time in

    (* 5. Formatar detalhes para mostrar *)
    let details = Printf.sprintf
      "--- Processo Criado ---\nPID: %d\nNome: %s\nArrival: %d\nBurst: %d\nPriority: %d\nCompletion: %d\nState: %s"
      processo.pid
      processo.name
      processo.arrival_time
      processo.burst_time
      processo.priority
      processo.completion_time
      (* Usa 'string_of_state' diretamente, pois 'open Process' expôs a função *)
      (Process.string_of_state processo.state) (* e aqui tb so string_of_state *)
    in
    display_process_details := details; (* guardar o texto formatado *)

    (* 6. Atualizar a Label na janela *)
    Label.configure display_label ~text:!display_process_details;

    (* 7. limpar os campos de entrada - opcional mas util *)
    (* Entry.delete name_entry ~first:0 ~last:`End;
       Entry.delete arrival_entry ~first:0 ~last:`End;
       Entry.delete burst_entry ~first:0 ~last:`End;
       Entry.delete priority_entry ~first:0 ~last:`End;
       Entry.icursor_set name_entry ~index:0; *)


    Printf.printf "Processo criado via GUI: PID %d Nome: %s\n%!" pid name (* msg pra consola *)

  with
  | Failure msg -> (* se alguma coisa correu mal no 'try'... *)
      let error_msg = Printf.sprintf "Erro: %s" msg in (* formata msg de erro *)
      display_process_details := error_msg; (* guarda a msg de erro *)
      Label.configure display_label ~text:!display_process_details; (* mostra o erro na janela *)
      Printf.eprintf "Erro ao criar processo: %s\n%!" msg (* manda erro pra consola *)
      (* Podia mostrar uma popup de erro, tipo isto: *)
      (* Dialog.message top ~title:"Erro na Criação do Processo" ~message:msg ~icon:`Error ~typ:`Ok *)


(* Cria o botão principal *)
let create_button = Button.create top ~text:"Criar Processo" ~command:create_process_action (* associa a função ao clique *)
let () = pack [coe create_button] ~pady:10 (* mete o botao na janela *)

(* --- Loop Principal da GUI --- *)
let () =
  try (* tentar correr a janela *)
    print_endline "A iniciar a interface gráfica...";
    mainLoop () (* !! isto é que faz a janela aparecer e funcionar !! *)
  with e -> (* se até o mainLoop der erro... *)
    Printf.eprintf "Erro fatal na aplicação GUI: %s\n" (Printexc.to_string e); (* mostra erro critico *)
    exit 1 (* e fecha o programa *)