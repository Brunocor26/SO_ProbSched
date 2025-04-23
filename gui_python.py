import tkinter as tk
# botões mais bonitos (ttk), caixas de diálogo (filedialog, messagebox) e texto com scroll (scrolledtext).
from tkinter import ttk, filedialog, messagebox, scrolledtext

# Importar bibliotecas para: chamar o programa OCaml (subprocess),
# ler a resposta em JSON (json) e mexer com ficheiros/paths (os).
import subprocess
import json
import os
# Importar a biblioteca para ler ficheiros CSV.
import csv
# Importar biblioteca para gráficos de Gantt
import matplotlib.pyplot as plt

# caminho para o programa gerado pelo Dune
OCAML_PATH = "./_build/default/bin/prob_sched.exe"


# --- Funções que fazem a janela funcionar ---

# Função para carregar e mostrar o conteúdo do CSV na tabela
def load_csv_to_table(filepath):
    # Primeiro, limpar a tabela de dados anteriores
    # Percorre todos os items ('children') da raiz da árvore e apaga-os
    for item in process_table.get_children():
        process_table.delete(item)

    try:
        # Abre o ficheiro CSV para leitura ('r'), especificando a codificação (importante!)
        # newline='' é recomendado pela documentação do módulo csv
        with open(filepath, mode='r', newline='', encoding='utf-8') as csvfile:
            # Cria um leitor de CSV, assumindo que o delimitador é vírgula ','
            # (Se o teu CSV usar ; ou outro, muda aqui: delimiter=';')
            csv_reader = csv.reader(csvfile, delimiter=',')

            # Lê a primeira linha para ver se são cabeçalhos
            header = next(csv_reader, None)

            # Valida se o cabeçalho parece correto (opcional, mas útil)
            # Adapta estes nomes se as colunas no teu CSV forem diferentes
            expected_headers = ["id", "start_time", "burst_time", "priority"] # ou "period" etc.
            if not header or len(header) < 4: # Verifica se tem pelo menos 4 colunas
                 messagebox.showwarning("Aviso CSV", "Ficheiro CSV parece vazio ou tem poucas colunas.")
                 return # Não continua se o cabeçalho for inválido

            # Se a primeira linha não parecer dados (ex: contém letras onde deviam ser números),
            # assume que é um cabeçalho e avança. Caso contrário, processa-a como dados.
            # Esta lógica pode precisar de ajustes dependendo do formato exato do CSV.
            try:
                # Tenta converter a segunda coluna (start_time) para inteiro. Se falhar, é provável que seja cabeçalho.
                int(header[1])
                # Se chegou aqui, a primeira linha parece ser dados, então insere-a
                process_table.insert('', tk.END, values=header[:4]) # Insere as primeiras 4 colunas
            except (ValueError, IndexError):
                # Provavelmente era um cabeçalho, ignora esta linha (já foi lida com next())
                pass # Ignora o cabeçalho

            # Lê e insere as restantes linhas na tabela
            row_count = 0
            for row in csv_reader:
                if len(row) >= 4: # Garante que a linha tem dados suficientes
                    # Insere um novo item na tabela:
                    # '' - item pai (raiz)
                    # tk.END - inserir no fim
                    # values - uma lista/tuplo com os valores para cada coluna (pegamos nos 4 primeiros)
                    process_table.insert('', tk.END, values=row[:4])
                    row_count += 1
            status_var.set(f"CSV '{os.path.basename(filepath)}' carregado ({row_count} processos).")

    except FileNotFoundError:
        messagebox.showerror("Erro Ficheiro", f"Não foi possível encontrar o ficheiro:\n{filepath}")
        status_var.set("Erro ao ler CSV: Ficheiro não encontrado.")
    except Exception as e:
        messagebox.showerror("Erro CSV", f"Erro ao ler ou processar o ficheiro CSV:\n{e}")
        status_var.set(f"Erro ao ler CSV: {e}")


# Função para o botão 'Procurar...' para escolher ficheiro
def browse_file():
    # Mostra todos os ficheiros (*.*), ou so ficheiros csv (comma separated values) (.csv)
    filepath = filedialog.askopenfilename(
        title="Selecionar Ficheiro de Processos CSV",
        filetypes=(("Ficheiros CSV", "*.csv"), ("Todos os ficheiros", "*.*")) # Dá prioridade aos CSV
    )
    # Se o utilizador escolheu mesmo um ficheiro...
    if filepath:
        # Guarda o caminho do ficheiro para mostrar na janela.
        file_path_var.set(filepath)
        # AGORA: Chama a função para carregar os dados do CSV para a tabela
        load_csv_to_table(filepath)
        # Atualiza o estado do botão 'Executar Simulação'
        update_run_button_state()

# Função quando se carrega no botão 'Executar Simulação'
def run_simulation():
    algo = algo_combo.get()
    fpath = file_path_var.get()
    quantum = quantum_spinbox.get()
    max_time = max_time_spinbox.get()
    gen_num = gen_num_var.get()

    if not os.path.exists(OCAML_PATH):
        messagebox.showerror("Erro", f"Não encontrei o executável OCaml em:\n{OCAML_PATH}\nVerifique o caminho.")
        return

    # Antes de correr a simulação:
    if gen_num > 0:
        show_generated_processes([])  # Limpa a tabela só para geração aleatória
    clear_results()  # Limpa só os resultados da simulação (estatísticas, timeline, etc.)

    # Decide se usa ficheiro ou geração aleatória
    if gen_num > 0:
        cmd = [OCAML_PATH, "--algo", algo, "--gen", str(gen_num)]
    else:
        if not fpath or fpath == "No file selected":
            messagebox.showerror("Erro", "Por favor, escolha um ficheiro de processos ou indique um número para gerar aleatórios.")
            return
        cmd = [OCAML_PATH, "--algo", algo, "--file", fpath]

    if algo == "rr":
        cmd.extend(["--quantum", str(quantum)])
    if algo in ("rm", "edf"):
        cmd.extend(["--max", str(max_time)])

    # Escreve na barra de estado em baixo o que esta a fazer
    status_var.set(f"A executar {algo} em {os.path.basename(fpath)}...")
    # Força a janela a atualizar
    root.update_idletasks()

    # Tenta correr o programa OCaml e ver o que acontece
    try:
        # Chama o programa OCaml, guarda o que ele escrever (stdout/stderr),
        # diz que a saída é texto, não verifica o código de erro automaticamente (check=False),
        # e espera no máximo 30 segundos.
        process = subprocess.run(cmd, capture_output=True, text=True, check=False, timeout=30)

        # Limpa os resultados que possam estar na janela da vez anterior.
        clear_results() # Esta função agora só limpa os resultados da simulação, não a tabela CSV

        # Se o programa OCaml deu erro (código de retorno diferente de 0)...
        if process.returncode != 0:
            # Mesmo com erro, tenta ver se a saída (stdout) é um JSON
            # (pode ter a mensagem de erro lá dentro formatada em JSON)
            try:
                output_data = json.loads(process.stdout)
                # Se o JSON diz explicitamente que não teve sucesso E tem uma mensagem de erro...
                if not output_data.get("success", False) and "error" in output_data:
                    # Mostra a mensagem de erro que veio do OCaml.
                    messagebox.showerror("Erro da Simulação", f"Erro do OCaml:\n{output_data['error']}")
                    status_var.set(f"Erro OCaml: {output_data['error']}") # Mensagem mais curta para status
                else:
                    # Se deu erro mas não veio mensagem de erro clara no JSON...
                    # Mostra um erro mais genérico, incluindo o que o OCaml escreveu (stderr e stdout).
                    messagebox.showerror("Erro na Execução", f"Processo OCaml falhou (código {process.returncode}).\nStderr:\n{process.stderr}\nStdout:\n{process.stdout}")
                    status_var.set(f"Execução falhou (código {process.returncode})")
            # Se deu erro E a saída nem sequer é JSON...
            except json.JSONDecodeError:
                 # Mostra mensagem a dizer que deu erro e a saída é esquisita (não JSON).
                 messagebox.showerror("Erro na Execução", f"Processo OCaml falhou (código {process.returncode}) e a saída não é JSON válido.\nStderr:\n{process.stderr}\nStdout:\n{process.stdout}")
                 status_var.set(f"Execução falhou (código {process.returncode}), saída inválida.")

        # Se correu tudo bem com o programa OCaml (código de retorno 0)...
        else:
            # Tenta processar a saída JSON (que deve ter os resultados)
            try:
                # Transforma o texto de saída (stdout) num objeto JSON
                output_data = json.loads(process.stdout)
                # Se o JSON diz que teve sucesso (tem "success": true)...
                if output_data.get("success", False):
                    # Vai buscar o dicionário 'results' de dentro do JSON (ou um dicionário vazio se não existir)
                    results = output_data.get("results", {})
                    # Mostra os processos gerados (aleatórios) se existirem, senão mostra os do campo "processes"
                    if "processes_generated" in results:
                        show_generated_processes(results["processes_generated"])
                    elif "processes" in results:
                        show_generated_processes(results["processes"])
                    else:
                        # Limpa a tabela se não houver processos (ex: erro ou simulação sem processos)
                        show_generated_processes([])
                    # Vai buscar o dicionário 'stats' de dentro dos 'results' (ou vazio se não existir)
                    stats = results.get("stats", {})

                    # Põe os valores das estatísticas nos sítios certos na janela
                    avg_wt = stats.get('avg_waiting_time')
                    avg_wt_var.set(f"{avg_wt:.2f}" if isinstance(avg_wt, (int, float)) else "N/A")

                    avg_tt = stats.get('avg_turnaround_time')
                    avg_tt_var.set(f"{avg_tt:.2f}" if isinstance(avg_tt, (int, float)) else "N/A")

                    cpu_util = stats.get('cpu_utilization')
                    cpu_util_var.set(f"{cpu_util:.2f}" if isinstance(cpu_util, (int, float)) else "N/A")

                    throughput = stats.get('throughput')
                    throughput_var.set(f"{throughput:.4f}" if isinstance(throughput, (int, float)) else "N/A")

                    deadlines = stats.get('deadline_misses')
                    deadline_misses_var.set(f"{deadlines}" if isinstance(deadlines, int) else "N/A")

                    # Atualiza a caixa de texto da timeline:
                    timeline_text.config(state=tk.NORMAL)
                    timeline_text.delete('1.0', tk.END)
                    timeline_text.insert(tk.END, results.get("timeline_string", "[Dados da timeline em falta]"))
                    timeline_text.config(state=tk.DISABLED)

                    # Mostra o gráfico de Gantt
                    mostrar_gantt(results.get("timeline_string", ""))

                    # Avisa na barra de estado que terminou.
                    status_var.set("Simulação completa.")
                # Se o programa OCaml não deu erro (código 0), mas o JSON diz que algo correu mal ("success": false)...
                else:
                    # Vai buscar a mensagem de erro dentro do JSON.
                    err_msg = output_data.get("error", "Erro desconhecido reportado pelo OCaml.")
                    # Mostra essa mensagem de erro.
                    messagebox.showerror("Erro da Simulação", err_msg)
                    status_var.set(f"Erro Simulação: {err_msg}") # Mensagem mais curta

            # Se correu bem (código 0) mas a saída não é JSON (muito estranho)...
            except json.JSONDecodeError:
                messagebox.showerror("Erro na Saída", f"Processo OCaml teve sucesso mas a saída não é JSON válido.\n{process.stdout}")
                status_var.set("Erro ao processar a saída do OCaml.")
            # Apanha outros erros que possam acontecer aqui a processar os resultados JSON
            except Exception as e:
                 messagebox.showerror("Erro no GUI", f"Erro ao processar os resultados: {e}")
                 status_var.set("Erro ao processar resultados.")

    # Apanhou o erro de ter demorado demasiado tempo (Timeout > 30s)
    except subprocess.TimeoutExpired:
        messagebox.showerror("Timeout", "A simulação demorou mais de 30 segundos.")
        status_var.set("Erro: Simulação excedeu o tempo limite.")
    # Apanhou o erro de não encontrar o ficheiro do OCaml
    except FileNotFoundError:
        messagebox.showerror("Erro", f"Não encontrei o executável OCaml em:\n{OCAML_PATH}\nVerifique o caminho.")
        status_var.set("Erro: CLI OCaml não encontrado.")
    # Apanha qualquer outro erro mesmo inesperado que aconteça.
    except Exception as e:
        messagebox.showerror("Erro", f"Ocorreu um erro inesperado: {e}")
        status_var.set(f"Erro inesperado: {e}")


# Função para limpar SÓ os resultados da simulação (não a tabela CSV)
def clear_results():
      # Põe 'N/A' em todas as caixinhas de estatísticas.
      avg_wt_var.set("N/A")
      avg_tt_var.set("N/A")
      cpu_util_var.set("N/A")
      throughput_var.set("N/A")
      deadline_misses_var.set("N/A")
      # Limpa a caixa de texto da timeline
      timeline_text.config(state=tk.NORMAL) # Torna editável
      timeline_text.delete('1.0', tk.END)   # Apaga tudo
      timeline_text.config(state=tk.DISABLED) # Torna só de leitura


# Função para mostrar o gráfico de Gantt
def mostrar_gantt(timeline_string):
    """
    Espera uma string da timeline no formato: [P1][P2][P2][P1]...
    Cada bloco representa o processo ativo em cada unidade de tempo.
    """
    import re

    # Extrai os nomes dos processos na ordem
    processos = re.findall(r'\[(.*?)\]', timeline_string)
    if not processos:
        messagebox.showinfo("Gantt", "Não há dados de timeline para mostrar.")
        return

    # Agrupa intervalos contínuos do mesmo processo
    tasks = []
    if processos:
        atual = processos[0]
        inicio = 0
        for t, nome in enumerate(processos[1:], 1):
            if nome != atual:
                tasks.append((atual, inicio, t))
                atual = nome
                inicio = t
        tasks.append((atual, inicio, len(processos)))  # último intervalo

    if not tasks:
        messagebox.showinfo("Gantt", "Não há dados de timeline para mostrar.")
        return

    # Substitui "-" por "CPU IDLE" para visualização
    tasks = [("CPU IDLE" if nome == "-" else nome, inicio, fim) for (nome, inicio, fim) in tasks]

    # Desenha o gráfico de Gantt
    fig, ax = plt.subplots(figsize=(10, 2 + 0.5 * len(set(t[0] for t in tasks))))
    ylabels = []
    yticks = []
    nomes_unicos = sorted(set(t[0] for t in tasks))
    for idx, nome in enumerate(nomes_unicos):
        ylabels.append(nome)
        yticks.append(idx)
        for tsk in tasks:
            if tsk[0] == nome:
                ax.barh(idx, tsk[2] - tsk[1], left=tsk[1], height=0.4, align='center')
                ax.text((tsk[1]+tsk[2])/2, idx, f"{tsk[1]}-{tsk[2]}", va='center', ha='center', color='white', fontsize=8)

    ax.set_yticks(yticks)
    ax.set_yticklabels(ylabels)
    ax.set_xlabel('Tempo')
    ax.set_title('Gráfico de Gantt da Execução dos Processos')
    ax.grid(True, axis='x', linestyle='--', alpha=0.5)
    plt.tight_layout()
    plt.show()


# Função para mostrar os processos gerados na tabela
def show_generated_processes(processes):
    # Limpa a tabela
    for item in process_table.get_children():
        process_table.delete(item)
    # Adiciona cada processo à tabela
    for proc in processes:
        # proc = [id, start_time, burst_time, priority/period]
        process_table.insert('', tk.END, values=proc)


# Função para atualizar o estado do botão 'Executar Simulação'
def update_run_button_state(*args):
    try:
        gen_num = int(gen_num_var.get())
    except (tk.TclError, ValueError):
        gen_num = 0
    if gen_num > 0:
        run_button.config(state=tk.NORMAL)
        # Desativa procurar ficheiro
        for child in input_frame.winfo_children():
            if isinstance(child, ttk.Button) and child['text'] == "Procurar...":
                child.config(state=tk.DISABLED)
    else:
        # Ativa procurar ficheiro
        for child in input_frame.winfo_children():
            if isinstance(child, ttk.Button) and child['text'] == "Procurar...":
                child.config(state=tk.NORMAL)
        if file_path_var.get() and file_path_var.get() != "No file selected":
            run_button.config(state=tk.NORMAL)
        else:
            run_button.config(state=tk.DISABLED)


# --- Configuração da Janela (Interface Gráfica com Tkinter) ---
# Cria a janela principal da aplicação.
root = tk.Tk()
# Define o título que aparece na barra da janela.
root.title("ProbSched GUI")
# Define o tamanho inicial da janela (largura x altura) - Aumentei a largura para a tabela
root.geometry("1200x600") # Ajusta conforme necessário

# Variáveis especiais do Tkinter para guardar informação que vai aparecer na janela
file_path_var = tk.StringVar(value="No file selected")
algo_var = tk.StringVar()
quantum_var = tk.IntVar(value=5)
max_time_var = tk.IntVar(value=100)
status_var = tk.StringVar(value="Pronto.")
# Variáveis para mostrar as estatísticas (começam todas com 'N/A').
avg_wt_var = tk.StringVar(value="N/A")
avg_tt_var = tk.StringVar(value="N/A")
cpu_util_var = tk.StringVar(value="N/A")
throughput_var = tk.StringVar(value="N/A")
deadline_misses_var = tk.StringVar(value="N/A")

# Liga as variáveis ao estado do botão 'Executar Simulação'
gen_num_var = tk.IntVar(value=0)
gen_num_var.trace_add("write", update_run_button_state)
file_path_var.trace_add("write", update_run_button_state)


# --- Organização dos elementos na janela ---

# Cria um 'contentor' principal (Frame) para organizar tudo lá dentro.
main_frame = ttk.Frame(root, padding="10")
main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
# Faz com que a linha 0 e coluna 0 da janela principal (onde está o main_frame)
# cresçam se a janela aumentar de tamanho (weight=1).
root.rowconfigure(0, weight=1)
root.columnconfigure(0, weight=1)

# --- Configuração da Grelha do Main Frame (2 colunas) ---
# Coluna 0: para inputs, botão, outputs e status (peso 1)
# Coluna 1: para a tabela CSV (peso 2 - dar mais espaço à tabela)
main_frame.columnconfigure(0, weight=1)
main_frame.columnconfigure(1, weight=3) # Tabela fica mais larga
main_frame.rowconfigure(2, weight=1) # Permite que a linha dos outputs/tabela cresça


# --- Coluna da Esquerda (Inputs, Run, Outputs) ---

# Frame para agrupar os controlos da esquerda
left_column_frame = ttk.Frame(main_frame)
# Coloca este frame na coluna 0 do main_frame, ocupando toda a altura (rowspan=4)
left_column_frame.grid(row=0, column=0, rowspan=4, sticky=(tk.W, tk.E, tk.N, tk.S), padx=(0, 10))
left_column_frame.rowconfigure(2, weight=1) # Permite que a área de output cresça
left_column_frame.columnconfigure(0, weight=1) # Permite que os widgets estiquem na horizontal


# --- Secção dos Inputs (Configuração) ---
input_frame = ttk.LabelFrame(left_column_frame, text="Configuração", padding="10")
input_frame.grid(row=0, column=0, sticky=(tk.W, tk.E), pady=(0, 5))
input_frame.columnconfigure(1, weight=1) # Coluna do combobox/path estica

# Widgets de input (como antes, mas dentro do left_column_frame e input_frame)
ttk.Label(input_frame, text="Algoritmo:").grid(row=0, column=0, sticky=tk.W, padx=5, pady=2)
algo_combo = ttk.Combobox(input_frame, textvariable=algo_var,
                          values=["fcfs", "sjf", "priority_np", "priority_preemp", "rr", "rm", "edf"],
                          state="readonly")
algo_combo.grid(row=0, column=1, sticky=(tk.W, tk.E), padx=5, pady=2)
algo_combo.current(0)

ttk.Label(input_frame, text="Ficheiro Processos:").grid(row=1, column=0, sticky=tk.W, padx=5, pady=2)
ttk.Label(input_frame, textvariable=file_path_var, relief="sunken", padding=2).grid(row=1, column=1, sticky=(tk.W, tk.E), padx=5, pady=2)
ttk.Button(input_frame, text="Procurar...", command=browse_file).grid(row=1, column=2, sticky=tk.E, padx=5, pady=2)

quantum_label = ttk.Label(input_frame, text="Quantum (RR):")
quantum_label.grid(row=2, column=0, sticky=tk.W, padx=5, pady=2)
quantum_spinbox = ttk.Spinbox(input_frame, from_=1, to=100, textvariable=quantum_var, width=5)
quantum_spinbox.grid(row=2, column=1, sticky=tk.W, padx=5, pady=2)

max_time_label = ttk.Label(input_frame, text="Max Time (RM/EDF):")
max_time_label.grid(row=3, column=0, sticky=tk.W, padx=5, pady=2)
max_time_spinbox = ttk.Spinbox(input_frame, from_=1, to=1000, textvariable=max_time_var, width=5)
max_time_spinbox.grid(row=3, column=1, sticky=tk.W, padx=5, pady=2)

ttk.Label(input_frame, text="Gerar Aleatórios:").grid(row=4, column=0, sticky=tk.W, padx=5, pady=2)
gen_num_spinbox = ttk.Spinbox(input_frame, from_=0, to=1000, textvariable=gen_num_var, width=7)
gen_num_spinbox.grid(row=4, column=1, sticky=tk.W, padx=5, pady=2)

# --- Botão Principal ---
run_button = ttk.Button(left_column_frame, text="Executar Simulação", command=run_simulation, state=tk.DISABLED)
run_button.grid(row=1, column=0, pady=10) # pady=10 dá espaço vertical à volta.

# --- Secção dos Resultados ---
output_frame = ttk.LabelFrame(left_column_frame, text="Resultados Simulação", padding="10")
output_frame.grid(row=2, column=0, sticky=(tk.W, tk.E, tk.N, tk.S)) # Ocupa o resto do espaço vertical (weight=1 na row 2 do left_column_frame)
output_frame.columnconfigure(0, weight=1) # Coluna única estica
output_frame.rowconfigure(2, weight=1) # Linha da timeline estica

# Frame para as estatísticas (dentro do output_frame)
stats_frame = ttk.Frame(output_frame)
stats_frame.grid(row=0, column=0, sticky=(tk.W, tk.E)) # Ocupa a largura.

# Labels das estatísticas (como antes)
ttk.Label(stats_frame, text="Tempo Espera Médio:").grid(row=0, column=0, sticky=tk.W)
ttk.Label(stats_frame, textvariable=avg_wt_var).grid(row=0, column=1, sticky=tk.W)
ttk.Label(stats_frame, text="Tempo Retorno Médio:").grid(row=1, column=0, sticky=tk.W)
ttk.Label(stats_frame, textvariable=avg_tt_var).grid(row=1, column=1, sticky=tk.W)
ttk.Label(stats_frame, text="Utilização CPU (%):").grid(row=0, column=2, sticky=tk.W, padx=10)
ttk.Label(stats_frame, textvariable=cpu_util_var).grid(row=0, column=3, sticky=tk.W)
ttk.Label(stats_frame, text="Throughput:").grid(row=1, column=2, sticky=tk.W, padx=10)
ttk.Label(stats_frame, textvariable=throughput_var).grid(row=1, column=3, sticky=tk.W)
ttk.Label(stats_frame, text="Deadlines Falhados (apenas algoritmos de tempo real):").grid(row=2, column=0, sticky=tk.W)
ttk.Label(stats_frame, textvariable=deadline_misses_var).grid(row=2, column=1, sticky=tk.W)

# Label e Texto da Timeline (dentro do output_frame)
ttk.Label(output_frame, text="Timeline:", font=("TkFixedFont", 10)).grid(row=1, column=0, sticky=(tk.W, tk.E), pady=(5,0))
timeline_text = scrolledtext.ScrolledText(output_frame, wrap=tk.WORD, height=10, state=tk.DISABLED, font=("TkFixedFont", 10))
timeline_text.grid(row=2, column=0, sticky=(tk.W, tk.E, tk.N, tk.S), pady=(0,5))

# --- Barra de Estado (em baixo, na coluna da esquerda) ---
status_bar = ttk.Label(left_column_frame, textvariable=status_var, relief=tk.SUNKEN, anchor=tk.W)
status_bar.grid(row=3, column=0, sticky=(tk.W, tk.E), pady=(5,0)) # Ocupa a largura da coluna esquerda


# --- Coluna da Direita (Tabela CSV) ---

# Cria uma 'caixa' com título para a tabela de processos
table_frame = ttk.LabelFrame(main_frame, text="Processos em tabela", padding="10")
# Coloca esta frame na coluna 1 do main_frame, ocupando as linhas 0 a 2 (mesma altura dos controlos/outputs)
table_frame.grid(row=0, column=1, rowspan=3, sticky=(tk.W, tk.E, tk.N, tk.S), pady=(0, 5)) # rowspan=3 para alinhar com input+button+output
# Configura a grelha interna do table_frame para a tabela e scrollbar esticarem
table_frame.columnconfigure(0, weight=1)
table_frame.rowconfigure(0, weight=1)

# Define as colunas da tabela
# Os IDs das colunas ('id', 'start', etc.) são usados internamente
# O texto em 'heading' é o que aparece ao utilizador
columns = ('id', 'start_time', 'burst_time', 'priority_period')
process_table = ttk.Treeview(table_frame, columns=columns, show='headings') # show='headings' esconde a primeira coluna fantasma

# Define os cabeçalhos e a largura inicial das colunas
process_table.heading('id', text='ID')
process_table.column('id', width=50, anchor=tk.CENTER)
process_table.heading('start_time', text='Chegada')
process_table.column('start_time', width=70, anchor=tk.CENTER)
process_table.heading('burst_time', text='Duração')
process_table.column('burst_time', width=70, anchor=tk.CENTER)
process_table.heading('priority_period', text='Prioridade/Período') # Cabeçalho genérico
process_table.column('priority_period', width=120, anchor=tk.CENTER)

# Adiciona a barra de scroll vertical
scrollbar = ttk.Scrollbar(table_frame, orient=tk.VERTICAL, command=process_table.yview)
process_table.configure(yscroll=scrollbar.set)

# Coloca a tabela e a scrollbar na grelha do table_frame
process_table.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
scrollbar.grid(row=0, column=1, sticky=(tk.N, tk.S))


# --- Pôr a janela a funcionar ---
root.mainloop()