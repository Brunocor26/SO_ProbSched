# SO_ProbSched - Simulador de Escalonamento de Processos

Este projeto foi desenvolvido no âmbito da unidade curricular de **Sistemas Operativos** na Universidade da Beira Interior (UBI). Consiste num simulador robusto de escalonamento de processos, utilizando **OCaml** para a lógica de baixo nível e **Python** para a interface de visualização.

## Funcionalidades
- **Simulação de Escalonamento:** Implementação de algoritmos de escalonamento (incluindo variações com quantum definido).
- **Processamento de Dados:** Leitura e escrita de estados de processos através de ficheiros CSV.
- **Interface Gráfica:** Visualização dos resultados da simulação via script Python.
- **Automação:** Scripts Shell (`.sh`) para execução de múltiplas baterias de testes e simulações.

## Tecnologias Utilizadas
- **OCaml:** Linguagem principal utilizada para a lógica do escalonador, gestão de filas e estados dos processos.
- **Python:** Utilizado para a interface gráfica (`gui_python.py`) e tratamento de dados.
- **Shell Script:** Automação do workflow de simulação.
- **Dune:** Sistema de build para o projeto OCaml.

## Estrutura do Projeto
- `/src` ou `/lib`: Lógica principal do escalonador.
- `/bin`: Pontos de entrada da aplicação.
- `processos.csv`: Ficheiro de entrada com a lista de processos (Arrival Time, Burst Time, etc).
- `simular.sh`: Script para correr a simulação padrão.

## Como Executar
Certifique-se de que tem o `opam` e o `dune` instalados para OCaml, e `python3` para a interface.

1. Compile o projeto:
   ```bash
   dune build
