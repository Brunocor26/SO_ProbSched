val escolha_algoritmo : int -> string
val read_string : string -> string
val read_non_empty_string : string -> string
val parse_int : string -> int option
val read_int : string -> int
val read_int_range : string -> int -> int -> int
val parse_float : string -> float option
val read_float : string -> float
val read_float_range : string -> float -> float -> float
val processa_linha_dados : int -> string -> (int * int * int * int) option
val ler_ficheiro_dados_processos : string -> (int * int * int * int) list
val processa_linha_dados_rt : int -> string -> (int * int * int * int) option
val ler_ficheiro_dados_processos_rt : string -> (int * int * int * int) list
