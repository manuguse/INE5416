#!/bin/bash

# Verifica se o SWI-Prolog está instalado
if ! command -v swipl &> /dev/null; then
    echo "SWI-Prolog não está instalado. Instale-o com 'sudo apt install swi-prolog'."
    exit 1
fi

# Verifica se o ID do puzzle foi fornecido
if [ -z "$1" ]; then
    echo "Por favor, forneça o ID do puzzle como argumento."
    echo "Uso: $0 <puzzle_id>"
    exit 1
fi

PUZZLE_ID=$1

# Define os nomes dos arquivos
MAIN_FILE="main.pl"
PUZZLES_FILE="puzzles.pl"

# Verifica se os arquivos existem
if [ ! -f "$MAIN_FILE" ]; then
    echo "Arquivo $MAIN_FILE não encontrado."
    exit 1
fi

if [ ! -f "$PUZZLES_FILE" ]; then
    echo "Arquivo $PUZZLES_FILE não encontrado."
    exit 1
fi

# Exibe informações no terminal
echo "Iniciando SWI-Prolog..."
echo "Carregando arquivos: $MAIN_FILE e $PUZZLES_FILE"
echo "Executando resolução do puzzle com ID $PUZZLE_ID"

# Inicia o SWI-Prolog e executa as consultas com saída detalhada
swipl <<EOF
write('Iniciando SWI-Prolog...'), nl, nl,
write('Carregando arquivo: $MAIN_FILE'), nl,
[main].
write('Carregando arquivo: $PUZZLES_FILE'), nl,
consult('$PUZZLES_FILE').
write('Resolvido? '), nl,
kojun($PUZZLE_ID),
halt.
EOF
