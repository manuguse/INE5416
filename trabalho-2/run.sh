#!/bin/bash

# verifica se o Scala está instalado no sistema
if ! command -v scala &> /dev/null || ! command -v scalac &> /dev/null; then
    echo "Scala is not installed. Please install it manually or with:"
    echo "  sudo apt update && sudo apt install -y scala"
    exit 1
fi

# verifica a exitência do diretório de saída, se não existir, cria
OUT_DIR="out"
mkdir -p "$OUT_DIR"

# verifica se é necessário compilar os arquivos, se for, compila
COMPILE_NEEDED=false
for src_file in src/*.scala; do
    compiled_file="$OUT_DIR/$(basename "${src_file%.scala}.class")"
    if [[ ! -f "$compiled_file" || "$src_file" -nt "$compiled_file" ]]; then
        COMPILE_NEEDED=true
        break
    fi
done

if [[ "$COMPILE_NEEDED" = true ]]; then
    echo "Compiling Scala source files..."
    scalac -d "$OUT_DIR" src/*.scala
    if [[ $? -ne 0 ]]; then
        echo "Compilation failed."
        exit 1
    fi
else
    echo "No compilation needed. Using existing compiled files."
fi

# vê se o número de argumentos está correto
# if [[ $# -eq 0 ]]; then
#     echo "Usage: ./run <number>"
#     exit 1
# fi

# roda o programa
echo "Rodando..."
scala -cp "$OUT_DIR" Main "$@"
EXIT_CODE=$?

if [[ $EXIT_CODE -ne 0 ]]; then
    echo "Programa encerrado com erro."
else
    echo "Programa encerrado com sucesso."
fi

exit $EXIT_CODE
